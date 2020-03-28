rm(list=ls())
library("tidyverse")
library("lubridate")
library("googleway")

setwd("E:\\GISWork_2\\Shen_Paths")



load("oneisallyouneed.RData")

oneperson <- person


oneperson.subset <- oneperson%>%
  filter(Timestamp>=1530410351 & Timestamp <= 1530411971)
oneperson <- oneperson.subset



latlong <- 4326   #These are codes for particular coordinate systems

oneperson$date <- as.POSIXct((oneperson$Timestamp+oneperson$Offset),origin = "1970-01-01",tz = "GMT") #Convert the original time stamp to a datetime object
oneperson$DayMonthYear <- paste(year(oneperson$date),month(oneperson$date),day(oneperson$date),sep="-") #convert once more to text for filtering
oneperson <- oneperson %>% #Changing to this way because there are duplicate timestamps that mess up the routing. just choosing one of them is fine
  distinct(Timestamp, .keep_all = TRUE)%>%
  arrange(Timestamp)
oneperson2 <- oneperson #just to keep a copy of the unaltered data
oneperson2$X <- oneperson2$long #copying these so the original coordinates remain in the dataset
oneperson2$Y <- oneperson2$lat
oneperson2 <- oneperson2 %>% #projecting the table as an sf and setting the coordinate system
  sf::st_as_sf(coords = c("X","Y")) %>% 
  sf::st_set_crs(latlong) 


first <- T
for (y in seq(1,nrow(oneperson2),100)){
  ender <- ifelse(test=y+99>nrow(oneperson2),yes = nrow(oneperson2),no = y+99)
  print(paste(y, ender))
  oneperson3 <- oneperson[y:ender,]
  try3<- google_snapToRoads(oneperson3, lat = "lat", lon = "long", interpolate = F)
  try4 <- data.frame(orgIndex = y+try3$snappedPoints$originalIndex, newLon =try3$snappedPoints$location$longitude, newLat = try3$snappedPoints$location$latitude, gplaceID = try3$snappedPoints$placeId )
  if (first){
    all.google.moved <- try4
    first<-F
  }else{
    all.google.moved <- rbind(all.google.moved, try4)
  }
  }
row.names(oneperson2) <- NULL
oneperson2$orgIndex <- row.names(oneperson2)
all.google.moved2 <- merge(oneperson2,all.google.moved, by = "orgIndex", all.x = T)

one.subet <- all.google.moved2 %>% #adding in fields that are the previous and next road ID
    arrange(Timestamp) %>%
    mutate(prevID = lag(gplaceID, order_by = Timestamp),
           nextID = lead(gplaceID, order_by = Timestamp))%>%
    filter(prevID != nextID)  
    
all.rows.update <- one.subet[0,]
  
for (x in 1:(nrow(one.subet)-1)){
  each.row <- st_sf(Type = "google",
                    startDT = one.subet[x,]$Timestamp, 
                    endDT = one.subet[x+1,]$Timestamp, 
                    latOrig =  one.subet[x,]$lat, 
                    lonOrig = one.subet[x,]$lon,  
                    latDest =  one.subet[x+1,]$lat, 
                    lonDest = one.subet[x+1,]$lon,
                    geometry  = one.subet[x,]$geometry) 
  df <- google_directions(origin =      c(one.subet[x,]$newLat,one.subet[x,]$newLon),
                          destination = c(one.subet[x+1,]$newLat,one.subet[x+1,]$newLon),
                          mode = "driving",
                          alternatives = F)
  print(paste(x, nrow(df$routes),sep = ": "))
  if (nrow(df$routes)>1){
    df_routes <- data.frame(polyline = direction_polyline(df)) 
    df_routes$each <- sapply(df_routes$polyline, function(y){
      df_coords <- decode_pl(as.character(y))
      st_linestring(as.matrix(df_coords[,c("lon","lat")]), dim = "XY")
      
    })
    df_routes$p1.geometry <- st_sfc((geom = df_routes$each),crs = latlong)
    each.row <- each.row[rep(seq_len(nrow(each.row)), each=nrow(df$routes)),]
    each.row$geometry <- df_routes$geometry
    each.row$ord <- 1:nrow(df$routes)
  }else{ #if only one route
    pl <- decode_pl(direction_polyline(df))
    each <- st_linestring(as.matrix(pl[,c("lon","lat")]), dim = "XY")
    each.row$geometry <- st_sfc((geom = each),crs = latlong )
    each.row$ord <- 1
  }
  all.rows.update <- rbind(all.rows.update, each.row)
  
}#end loop through all routers
all.rows.update$length <- as.numeric(st_length(all.rows.update))
all.rows <- all.rows.update[all.rows.update$length > 0 &all.rows.update$length < 12000,]

plot(all.rows$geometry)

load(".\\ClimateData\\data.RData")
pm2.5.raster <- rasterFromXYZ(cbind(gr.pred,pm2.5), crs = "+init=epsg:4326")

oop <- st_sf(aa = "aa", st_cast(st_combine(all.rows),"LINESTRING"))
oop <- st_crop(all.rows, y = st_bbox(pm2.5.raster))
oop <- all.rows%>%
  group_by(Type)%>%st_union()%>%
  st_sf()
plot(oop) 


groot <- gLineMerge(as(all.rows,"Spatial"))
values.from.google <- extract(x=pm2.5.raster, y=all.rows,cellnumbers=T, along = T, df = T)


names(values.from.google) <- c("lineID","CellID","val")


# try.rev <- sapply(values.from.google, rev)
all.ordered.cells.values <- values.from.google %>%
 filter(CellID != lead(CellID,n=1))

all.ordered.cells.values$X <- xyFromCell(object = pm2.5.raster, cell = all.ordered.cells.values$CellID)[,1]
all.ordered.cells.values$Y <- xyFromCell(object = pm2.5.raster, cell = all.ordered.cells.values$CellID)[,2]

write.csv(all.ordered.cells.values)

try.df <- data.frame(values.from.google)
aa<-unique(try.df)
# try.df$order<-as.integer(row.names(try.df))
# ggplot(try.df)+
  # geom_line(aes(x=order,y=vals))

aa<-aa%>%
  mutate(order = row_number())


map.line <- SpatialLinesDataFrame(sl = groot, data = data.frame(each = "one"))
all.google.moved2$colorer <- "GPS Points"
all.google.moved2[1,"colorer"] <- "Starting Point"




distance.plot <- ggplot(aa)+
  geom_line(aes(x=order,y=pm2.5),size = 1.5)+
  geom_point(data = aa[1,], aes(x=order, y=pm2.5), size = 3)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        legend.title = element_blank()) +
  theme(legend.position="bottom") +
  xlab("Distance") +
  ylab("pm2.5")+ 
  coord_fixed(ratio=6)
distance.plot



# all.google.moved2$date <- all.google.moved2$date - all.google.moved2$Offset 
all.google.moved2$pm.2.5 <- extract(x=pm2.5.raster, all.google.moved2, df = F)
#smoothing the time plot
spline.d <- as.data.frame(spline(all.google.moved2$date, all.google.moved2$pm.2.5))
spline.d$date <- as.POSIXct((spline.d$x),origin = "1970-01-01",tz = "GMT") #Convert the original time stamp to a datetime object



time.plot <- ggplot(all.google.moved2, aes(x = date, y = pm.2.5))+
  geom_line(data = spline.d, aes(x=date,y=y),size = 1)+
  geom_point(aes(color = colorer, size = colorer))+
  geom_point(data = all.google.moved2[all.google.moved2$Timestamp == 1530487926,], aes(x = date, y = pm.2.5), shape = 17, size = 4, color = "cyan")+
  scale_color_manual(values =  c("blue","cyan")) +
  scale_size_manual(values =  c(.2,4)) +
  theme_bw()+
  theme(legend.title = element_blank()) +
  theme(legend.position="none") +
  xlab("Time") +
  ylab("pm2.5")
time.plot

cent <- c((st_bbox(oop)$xmax + st_bbox(oop)$xmin)/2, (st_bbox(oop)$ymax + st_bbox(oop)$ymin)/2)
mapt.stamen <- qmap(cent, zoom = 11, maptype = "toner-lite", source = "stamen", filename = "hop.png")
crud <- all.rows %>% as("Spatial") %>% fortify()

map.plot <- mapt.stamen +
  geom_path(aes(x = long, y = lat), col = "black", data = crud, size = 2)+
  # geom_point(aes(x = all.rows[1,]$lonOrig, y = all.rows[1,]$latOrig, shape = "Starting point"), size = 4)+
  geom_point(data = all.google.moved2, aes(x=newLon, y = newLat, color = colorer, size = colorer))+
  geom_point(data = all.google.moved2[all.google.moved2$Timestamp == 1530487926,], aes(x = newLon, y = newLat, shape = "Ending Point"), size = 4, color = "cyan")+
  scale_shape_manual(values = 17)+
  scale_color_manual(values =  c("blue","cyan")) +
  scale_size_manual(values =  c(.2,4)) +
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")+
  guides(alpha = guide_legend(order = 4),
         color = guide_legend(order = 1),
         size = guide_legend(order = 1))
map.plot

sing.leg <- get_legend(map.plot)
map.plot <- map.plot + theme(legend.position = "none")
# map.plot+ inset_raster(as.raster(pm2.5.raster), xmin = pm2.5.raster@extent[1], xmax = pm2.5.raster@extent[2],ymin =pm2.5.raster@extent[3], ymax = pm2.5.raster@extent[4])+ coord_cartesian()
# map.plot + geom_raster(pm.2.5.df, mapping = aes(x = x, y = y, fill = pm2.5), alpha = .5) +coord_cartesian()

library(cowplot)
# grid.arrange(distance.plot,map.plot, ncol=2, heights = 1:2, widths= 1:2)

plot_grid(map.plot,time.plot, labels = c("A","B"),label_size = 12, align = "V",sing.leg, rel_heights = c(1,.1))


# plot_gg(oop, width = 3.5, raytrace = FALSE, preview = TRUE)
# plot_gg(oop, width = 3.5, multicore = TRUE, windowsize = c(800, 800), 
#         zoom = 0.85, phi = 35, theta = 30, sunangle = 225, soliddepth = -100)

values.from.google <- extract(x=pm2.5.raster, y=all.rows,cellnumbers=T, along = T, df = T)


crud$pm.2.5 <- extract(x = pm2.5.raster, crud)

#WORKING VERSION OF THE 3D
height <- max(all.google.moved2$newLat, na.rm = T) - min(all.google.moved2$newLat, na.rm = T)
width <- max(all.google.moved2$newLon, na.rm = T) - min(all.google.moved2$newLon, na.rm = T)
borders <- c(bottom  = min(all.google.moved2$newLat, na.rm = T)  - .2 * height, 
             top     = max(all.google.moved2$newLat, na.rm = T)  + .1 * height,
             left    = min(all.google.moved2$newLon, na.rm = T) - 0.3 * width,
             right   = max(all.google.moved2$newLon, na.rm = T) + 0.1 * width)



map <- get_stamenmap(borders, zoom = 10, maptype = "toner-lite")
map.low <- get_stamenmap(borders, zoom = 8, maptype = "toner-lite")


#totally a hack
gap.filler <- read.csv("Export_Output_3.csv", stringsAsFactors = F)
gap.filler <- gap.filler[,c(4,3,5)]
names(gap.filler) <- c("newLon","newLat","pm.2.5")
gap.filler$X <- gap.filler$newLon
gap.filler$Y <- gap.filler$newLat
gap.filler <- gap.filler %>% #projecting the table as an sf and setting the coordinate system
  sf::st_as_sf(coords = c("X","Y")) %>% 
  sf::st_set_crs(latlong) 


gaps.filled <- all.google.moved2[,c("newLon","newLat","pm.2.5", "geometry")]
gaps.filled <- rbind(gaps.filled,gap.filler)

ascot <- ggmap(map.low)+
  # geom_point(data = all.ordered.cells.values, aes(x=X, y = Y, color = val, fill = val), alpha = 1)+
  # geom_path(aes(x = long, y = lat), col = "black", data = crud, size = 2)+
  geom_point(aes(x = all.rows[1,]$lonOrig, y = all.rows[1,]$latOrig), size = 4, color = "cyan")+
  geom_point(aes(x = all.rows[nrow(all.rows),]$lonDest, y = all.rows[nrow(all.rows),]$latDest), color = "cyan", shape = 17, size = 6)+
  geom_point(data = gaps.filled, aes(x=newLon, y = newLat, color = pm.2.5, fill = pm.2.5), alpha =1)+
  
  scale_color_gradient(low = "blue", high = "red")+
  scale_fill_gradient(low = "blue", high = "red") +
  guides(color = guide_colorbar(title.position = "bottom"))+
  labs(x = "",y = "")
ascot
library(gginnards)
plot_gg(list(ascot,delete_layers(ascot,idx = c(2,4,5))), multicore = T, fov = 0, height_aes = "color", raytrace = F, pointcontract = 0.7)
# render_label(ascot, x = -95, y = 29.7, z = 40, zscale = 50,
#              text = "Moss Landing", textsize = 2, linewidth = 5)

render_snapshot()

library(cowplot)
# grid.arrange(distance.plot,map.plot, ncol=2, heights = 1:2, widths= 1:2)

plot_grid(ascot,time.plot, labels = c("A","B"),label_size = 12, align = "V",sing.leg, rel_heights = c(1,.1))




