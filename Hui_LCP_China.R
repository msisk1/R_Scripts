# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory

#Todo:
# Get the polylines all in one
# Make it so changes to the factor for the route names does not change the order
# 

packages<- c("rgdal","raster","gdistance","rgeos","sp","maptools") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first


run.at.low.res <- T
correct.format.check <- T


setwd("E:/GISWork_2/Hui_China") #WIndows maching
# setwd("/home/matthew/Documents/HUI") #Linux machine

# exten <- "2018-04-02"
# out.path <- paste("paths_",exten,sep="")
# 
# in.files <- list.files(path = out.path, pattern = ".csv$")
# in.file <- in.files[2]

#Functions
scaleBB <- function(box){
  # box <- bbox(site.points[1:2,])
  # box <- bbox(site.points[x:x+1,])
  #renamed bbox fields? This needed to be switched to indices
  # ew <- (box["E","max"] -box["E","min"]) / 4
  # ns <- (box["N","max"] -box["N","min"]) / 4
  ew <- (box[1,1] -box[1,2]) / 4
  ns <- (box[2,1] -box[2,2]) / 4
  if (ns < 1000){
    ns <- ew
  }
  # M <- matrix(c(-ew,-ns,ew,ns), ncol = 2)
  M <- matrix(c(-ew,-ns,ew,ns), ncol = 2)
  
    return(box - M)
}
altDiff <- function(x){x[2] - x[1]}
calculateConductance <- function(in.raster){
  hd <- transition(in.raster, altDiff, 8, symm=FALSE)
  slope <- geoCorrection(hd)
  # plot(dem.low)
  adj <- adjacent(in.raster, cells=1:ncell(in.raster), pairs=TRUE, directions=8)
  speed <- slope
  speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
  cond <- geoCorrection(speed)  
  return(cond)
}



#Variables
latlong <- "+init=epsg:4326" #This is the espg code for the WGS 1984 geographic projection
google <- "+init=epsg:3857"  #This is the espg code for the web mercator projection used by google earth

#open the raster 
if (!file.exists("chgis_dem_processed.tif")){
  dem <- raster("chgis_dem.tif")   #load the raster: Make sure the auxillary files are present
  library(rworldmap)
  world.map <- getMap()
  # proj4string(world.map)
  dem <- projectRaster(dem, crs = google)
  
  asia.map <- world.map[world.map@data$continent == "Eurasia"& !is.na(world.map@data$continent),]
  asia.map <- spTransform(asia.map, crs(proj4string(dem)))
  dem <- mask(dem, asia.map)
  dem[dem == -32768] <- NA #Filtering out NA Values
  writeRaster(dem,"chgis_dem_processed.tif")
  
}else{
  dem <- raster("chgis_dem.tif")   #load the raster: Make sure the auxillary files are present  
  dem[dem == -32768] <- NA #Filtering out NA Values
}
if (!file.exists("chgis_dem_low.tif")){
  print("no.file")
  dem.low <-aggregate(dem, fact =  5)
  writeRaster(dem.low,"chgis_dem_low.tif")
}else{
  dem.low <- raster("chgis_dem_low.tif")
}

proj4string(dem) #verify that this is non-NA

# dem[is.na(dem)] <- 100000

if (run.at.low.res) {
  dem <- dem.low
}

date.string <- "2020-03-28"
file.names <- list.files(path = paste0("paths_",date.string, "/"), pattern = "*.csv$")
file.string <- tools::file_path_sans_ext(file.names[5])

raw.file <- paste("paths_",date.string, "/",file.string, ".csv", sep = "")
#new data processing section
master.file <- read.csv(raw.file, stringsAsFactors = F, encoding = "UTF-8")
master.file$N <- as.numeric(master.file$N)
master.file$N.1 <- as.numeric(master.file$N.1)

master.file <- master.file %>%
  filter(!is.na(N),
         !is.na(N.1))
#ADDING OPTIONAL CHECK FOR CORRECT DATA
if (correct.format.check){
  library(tidyverse)
  templat <- if_else(master.file$N > 70, true = master.file$E,false = master.file$N)
  templon <- if_else(master.file$N > 70, true = master.file$N,false = master.file$E)  
  master.file$N <- templat
  master.file$E <- templon
  
  templat <- if_else(master.file$N.1 > 70, true = master.file$E.1,false = master.file$N.1)
  templon <- if_else(master.file$N.1 > 70, true = master.file$N.1,false = master.file$E.1)  
  master.file$N.1 <- templat
  master.file$E.1 <- templon
  
}

x<- 1
subset.file <- master.file[master.file$Identification.Number ==1,]
first <- T
# for (each.id in unique(master.file$Identification.Number)){
for (each.id in row.names(master.file)){

  subset.file <- master.file[row.names(master.file) ==each.id,]
  print(paste(each.id,x,sep=": "))
  x <- x+1
  subset.origins <- subset.file
  coordinates(subset.origins) <- ~E + N             #Define the coordinates to convert it to a spatial points data frame
  proj4string(subset.origins) <- CRS(latlong)
  subset.origins <- spTransform(subset.origins, CRS(proj4string(dem)))
  # 
  # 
  subset.dest <- subset.file
  coordinates(subset.dest) <- ~E.1 + N.1             #Define the coordinates to convert it to a spatial points data frame
  proj4string(subset.dest) <- CRS(latlong)
  subset.dest <- spTransform(subset.dest, CRS(proj4string(dem)))
  subset.all <- subset.dest + subset.origins
  q <- bbox(subset.all)
  q[,"max"] <- q[,"max"] + res(dem)
  q[,"min"] <- q[,"min"] - res(dem)
  clip_dem <- crop(dem,scaleBB(q))
  #crate concordance for the clip
  Conductance <- calculateConductance(clip_dem)

  #shortest
  each.line <- shortestPath(Conductance,origin = subset.origins@coords[1,],goal = subset.dest@coords[1,] , output="SpatialLines")
  each.sldf <- SpatialLinesDataFrame(sl = each.line, data = subset.file, match.ID = F)
  # row.names(each.sldf) <- row.names(subset.file)
  if (first){
    all <- each.sldf
    first <- F
  }else{
    all <-all + each.sldf
  }
}

out.file.base <- paste(file.string, "_",date.string, sep = "")
writeOGR(dsn=".", layer = paste(out.file.base, ".shp", sep = ""),obj=all, driver = "ESRI Shapefile" )
plot(all)
save(all,file=paste(out.file.base,".rData", sep=""))


# 
# #create points out of each line (old way was line x to line x+1)
# subset.origins <- subset.file
# coordinates(subset.origins) <- ~E + N             #Define the coordinates to convert it to a spatial points data frame
# proj4string(subset.origins) <- CRS(latlong)  
# subset.origins <- spTransform(subset.origins, CRS(proj4string(dem)))
# 
# 
# subset.dest <- subset.file
# coordinates(subset.dest) <- ~E.1 + N.1             #Define the coordinates to convert it to a spatial points data frame
# proj4string(subset.dest) <- CRS(latlong)  
# subset.dest <- spTransform(subset.dest, CRS(proj4string(dem)))
# 
# 
# #bounding box clip
# # library(sp)
# subset.all <- subset.dest + subset.origins
# q <- bbox(subset.all)
# q[,"max"] <- q[,"max"] + res(dem)
# q[,"min"] <- q[,"min"] - res(dem)
# clip_dem <- crop(dem,scaleBB(q))
# #crate concordance for the clip
# Conductance <- calculateConductance(clip_dem)
# 
# #shortest
# each.line <- shortestPath(Conductance,origin = subset.origins@coords[1,],goal = subset.dest@coords[1,] , output="SpatialLines")
# 
# 
# 
# 
# 
# for (in.file in in.files){
#   print(in.file)
#   temp.folder <- file.path(out.path, tools::file_path_sans_ext(in.file))
#   if (!dir.exists(temp.folder)){
#     dir.create(temp.folder)
#   }
# 
#   #Open the list of points
#   site.points.all <- read.csv(paste(out.path,in.file,sep="/"))  #Read the table with Lat and lon
#   # site.points.table <- site.points
#   coordinates(site.points.all) <- ~E + N             #Define the coordinates to convert it to a spatial points data frame
#   proj4string(site.points.all) <- CRS(latlong)  
#   site.points.all <- spTransform(site.points.all, CRS(proj4string(dem)))
#   (proj4string(site.points.all) == proj4string(dem))
#   
#   
#   
#   all.path.descriptions <- unique(site.points.all$Desc)
#   plot(dem.low)
#   first.file <- TRUE
#   for (each.path in all.path.descriptions){
#     print(each.path)
#     # out.name <- paste("Route_",as.numeric(all.path.descriptions[all.path.descriptions==each.path]),sep="")
#     out.name <- paste("Route_",strsplit(as.character(each.path),split = " ")[[1]][2],sep="")
#     
#     # out.name2 <- paste("RouteSimp_",as.numeric(all.path.descriptions[all.path.descriptions==each.path]),sep="")
#     site.points <- site.points.all[(site.points.all$Desc == each.path),]
#     if (file.exists(paste(temp.folder,"/",out.name,".shp",sep=""))){
#       print("ouput already exists")
#       all.df <- readOGR(dsn=temp.folder, layer = out.name)
#       #next()
#     }else{
#       # print(site.points)
#       first <- T
#       for (x in 1:(nrow(site.points)-1)){
#         print(paste(x,x+1))
#         q <- bbox(site.points[x:(x+1),])
#         q[,"max"] <- q[,"max"] + res(dem)
#         q[,"min"] <- q[,"min"] - res(dem)
#         clip_dem <- crop(dem,scaleBB(q))
#         Conductance <- calculateConductance(clip_dem)
#         each.line <- shortestPath(Conductance, site.points@coords[x,], site.points@coords[x+1,], output="SpatialLines")
#         if (first){
#           all <- each.line
#           first <- FALSE
#         }else{
#           all <- all + each.line
#         }#end else
#       }# end interior for loop
#       all <- gLineMerge(all)
#       # all2 <- gSimplify(all, 500, topologyPreserve=FALSE)
#       
#       all.df <- SpatialLinesDataFrame(all, data.frame(Name = each.path))
#       # all.df2 <- SpatialLinesDataFrame(all2, data.frame(Name = each.path))
#       # all.df <- SpatialLinesDataFrame(all, data.frame(ID = c(1:length(all))))
#       writeOGR(obj = all.df, dsn= temp.folder, layer = out.name, driver="ESRI Shapefile",overwrite_layer=T) #writes the spatial points data frame as a shapefile
#       # writeOGR(obj = all.df2, dsn= out.path, layer = out.name2, driver="ESRI Shapefile",overwrite_layer=T) #writes the spatial points data frame as a shapefile
#       
#     }#end else if file exists
#     plot(site.points, add=T)
#     plot(all.df, add=TRUE)
#     if (first.file){
#       all.in.one <-all.df
#       first.file <- FALSE
#     }else{
#       all.in.one <- rbind(all.in.one,all.df)
#     }
#     
#   }#end exterior for loop
#   writeOGR(dsn=out.path, obj = all.in.one, layer = paste(tools::file_path_sans_ext(in.file),"_AllPaths",sep=""),driver = "ESRI Shapefile" ,overwrite_layer = T)
#   
#     
# }# end of the loop through files
# 
# 
# 
# 
# 
# 
# #Adding a china boundry
# china.outline<-getData("GADM",country="China",level=0)
# # writeOGR(obj=china.outline,dsn=".",layer="china_outline",driver="ESRI Shapefile",overwrite_layer=T)
# plot(spTransform(china.outline, CRS(proj4string(dem))), col = "transparent", add=T)
# 
# 
# 
# 
# each.sldf <- spTransform(all.in.one, CRS((latlong)))
# each.pointsDF <- spTransform(site.points.all, CRS((latlong)))
# pal <- colorFactor(c("navy", "red","orange","cyan","yellow","blue","black"), domain = unique(each.sldf$Name))
# 
# 
# leaflet() %>%
#   # fitBounds(-129,24.2,-65.58,50.54)%>%
#   addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(minZoom = 1, maxZoom = 13, noWrap = F)) %>%
#   addPolylines(data = each.sldf, popup = ~Name, color = ~pal(Name),opacity = 1) %>%
#   addCircleMarkers(data = each.pointsDF, popup = ~Location, color = ~pal(Desc), radius = 5)%>%
#   addLegend("bottomleft", pal = pal, values = each.sldf$Name,
#             title = "each title",
#             opacity = 1
#   )
#   
