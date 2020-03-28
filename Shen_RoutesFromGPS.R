rm(list=ls(all=TRUE)) # clear memory

#This takes a series of GPS points along a road, snaps them to the nearest road, filters them to remove
# some errors, and creates a line from a transportation network built from OpenStreetMap data. In cases
# Where it is not clear which route it uses, it takes the possible routes from google maps



library(rgdal)      #This is the base package for working with spatial data in R
library(lubridate)  #For coverting the date formats
library(sf)         #A newer spatial data model
library(tidyverse)  
library(dodgr)      #transportation routing
library(googleway)  #Interfacing with the google maps API

latlong <- 4326   #These are codes for particular coordinate systems

osm.file.cache <- "roads2.osm"
# inc.data.storage <- "AllData.RData"
inc.data.storage <- "SingleTest_Houston.RData"
out.file.name <- "singleFinal_Houston.RData"
setwd("E:\\GISWork_2\\Shen_Paths")
load( file = "E:\\GISWork_2\\Shen_Paths\\key.RData") # You can either register for your own, or I can send this file. I can push the actual key
set_key(key = google.key) #sets the key in the googleway package

if (file.exists(inc.data.storage)){ #If the data has been processed file, use this
  print("Opening Cache")
  load(inc.data.storage)
}else{ #Otherwise, process it
  load("a26da9ec19d7be28e90d1ea829b8e34a21246207e9d915e290207b8538274d20.Rdata")
  #Convert the date into a real date
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
  dat <- dodgr_streetnet(pts = st_coordinates(oneperson2), expand = 0.05, quiet = F)
  #Calculating the nearest road to each GPS point
  oneperson2$nearest <- st_nearest_feature(oneperson2,dat) #getting the ID of the closest feature
  # Building the road network model and setting aside an index of vertices
  nav.network <- weight_streetnet(dat, wt_profile = "motorcar")
  verts <- dodgr_vertices (nav.network) #makes a copy of the vertices
  save(dat,nav.network,oneperson, oneperson2, verts, file = inc.data.storage)  
}



#helper function to convert paths to a line
convert.path <- function(path.obj, netwrk = nav.network, verts2 = verts){
  points1 <- verts2 [match (path.obj [[1]] [[1]], verts2$id), ]
  points1 <- points1 %>% #projecting the table as an sf and setting the coordinate system
    sf::st_as_sf(coords = c("x","y")) %>% 
    sf::st_set_crs(latlong) 
  path1 <- points1  %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")
  return(path1)
}

first = TRUE
#Iterating through all of the dates
for(each.date in unique(oneperson2$DayMonthYear)){
# for(each.date in unique(oneperson2$DayMonthYear)[1:2]){
    one.subet <- oneperson2 %>%
    filter(DayMonthYear == each.date,
           Accuracy < 65) # throwing out low accuracy points to see if it has an affect
  one.subet <- one.subet %>% #adding in fields that are the previous and next road ID
    arrange(Timestamp) %>%
    mutate(prevID = lag(nearest, order_by = Timestamp),
           nextID = lead(nearest, order_by = Timestamp))%>%
    filter(prevID != nextID)
  #Move each GPS point to the closest vertex on the road network. This dramatically improves accuracy
  for (x in 1:nrow(one.subet)){
    pts <- st_cast(dat[as.integer(one.subet[x,"nearest"])[1],"geometry"], "POINT")
    clos <- pts[suppressWarnings(st_nearest_feature(one.subet[x,"geometry"],pts)),"geometry"]
    one.subet[x,]$geometry <- clos$geometry
  }#end moving points
  #Iterates through and creates the optimal route either along the same road (same) or to a new road(change)
  for (x in 1:(nrow(one.subet)-1)){
    print(x)
    type <- "same"
    if(one.subet[x,]$nearest != one.subet[x,]$nextID){
      type<- "change"
    }
    dp <- dodgr_paths (nav.network, from = st_coordinates(one.subet[x,]), to = st_coordinates(one.subet[x+1,]), quiet = T)
    if (length(dp$`1`$`1-1`) > 0){
      p1 <- convert.path(dp)
      each.row <- st_sf(Type = type,
                        startDT = one.subet[x,]$Timestamp, 
                        endDT = one.subet[x+1,]$Timestamp, 
                        DMY = each.date, 
                        latOrig =  one.subet[x,]$lat, 
                        lonOrig = one.subet[x,]$lon,  
                        latDest =  one.subet[x+1,]$lat, 
                        lonDest = one.subet[x+1,]$lon,
                        p1$geometry)      
      if(first){
        all.rows <- each.row
        first <- FALSE
      }else{
        all.rows <-rbind(all.rows,each.row)
      }  
    }else{ # if length is 0
      print ("   Failed")
    }
  }
}# end for loop for all dates
all.rows$length <- as.numeric(st_length(all.rows))
all.rows$ord <- 0
all.rows.forgoogle <- all.rows %>% filter(Type == "change" & length > 500)
all.rows.update <- all.rows.forgoogle[0,]
for (x in 1:(nrow(all.rows.forgoogle))){
  # print(each.row)
  orig.coord <- st_coordinates(oneperson2[oneperson2$Timestamp == all.rows.forgoogle[x,]$startDT,])
  dest.cord <-  st_coordinates(oneperson2[oneperson2$Timestamp == all.rows.forgoogle[x,]$endDT,])
  each.row <- all.rows.forgoogle[x,]    
  each.row$Type <- "google"
  
  df <- google_directions(origin =      c(orig.coord[2],orig.coord[1]),
                          destination = c(dest.cord[2],dest.cord[1]),
                          mode = "driving",
                          alternatives = T)
  print(paste(x, nrow(df$routes),sep = ": "))
  if (nrow(df$routes)>1){
    df_routes <- data.frame(polyline = direction_polyline(df)) 
    df_routes$each <- sapply(df_routes$polyline, function(y){
      df_coords <- decode_pl(as.character(y))
      st_linestring(as.matrix(df_coords[,c("lon","lat")]), dim = "XY")
      
    })
    df_routes$geometry <- st_sfc((geom = df_routes$each),crs = latlong)
    each.row <- each.row[rep(seq_len(nrow(each.row)), each=nrow(df$routes)),]
    each.row$p1.geometry <- df_routes$geometry
    each.row$ord <- 1:nrow(df$routes)
  }else{ #if only one route
    pl <- decode_pl(direction_polyline(df))
    each <- st_linestring(as.matrix(pl[,c("lon","lat")]), dim = "XY")
    each.row$p1.geometry <- st_sfc((geom = each),crs = latlong )
    each.row$ord <- 1
  }
  all.rows.update <- rbind(all.rows.update, each.row)
  
}#end loop through all routers
all.rows <- rbind(all.rows,all.rows.update)
all.rows <- arrange(all.rows,startDT)
save(all.rows, oneperson2,file =out.file.name)
#The final data layer has everything from the transportation model with a code of "same" or change"
#Same means it was all along the same road and did not need additional routing
#Change means it moved to a different road between two points
#All changes were then run through the google API if there was more than 500 meters between the roads
#The records with a code of google duplicate those with change in most cases. Though, multiple routes, 
#where they exist, are found in the Google version. I left both in place, but the change records can 
#likely be removed

#These are testing things for loading in a GIS program
write_sf(all.rows, "Single_Out_Houston_4.shp")
write_sf(oneperson2, "single_out_Houston_points4.shp")

# write.csv(try2$snappedPoints,"snappy.csv")

