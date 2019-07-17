# rm(list=ls(all=TRUE)) # clear memory


library(rgdal)      #This is the base package for working with spatial data in R
library(lubridate)  #For coverting the date formats
library(sf)         #A newer spatial data model
library(tidyverse)  
library(dodgr)

latlong <- "+init=epsg:4326"   #These are codes for particular coordinate systems
google <- "+init=epsg:3857"
osm.file.cache <- "roads2.osm"


setwd("E:\\GISWork_2\\Shen_Paths")

load("a26da9ec19d7be28e90d1ea829b8e34a21246207e9d915e290207b8538274d20.Rdata")

#Convert the date into a real date
oneperson$date <- as.POSIXct(oneperson$Timestamp,origin = "1970-01-01",tz = "GMT") #Convert the original time stamp to a datetime object
oneperson$DayMonthYear <- paste(year(oneperson$date),month(oneperson$date),day(oneperson$date),sep="-") #convert once more to text for filtering
# oneperson <- unique(oneperson)
oneperson <- oneperson %>% #Chanmging to this way because there are duplicate timestamps that mess up the routing. just choosing one of them is fine
  distinct(Timestamp, .keep_all = TRUE)


oneperson2 <- oneperson
oneperson2$X <- oneperson2$long
oneperson2$Y <- oneperson2$lat
oneperson2 <- oneperson2 %>% #projecting the table as an sf and setting the coordinate system
  sf::st_as_sf(coords = c("X","Y")) %>% 
  sf::st_set_crs(4326) 
# coordinates(oneperson2) <- ~X * Y #Converting the original data frame into a spatial one
# proj4string(oneperson2) <- CRS("+init=epsg:4326") #This is the espg code for the WGS 1984 geographic projection
# write_sf(oneperson2,"oneperson2a.shp")

#Building transportation model

#Downloading the roads network: TODO: Make this cache 
dat <- dodgr_streetnet(pts = st_coordinates(oneperson2), expand = 0.05, quiet = F)

#Calculating the nearest road to each GPS point
oneperson2$nearest <- st_nearest_feature(oneperson2,dat) #getting the ID of the closest feature


# Building the road network model and setting aside an index of vertices
nav.network <- weight_streetnet(dat, wt_profile = "motorcar")
verts <- dodgr_vertices (nav.network)


#helper function to convery paths
convert.path <- function(path.obj, netwrk = nav.network, verts2 = verts){
  points1 <- verts2 [match (path.obj [[1]] [[1]], verts2$id), ]
  points1 <- points1 %>% #projecting the table as an sf and setting the coordinate system
    sf::st_as_sf(coords = c("x","y")) %>% 
    sf::st_set_crs(4326) 
  path1 <- points1  %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")
  return(path1)
}



#Building a single subset for testing
one.subet <- oneperson2 %>%
  filter(DayMonthYear == "2018-1-15",
         Accuracy < 65) # throwing out low accuracy points to see if it has an affect


#This throws out all the cases where one point has the same road before and after it. 
#   Should take care of cases where a GPS point has jumped from one road to another as well as cases where the gps stays on the same road for multiple points
one.subet <- one.subet %>%
  arrange(Timestamp) %>%
  mutate(prevID = lag(nearest, order_by = Timestamp),
         nextID = lead(nearest, order_by = Timestamp))%>%
  filter(prevID != nextID)

# write_sf(one.subet, "onesubet.shp")


one.subset.moved <- one.subet
#Move each GPS point to the closest vertex on the road network. This dramatically improves accuracy
for (x in 1:nrow(one.subset.moved)){
  pts <- st_cast(dat[as.integer(one.subset.moved[x,"nearest"])[1],"geometry"], "POINT")
  clos <- pts[st_nearest_feature(one.subset.moved[x,"geometry"],pts),"geometry"]
  one.subset.moved[x,]$geometry <- clos$geometry
}


first = TRUE
one.subet <- one.subset.moved
for (x in 1:(nrow(one.subet)-1)){
  print(x)
  dp <- dodgr_paths (nav.network, from = st_coordinates(one.subet[x,]), to = st_coordinates(one.subet[x+1,]), quiet = T)
  if (length(dp$`1`$`1-1`) > 0){
    p1 <- convert.path(dp)
    each.row <- st_sf(startDT = one.subet[x,]$Timestamp, endDT = one.subet[x+1,]$Timestamp,p1$geometry)
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

write_sf(all.rows, "FirstRun_car_All_moved.shp")


