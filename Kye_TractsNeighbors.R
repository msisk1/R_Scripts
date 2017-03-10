#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("rgdal","sf","rgeos","foreign","spdep") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

setwd("E:\\GISWork_2\\Kye_Tracts_ZipCodes\\2017-03-08_GeocodingToTract")

latlong <- "+init=epsg:4326"
google <- "+init=epsg:3857"
# Five.miles.in.meters <- 8046.72



tracts_poly <- readOGR(dsn = "..", layer = "US_tract_2000", stringsAsFactors = F)
useful.tracts <- read.csv("whgentrlist.csv", stringsAsFactors = F)
tracts_poly$tractid <- paste(as.numeric(tracts_poly$NHGISST)/10, substring(tracts_poly$NHGISCTY,1,3), substring(tracts_poly$GISJOIN,9), sep="")


tracts_poly <- merge(tracts_poly, useful.tracts, by="tractid", all.x= T)
tracts_smaller <- tracts_poly[!is.na(tracts_poly$city),]


# tracts_larger <- gIntersection(tracts_poly,tracts_smaller)
tracts_larger <- readOGR(dsn=".",layer = "US_tract2000_WithUsefulAndNeighbors",stringsAsFactors = F)
tracts_larger$tractid <- paste(as.numeric(tracts_larger$NHGISST)/10, substring(tracts_larger$NHGISCTY,1,3), substring(tracts_larger$GISJOIN,9), sep="")

# points <- readOGR(dsn=".", layer = "Schools_Geocoding_Result", stringsAsFactors = F)
points <-  read.csv("SingeTracts_Fixed.csv", stringsAsFactors = F)
coordinates(points) <- ~X + Y             #Define the coordinates to convert it to a spatial points data frame
proj4string(points) <- CRS(latlong)  
points <- spTransform(points,CRS(proj4string(tracts_larger)))


# a<- over(tracts_larger,points)
points@data$tractid <- over(points, tracts_larger)$tractid  #appending the tractid
points <- points[points$Addr_type %in% c("PointAddress","StreetAddress") ,] #tossing values other than streer or point addresses

schools.in.tract <- aggregate(EPIN ~ tractid, data = points@data, paste, collapse = " ") #Generating list  of schools in each tract
names (schools.in.tract) <- c("tractid","epin_in") #changing names
tots <- data.frame(table(points$tractid)) #genrating number of schools in each tract
names(tots) <- c("tractid","freq_in") #changing names
schools.in.tract <- merge(schools.in.tract,tots,by="tractid") #adding it together


# 
# write.dbf(schools.in.tract, "Schools_in_Tract")
# write.csv(schools.in.tract, "Schools_in_Tract2.csv", row.names = F)
# 
# 
# 



# 
# row.names(tracts_smaller) <- tracts_smaller$tractid
# working <- tracts_larger[1:100,]
# d<-gTouches(working, byid = T)
# 
# 
# writeOGR(obj= tracts_poly, dsn=".",driver="ESRI Shapefile",layer = "US_tract2000_WithUseful")
# 
# 






# 
# working <-tracts_poly@data
# sum(!is.na(working$city))
# 
# 
# working$tractid <- paste(as.numeric(working$NHGISST)/10, substring(working$NHGISCTY,1,3), substring(working$GISJOIN,9), sep="")
# 
# working2 <- merge(useful.tracts, working, by="tractid")
# 
# 
# 



# 
# 
# 
# 
# 
# cents <- coordinates(tracts_poly)
# 
# 
# tracts_point <- SpatialPointsDataFrame(coords=coordinates(tracts_poly), data=tracts_poly@data, proj4string = CRS(proj4string(tracts_poly)))
# 
# tracts_point <- spTransform(tracts_point,CRS(google))
# # tracts_buffer <- gBuffer( tracts_point, width=Five.miles.in.meters)
# tracts_buffer <- readOGR(dsn = "2017-03-08_GeocodingToTract",layer ="2000_Tract_Centroid_5MileBuffer")


#FIXING
load("E:/GISWork_2/Kye_Tracts_ZipCodes/2017-03-08_GeocodingToTract/school9192.rdata")
coded <- read.csv("2017-03-08_GeocodingToTract\\SingleTracts.txt",stringsAsFactors = F)
school$Field1 <- row.names(school)
school2 <- school[,c(8,7)]
coded$EPIN <- NULL
coded2 <- merge(coded,school2,by="Field1",all.x =T)
coded2 <- coded2[,c("Addr_type", "X", "Y", "Field1", "ISR","EINST",  "EADDRS", "ECITY", "ESTABB", "EZIP", "EPIN", "NHGISST", "NHGISCTY" ,"GISJOIN", "GISJOIN2")]
write.csv(coded2,"SingeTracts_Fixed.csv", row.names = F)
