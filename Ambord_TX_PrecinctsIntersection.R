#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("rgdal","sp") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("E:\\GISWork_2\\Ambord_TX_Voting") 

#The intersection file was created in ArcGIS for ease
intersection.bg.spdf <-  readOGR(".", layer = "BlockGroups_VotingPrecintsINTERSECTION")


overlap.bg.data <- intersection.bg.spdf@data
overlap.bg.data <- overlap.bg.data[,c(9,11,13)]
overlap.bg.agg <- aggregate(Shape_Area ~ bgkey, overlap.bg.data, max)
overlap.bg.max <- merge(overlap.bg.data,overlap.bg.agg)
overlap.bg.max <- overlap.bg.max[,c("bgkey","PRECINCT_1")]

#This was done by finding those block groups that overlapped the precincts and manually deleting a few that were just because of edges overlapping slightly.
blockGroup.spdf <- readOGR(".",layer = "Harris_BlockGroups")
blockGroup.spdf.merge <- merge(blockGroup.spdf, overlap.bg.max)
blockGroup.spdf.merge@data$GEO_FIPS <- paste(blockGroup.spdf.merge@data$STATE,blockGroup.spdf.merge@data$CTBGKEY,sep="")

writeOGR(obj=blockGroup.spdf.merge, dsn=".", layer="BlockGroupsWithPrecint2", driver="ESRI Shapefile",overwrite_layer=TRUE) 

BG.census.data <- read.csv("BG_Census_mod.csv",stringsAsFactors = F)
write.csv(blockGroup.spdf.merge@data,"BlockGroupsWithPrecintID.csv")


#Tracts instersection
intersection.tracts.spdf <-  readOGR(".", layer = "Tracts_VotingPrecintsINTERSECTION")


overlap.tracts.data <- intersection.tracts.spdf@data
overlap.tracts.data <- overlap.tracts.data[,c(3,5,7)]
overlap.tracts.agg <- aggregate(Shape_Area ~ CTTRTKEY, overlap.tracts.data, max)
overlap.tracts.max <- merge(overlap.tracts.data,overlap.tracts.agg)
overlap.tracts.max <- overlap.tracts.max[,c("CTTRTKEY","PRECINCT_1")]
tracts.spdf <- readOGR(".",layer = "Harris_Tracts")
tracts.spdf.merge <- merge(tracts.spdf, overlap.tracts.max)

writeOGR(obj=tracts.spdf.merge, dsn=".", layer="TractsWithPrecint", driver="ESRI Shapefile",overwrite_layer=TRUE) 


#11/9/16: More intersections of voting precincts
library(rgdal)
library(sp)
setwd("E:/GISWork_2/Archive/Ambord_TX_Voting/FilesToSampleForPrecincts")
latlong <- "+init=epsg:4326" #This is the espg code for the WGS 1984 geographic projection

# spatial data (political districts of Austria)
precincts <- readOGR(dsn = ".", layer = "HarrisCounty_VoterPrecincts_1069")
precincts.latlong <- spTransform(precincts, CRS(latlong))
# view

# some addresses
points <- read.csv("Paige's List-MatchedFile.csv", stringsAsFactors = F)
points$ID <- row.names(points)

# make pts spatial
working.points <-  (points[,c("ID","GE_LATITUDE_2010","GE_LONGITUDE_2010")])
working.points <- working.points[complete.cases(working.points),]

coordinates(working.points) <- ~GE_LONGITUDE_2010 + GE_LATITUDE_2010             #Define the coordinates to convert it to a spatial points data frame
proj4string(working.points) <- CRS(latlong)           #Define the projection using the CRS command to convert the string with the EPSG code

plot(precincts.latlong)
plot(working.points, add=T)



over.table <- over(working.points, precincts.latlong)
over.table <- 
working.points.table <- working.points@data


hio <- cbind(working.points, a) # I think this needs more work

a1<-read.csv("LGBT Business LatLong.txt")
a2<-read.csv("LGBT Business LatLong_second.txt")
identical(a1,a2)
