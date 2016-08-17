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
blockGroup.spdf.merge <- merge(blockGroup.spdf, overlap.max)
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
