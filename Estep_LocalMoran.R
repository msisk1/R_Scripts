#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("rgdal","spdep","maptools","rgeos") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("C:\\Temp\\New") # SET YOUR WORKING DIRECTORY HERE!!!


in.tracts <- readOGR(".", layer = "IN_CensusTracts")
in.data <- in.tracts@data
in.data$AA_per <- in.data$AfrAm / in.data$Population
in.data2 <- in.data[,c("GEOID","AA_per")]

mergeed.spdf <- merge(in.tracts,in.data, by = "GEOID", all=T)

tracts.nd <-poly2nb(in.tracts)
tracts.listw <- nb2listw(tracts.nd, style = "W")

ca.prec <- readOGR(".", layer = "CA_2012_precinct_shapefile_with_voting_data")



la.prec <- readOGR(".", layer = "LA_Precints")

la.prec.single <- disaggregate(la.prec)
count.table<- data.frame(table(la.prec.single@data$SRPREC_KEY))
colnames(count.table) = c("SRPREC_KEY","Count")
la.prec.with.count <- merge(la.prec,count.table, by="SRPREC_KEY", all.x = T)
#writeOGR(la.prec.with.count, dsn="." ,layer="LA_Precints_withCount2",driver="ESRI Shapefile")


la.prec.single.nb <-poly2nb(la.prec.single)
la.prec.single.list <- nb2listw(la.prec.single.nb, style = "W", zero.policy = T)
locm <- localmoran(la.prec.single$p_obama_20, la.prec.single.list, alternative="two.sided")

num_neig <- data.frame(card(la.prec.single.nb))


library(maptools)
la.prec.single2<- spCbind(la.prec.single,num_neig)
writeOGR(la.prec.single2, dsn="." ,layer="LA_PrecintsSing_withneighbors",driver="ESRI Shapefile")



la.prec.single2 <- merge(la.prec.single,just.neighborcounts, by="SRPREC_KEY", all.x = T)


#Calculating areas
library("rgeos")

#areas <-sapply(slot(la.prec.single, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
aa <- data.frame(gArea(la.prec.single, byid = TRUE))
colnames(aa) <- c("area")
la.prec.single.area<- spCbind(la.prec.single,aa)
writeOGR(la.prec.single.area, dsn="." ,layer="LA_PrecintsSing_area3",driver="ESRI Shapefile")
write.csv(la.prec.single.area, "crap.csv")
