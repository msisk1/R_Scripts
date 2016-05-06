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

ca.prec <- readOGR(".", layer = "CA_2012_precinct_shapefile_with_voting_data")
ca.prec.repro <- readOGR(".", layer = "CA_2012_EqualArea_Conic")



la.prec <- readOGR(".", layer = "LA_Precints")

la.prec.single <- disaggregate(la.prec)
#count.table<- data.frame(table(la.prec.single@data$SRPREC_KEY))
#colnames(count.table) = c("SRPREC_KEY","Count")
#la.prec.with.count <- merge(la.prec,count.table, by="SRPREC_KEY", all.x = T)
#writeOGR(la.prec.with.count, dsn="." ,layer="LA_Precints_withCount2",driver="ESRI Shapefile")


#Analysis
la.prec.single.nb <-poly2nb(la.prec)
la.prec.single.list <- nb2listw(la.prec.single.nb, style = "W", zero.policy = T)
locm <- localmoran(la.prec.single$p_obama_20, la.prec.single.list, alternative="two.sided")

num_neig <- data.frame(card(la.prec.single.nb))


library(maptools)
la.prec.single2<- spCbind(la.prec,num_neig)
writeOGR(la.prec.single2, dsn="." ,layer="LA_PrecintsSing_withneighbors",driver="ESRI Shapefile")



la.prec.single2 <- merge(la.prec.single,just.neighborcounts, by="SRPREC_KEY", all.x = T)


#Calculating areas

ca.prec <- readOGR(".", layer = "CA_2012_precinct_shapefile_with_voting_data")
CA_Albers <- "+init=epsg:3310"   #This is just a code for the projection we are moving the data into
ca.prec.albers <- spTransform(ca.prec, CRS(CA_Albers)) #This does the reprojection allowing us to calculate area.
ca.prec.albers.diss <- disaggregate(ca.prec.albers)
ca.prec.albers.diss2 <- ca.prec.albers.diss[which (ca.prec.albers.diss@data$SRPREC!= "UNASSIGN"),]

areas <- sapply(slot(ca.prec.albers.diss, "polygons"), function(x) sapply(slot(x,"Polygons"), slot, "area"))
test <- spCbind(ca.prec.albers.diss,a2)



writeOGR(ca.prec.albers, dsn="." ,layer="CA2012_CA_AlbersfromR",driver="ESRI Shapefile")

#4). Figure out which is the biggest polygon for each of the SRPREC_KEYs and flag it.

library("data.table")
diss.with.area <- readOGR(".", layer = "CA2012_CA_Albers_diss_fromR")
diss.with.area@data$rowID <- row.name
diss.with.area@data$rowID <- 1:nrow(diss.with.area@data)      #create an index number for each row
just.df <- diss.with.area@data[,c("rowID", "area_sin","SRPREC_KEY")]
max.table <- data.frame(setDT(just.df)[, .SD[which.max(area_sin)], by=SRPREC_KEY])
max.table$use <- 1
max.table <- max.table[c("rowID","use")]
diss.with.area2 <-merge(diss.with.area,max.table)
diss.with.area2@data$use[is.na(diss.with.area2@data$use)] <- 0
writeOGR(diss.with.area2, dsn="." ,layer="CA2012_with_use",driver="ESRI Shapefile")

#Selection.in.QGIS <- "SRPREC" != 'UNASSIGN'  AND  "use"  = 0

return.neighbor.count<-function(in.spolydf){
        message("creating neighbor matrix")
        in.spolydf.nb <-poly2nb(in.spolydf)
        message("creating queens weight")
        in.spolydf.list <- nb2listw(in.spolydf.nb, style = "W", zero.policy = T)
        message(("formating output"))
        num_neig <- data.frame(card(in.spolydf.nb))
        names(num_neig)<-"num_ne"
        in.spolydf@data$num_ne <- num_neig$num_ne
        
        #row.names(num_neig)<-matrix(1:nrow(num_neig)-1)
        
        
        
        #done<- spCbind(in.spolydf,num_neig)
        return(list(in.spolydf.nb,in.spolydf.list,in.spolydf))
}

#open the eliminated one
slivers.missing <- readOGR(".", layer = "test_lt500m2")

slivers.missing.noua <- slivers.missing[which(slivers.missing@data$SRPREC != "UNASSIGN"),]


all.slivers <- return.neighbor.count(slivers.missing)
all.slivers.noUA <- return.neighbor.count(slivers.missing.noua)

writeOGR(all.slivers[3], dsn="." ,layer="CA2012_Elim_Many_noUA",driver="ESRI Shapefile")

aa<-all.slivers[[2]]
locm <- localmoran(slivers.missing$p_obama_20, all.slivers.noUA, na.action= na.omit,zero.policy=T)





writeOGR(in.spolydf2, dsn="." ,layer="CA2012_Elim_Many_noUA",driver="ESRI Shapefile")


#major testing


"UNASSIGN"

