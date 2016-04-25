#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("rgdal","spdep") # list the packages that you'll need
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
ca.nd <-poly2nb(ca.prec)
ca.list <- nb2listw(ca.nd, style = "W")
