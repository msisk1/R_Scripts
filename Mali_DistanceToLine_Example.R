# Example code for distance to lines in Mali

rm(list=ls(all=TRUE)) # clear memory

#install.packages(("geosphere","rgdal")) #Run if necessary to install packages

packages<- c("geosphere","rgdal") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd(".") #Currently it runs in the same file, you may need to set it differently.

#Load line shapefiles as Spatial Lines Data Frame
roads.sldf <- readOGR(dsn = ".", layer = "MLI_roads") # you load a shapefile with the directory in dsn and the name of the file minus .shp as the layer
niger.sldf <- readOGR(dsn = ".", layer = "Niger_River")

#Load example points
example.points <- read.csv("Mali_Example_Points.csv")

dist.roads <- dist2Line(p = example.points[,c("Lon","Lat")], line = roads.sldf)
dist.river <- dist2Line(p = example.points[,c("Lon","Lat")], line = niger.sldf)


#Combine the distance with the original dataframe

example.points.newdata <- cbind(example.points,distance.river = dist.river[,1],distance.roads = dist.roads[,1])
