#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("maptools","rgdal","leaflet","raster") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

setwd("E:\\GISWork_2\\Dinsman_Modernism\\")

latlong <- "+init=epsg:4326"
google <- "+init=epsg:3857"


mappy <- raster("Clip_Base_Modernism1_Project.tif") #stack instead of raster for multiband image
projection(mappy) <- google
writeRaster(mappy,"ThreeColor.tif",overwrite=TRUE)
