rm(list=ls(all=TRUE)) # clear memory
packages<- c("ggmap", "foreign") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first


setwd("C:/Temp/Tiles") 

all.jpeg.tiles <- list.files(path = "ArcSat",pattern=".jpeg")
#pattern = FOLDER-FILE-ZOOM