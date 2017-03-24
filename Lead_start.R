# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory



packages<- c("rgdal","raster","gdistance","rgeos") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first



# setwd("E:/GISWork_2/Hui_China") #WIndows maching
setwd("/home/matthew/Desktop") #Linux machine

lead.data <- read.csv("HBB312017.csv", stringsAsFactors = F)
lead.agg <- aggregate( PB_RESULT ~ GEO_ID, data = lead.data, FUN = mean, na.remove = F)
lead.data$year <- substr(lead.data$SPEC_DT,7,10)
lead.agg2 <- data.frame(with(lead.data, tapply(PB_RESULT, list(GEO_ID, year), mean)))
