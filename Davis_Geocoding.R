rm(list=ls(all=TRUE)) # clear memory
packages<- c("ggmap", "foreign") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first



setwd("E:\\GISWork_2\\Davis_Churches")
church.data <- read.dbf("Black Catholic Parishes 2016.dbf", as.is = TRUE)
church.data.all <- church.data
church.data <- church.data[,1:5]

church.data$row.number <- 1:nrow(church.data)      #create an index number for each row
for (i in church.data$row.number){
        church <- church.data[i,c("NAME")]
        address <- church.data[i,c("ADDRESS")]
        #each <- geocode(paste(church, address, sep=", "), output = "more", source = "google")
        each <- geocode(address, output = "more", source = "google")
        church.data[i,c("lat")] <- each$lat
        church.data[i,c("lon")] <- each$lon
        church.data[i,c("type")] <- each$type
        church.data[i,c("new_addr")] <- each$address

}
        
        