# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory



packages<- c("rgdal","raster","gdistance","rgeos") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first



# setwd("E:/GISWork_2/Hui_China") #WIndows maching
setwd("E:/GISWork_2/Beidinger_Lead") #Linux machine




lead.data <- read.csv("HBB312017.csv", stringsAsFactors = F)
lead.data$age <- (as.Date(substr(lead.data$SPEC_DT,1,10), format = "%m/%d/%Y")- as.Date(substr(lead.data$DOB,1,10), format = "%m/%d/%Y")) / 365


lead.data <- lead.data[which (lead.data$code != "Postal" & lead.data$code != "Locality"),]
lead.data <- lead.data[which (lead.data$age >=0 & lead.data$age < 7),]

#For all
lead.agg <- aggregate( PB_RESULT ~ GEO_ID, data = lead.data, FUN = mean, na.remove = F)
total.all <- as.data.frame(table(lead.data$GEO_ID))
names(total.all) <- c("GEO_ID","N_all")
total.elevated <- as.data.frame(table(lead.data[which (lead.data$PB_RESULT>=5),]$GEO_ID))
names(total.elevated) <- c("GEO_ID","N_elev")

lead.agg <- merge(lead.agg,total.all,by="GEO_ID",all=T)
lead.agg <- merge(lead.agg,total.elevated,by="GEO_ID",all=T)



#By year
lead.data$year <- substr(lead.data$SPEC_DT,7,10)
lead.agg2 <- data.frame(with(lead.data, tapply(PB_RESULT, list(GEO_ID, year), mean)))
lead.agg2$GEO_ID <- row.names(lead.agg2)


#getting monthly averages
lead.month2 <- aggregate( PB_RESULT ~ Month, data = lead.data[which (lead.data$GEO_ID=="1400000US18141000600"),], FUN = mean, na.remove = F)


lead.agg2 <- merge(lead.agg2,lead.agg,by="GEO_ID")
write.csv(lead.agg2,"lead_test2.csv")
