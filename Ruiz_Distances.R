
#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("rgdal","sp","ggmap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("E:\\GISWork_2\\Ruiz_MexicoDistances") 

# capitol.data <- readOGR(dsn = ".", layer = "MunicipalCaptials_Geocoded", stringsAsFactors = F, encoding = "UTF-8")
capitol.data.table <- read.csv("MexicoMunicipalitiesCapitals_Geocoded.csv", encoding = "UTF-8")
processing.table <- capitol.data.table[,c(34,35,50:57)]

capitols.only <- subset(processing.table, CAPITAL_CO == CODE, select = c(X,Y, CAPITAL_CO))
names(capitols.only) <- c("Cap_X","Cap_Y", "CAPITAL_CO")
processing.table <-  merge(processing.table, capitols.only,by="CAPITAL_CO",all.x = T)

processing.table$Dist2Cap <- distGeo(processing.table[,c("X", "Y")], processing.table[,c("Cap_X","Cap_Y")])
processing.table <-  processing.table[,c(4:9,1,10,2:3,11:13)]
write.csv(processing.table,"MexicoMunicipalitiesCapitals_Processed.csv", row.names = F)


# capitols.only <- unique(capitol.data.table[,c("CAPITAL_CO","CAPITAL_NA")])
# capitols.only <- merge(capitols.only,capitol.data.table[,c("CODE","X","Y")], by.x = "CAPITAL_CO", by.y = "CODE", all.x = T)
# names(capitols.only) <- c("CAPITAL_CO","CAPITAL_NA","Cap_X","Cap_Y")
# processing.table <- capitol.data.table[,c( "STATE_NUMB","STATE_NAME","X","Y","ABBREVIATE","MUNICIPAL_","MUNICIPALI","CAPITAL_CO","CAPITAL_NA")]
# processing.table <-  merge(capitol.data.table, capitols.only,by.)
# 
# 
# newdata <- subset(capitol.data.table, CAPITAL_CO == CODE)
