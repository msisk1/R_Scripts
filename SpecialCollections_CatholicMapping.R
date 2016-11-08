rm(list=ls(all=TRUE)) # clear memory
#test
packages<- c("foreign","ggmap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

setwd("E:\\GISWork_2\\Bohlman_CatholicsInAmerica")

seton.bibles <- read.csv("RawData\\SetonBibles_Subscription_Lists.csv", stringsAsFactors = F)

if (!file.exists("SetonBibles_freqTable.csv")){
        seton.bibles.to.geocode <- data.frame(table(seton.bibles$SubscriberCity))
        seton.bibles.to.geocode$SubscriberCity <- as.character(seton.bibles.to.geocode$Var1)
        
        seton.bibles.to.geocode$city <- seton.bibles.to.geocode$SubscriberCity
        
        seton.bibles.to.geocode$city[seton.bibles.to.geocode$SubscriberCity == "Washington City, George-Town, Fredericktown, &c."] <-"Washington DC"
        
        seton.bibles.to.geocode <- seton.bibles.to.geocode[c(2:7),c(3,4,2)]
        
        seton.bibles.geocoded<-cbind.data.frame(seton.bibles.to.geocode, geocode(seton.bibles.to.geocode$city))
        write.csv(seton.bibles.geocoded, "SetonBibles_freqTable.csv",row.names =F)
}else{
        seton.bibles.geocoded <- read.csv("SetonBibles_freqTable.csv", stringsAsFactors = F)
        seton.bibles.all <- merge(seton.bibles, seton.bibles.geocoded[,c(1,4,5)],by="SubscriberCity")
        write.csv(seton.bibles.all, "SetonBibles_all_city.csv",row.names =F)
}


baden.bibles <- read.csv("RawData\\BadenBibles_Subscription_Lists.csv", stringsAsFactors = F)

if (!file.exists("BadenBibles_freqTable.csv")){
        baden.bibles.to.geocode <- data.frame(table(baden.bibles$SubscriberCity))
        baden.bibles.to.geocode$SubscriberCity <- as.character(baden.bibles.to.geocode$Var1)
        
        baden.bibles.to.geocode$city <- baden.bibles.to.geocode$SubscriberCity
        
        baden.bibles.to.geocode$city[baden.bibles.to.geocode$SubscriberCity == "Washington City, George-Town, Fredericktown, &c."] <-"Washington DC"
        
        baden.bibles.to.geocode <- baden.bibles.to.geocode[c(2:nrow(baden.bibles.to.geocode)),c(3,4,2)]
        
        baden.bibles.geocoded<-cbind.data.frame(baden.bibles.to.geocode, geocode(baden.bibles.to.geocode$SubscriberCity))
        write.csv(baden.bibles.geocoded, "BadenBibles_freqTable.csv",row.names =F)
}else{
        baden.bibles.geocoded <- read.csv("BadenBibles_freqTable.csv", stringsAsFactors = F)
        baden.bibles.all <- merge(baden.bibles, baden.bibles.geocoded[,c(1,4,5)],by="SubscriberCity")
        write.csv(baden.bibles.all, "BadenBibles_all_city.csv",row.names =F)
}




sentinal <- read.csv("RawData\\Jesuit_Sentinel.csv", stringsAsFactors = F)
if (!file.exists("sentinal_freqTable.csv")){
        sentinal$code <- paste(sentinal$Subscriber.City,sentinal$Date,sentinal$Year, sep="-")
        sentinal <- sentinal[!(sentinal$Subscriber.City == ""),]
        sentinal.to.geocode <- data.frame(table(sentinal$Subscriber.City))
        sentinal.to.geocode$Subscriber.City <- as.character(sentinal.to.geocode$Var1)

        
        sentinal.to.geocode <- sentinal.to.geocode[c(2:nrow(sentinal.to.geocode)),c(3,2)]
        sentinal.geocoded <-cbind.data.frame(sentinal.to.geocode, geocode(sentinal.to.geocode$Subscriber.City))
        write.csv(sentinal.geocoded, "sentinal_freqTable.csv",row.names =F)
}else{
        sentinal.geocoded <- read.csv("sentinal_freqTable.csv", stringsAsFactors = F)
        sentinal.all <- merge(sentinal, sentinal.geocoded,by="Subscriber.City")
        write.csv(sentinal.all, "sentinal_all_city.csv",row.names =F)
        sentinal.time.base <- unique(sentinal.all[,c("Subscriber.City","Date","Year","code","Freq","lon","lat")])
        write.csv(sentinal.time.base, "sentinal_time.csv",row.names = F)
}