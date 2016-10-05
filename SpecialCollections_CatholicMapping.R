rm(list=ls(all=TRUE)) # clear memory
#test
packages<- c("foreign","ggmap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

setwd("E:\\GISWork_2\\Bohlman_CatholicsInAmerica")

bibles <- read.csv("RawData\\Bibles_Subscription_Lists.csv", stringsAsFactors = F)

if (!file.exists("bibles_freqTable.csv")){
        bibles.to.geocode <- data.frame(table(bibles$SubscriberCity))
        bibles.to.geocode$SubscriberCity <- as.character(bibles.to.geocode$Var1)
        
        bibles.to.geocode$city <- bibles.to.geocode$SubscriberCity
        
        bibles.to.geocode$city[bibles.to.geocode$SubscriberCity == "Washington City, George-Town, Fredericktown, &c."] <-"Washington DC"
        
        bibles.to.geocode <- bibles.to.geocode[c(2:7),c(3,4,2)]
        
        bibles.to.geocode<-cbind.data.frame(bibles.to.geocode, geocode(bibles.to.geocode$city))
        write.csv(bibles.to.geocode, "bibles_freqTable.csv",row.names =F)
}else{
        bibles.to.geocode <- read.csv("bibles_freqTable.csv", stringsAsFactors = F)
        bibles.all <- merge(bibles, bibles.to.geocode[,c(1,4,5)],by="SubscriberCity")
        write.csv(bibles.all, "bibles_all_city.csv",row.names =F)
}
