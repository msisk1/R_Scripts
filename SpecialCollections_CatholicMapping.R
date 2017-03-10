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
        # baden.bible.freq2 <- 
        baden.bibles.all <- merge(baden.bibles, baden.bibles.geocoded[,c(1,4,5)],by="SubscriberCity")
        write.csv(baden.bibles.all, "BadenBibles_all_city.csv",row.names =F)
}




sentinal <- read.csv("RawData\\Jesuit_Sentinel.csv", stringsAsFactors = F)
sentinal$DateStr <- as.character(as.Date(sentinal$Date.1, format = "%d-%m-%Y"))
sentinal$Date.1 <- NULL

sentinal$Dated <-as.Date(sentinal$DateStr,format = "%Y-%m-%d")
sentinal <- sentinal[sentinal$Subscriber.City != "",] #remove null cities

date.list <- c("1829-09-30","1829-12-31","1830-03-31","1830-06-30","1830-09-30")

sentinal.1829Sep <- sentinal[sentinal$Dated <= as.Date("1829-09-30", format = "%Y-%m-%d"),]
sentinal.1829Dec <- sentinal[sentinal$Dated <= as.Date("1829-12-31", format = "%Y-%m-%d"),]
sentinal.1830Mar <- sentinal[sentinal$Dated <= as.Date("1830-03-31", format = "%Y-%m-%d"),]
sentinal.1830Jun <- sentinal[sentinal$Dated <= as.Date("1830-06-30", format = "%Y-%m-%d"),]
sentinal.1830Sep <- sentinal[sentinal$Dated <= as.Date("1830-09-30", format = "%Y-%m-%d"),]




if (!file.exists("sentinal_freqTable.csv")){
        sentinal$code <- paste(sentinal$Subscriber.City,sentinal$Date,sentinal$Year, sep="-")
        sentinal <- sentinal[!(sentinal$Subscriber.City == ""),]
        sentinal.to.geocode <- data.frame(table(sentinal$Subscriber.City))
        sentinal.to.geocode$Subscriber.City <- as.character(sentinal.to.geocode$Var1)

        
        sentinal.to.geocode <- sentinal.to.geocode[c(2:nrow(sentinal.to.geocode)),c(3,2)]
        sentinal.geocoded <-cbind.data.frame(sentinal.to.geocode, geocode(sentinal.to.geocode$Subscriber.City))
        write.csv(sentinal.geocoded, "sentinal_freqTable.csv",row.names =F)
}else{
        sentinal.geocoded <- read.table("sentinal_freqTable2.csv", stringsAsFactors = F, header = T,sep="\t")
        sentinal.all <- merge(sentinal, sentinal.geocoded,by="Subscriber.City")
        write.csv(sentinal.all, "sentinal_all_city.csv",row.names =F)
        sentinal.time.base <- unique(sentinal.all[,c(1,16:22)])
        write.csv(sentinal.time.base, "sentinal_time2.csv",row.names = F)
}

#Sentinal Time spots
rm(list=ls(all=TRUE)) # clear memory
packages<- c("foreign","ggmap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

setwd("E:\\GISWork_2\\Bohlman_CatholicsInAmerica")

sentinal <- read.csv("RawData\\Jesuit_Sentinel.csv", stringsAsFactors = F)
sentinal$DateStr <- as.character(as.Date(sentinal$Date.1, format = "%d-%m-%Y"))
sentinal$Dated <-as.Date(sentinal$DateStr,format = "%Y-%m-%d")

sentinal <- sentinal[,c("Subscriber.City","Dated")]

sentinal <- sentinal[sentinal$Subscriber.City != "",] #remove null cities

date.list <- c("1830-09-30","1830-06-30","1830-03-31","1829-12-31", "1829-09-30")
first <-TRUE

for (each.date in date.list){
        sentinal.setDate <- sentinal[sentinal$Dated <= as.Date(each.date, format = "%Y-%m-%d"),]
        
        sentinal.table <- data.frame(table(sentinal.setDate$Subscriber.City))
        sentinal.table$Subscriber.City <- as.character(sentinal.table$Var1)
        sentinal.table <- sentinal.table[,c(3,2)]
        names(sentinal.table)<-c("SubscriberCity",paste("d",each.date,sep=""))
        if (first){
                sentinal.total <- sentinal.table
                first <- FALSE
        }else{
                sentinal.total <- merge(sentinal.total,sentinal.table,by = "SubscriberCity", all = T)
        }  
        
}
remove(sentinal.setDate,sentinal.table)
#Redoing the geocoding
sentinal.geocoded <-cbind.data.frame(sentinal.total, geocode(sentinal.total$SubscriberCity))
write.csv(sentinal.geocoded, "sentinal_TimeTable.csv",row.names =F)




