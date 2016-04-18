rm(list=ls(all=TRUE)) # clear memory

packages<- c("geonames") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first


setwd("E:\\GISWork_2\\Hendriksen_1916Irish")


options(geonamesUsername="mlsisk")

#in.file <- read.csv("Broadcasts.csv", stringsAsFactors = F)
#list.of.locations <- (unique(in.file[,c("City")]))


#list.of.locations <- list.of.locations[1:30]

run.a.list.of.cities <- function(list.of.locations){
        solution<-data.frame()
        for (i in list.of.locations){
                print(i)
                res <- GNsearch(name=i, country="US")
                each <- cbind(City = i, res[1,c("lng", "lat", "adminName1","fclName")])
                print(each)
                solution <- rbind(solution,each, fill=TRUE)   
                
        }
        solution
}

#first.ll.pass <- merge(in.file, solution, by = "City", all.x = T)
#write.csv(first.ll.pass,"broadcasts_ll.csv")



#Processing new file
#first.ll.pass <- read.csv("broadcasts_ll.csv")
new.ll <- read.csv("Broadcasts_4-12-16.csv")
remove(new.ll)
new.ll.un <- unique(new.ll[,c(1:7)])
new.ll.un$cityState <- paste(new.ll.un$City, new.ll.un$State, sep = ", ")
new.ll.un$marketState <- paste(new.ll.un$Market, new.ll.un$State, sep = ", ")
write.csv(new.ll.un,"hope.csv")



list.city <- (unique(new.ll.un[,c("cityState")]))
from.city <- run.a.list.of.cities(list.city)
from.city <- from.city[complete.cases(from.city),]


coded.by.city <- merge(new.ll.un, from.city, by = "City", all.x = T)
write.csv(new.ll.un,"hope.csv")

loc.by.city <- read.csv("Broadcasts_listCity.csv")
out.all.city <- merge(new.ll.un,loc.by.city, by.x="cityState",by.y = "original.address", all.x=T)
write.csv(out.all.city,"BroadCasts_fixed_City.csv")







list.market <- (unique(new.ll.un[,c("Market")]))



from.market <- run.a.list.of.cities(list.market)

