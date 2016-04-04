rm(list=ls(all=TRUE)) # clear memory

packages<- c("ggmap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd("/mnt/smb/")
setwd("E:\\GISWork_2\\Barber_Distances\\")
#gmaps.key <- "AIzaSyDZy7HLVrouFTsULZ6D6ZyGub8iseJI_OU" #Unneeded!

index.string.all ="PLWHIV_ID"

append.dist.to.table2 <- function(base.table, from.name, to.name, name.string="none",index.string) {
        if (name.string =="none"){name.string = to.name }
        DF <- base.table[,c(index.string, from.name,to.name)]
        DF$row.number <- 1:nrow(DF)      #create an index number for each row
        for (i in DF$row.number){
                print(i)
                orig <- DF[i,c(from.name)] # get origin from DF in the position line 'i', column 'from'
                dest <- DF[i,c(to.name)]   # get origin from DF in the position line 'i', column 'to'
                a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
                a$row.number <- i # include in temp. df 'a' the index number of the row from DF
                DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
                #DF$hours[match(a$row.number, DF$row.number)] <- a$hours # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
                DF$km[match(a$row.number, DF$row.number)] <- a$km # ibdem
        }# end for loop
        names(DF)[names(DF) == 'km']    <- paste(name.string,"_dist",sep="")
        names(DF)[names(DF) == 'minutes']    <- paste(name.string,"_time",sep="")
        DF
}#end append.dist.to.table


run.all.the.things<-function(file.name, in.data = barber.data, missing.values = c(" ", ""), to.name, from.name = "Match_addr", name.string, index.string=index.string.all){
        if (file.exists(file.name)){
                print("Loading file")
                distances<-read.csv(file.name)
        }
        else{
                complete.cases <- in.data[ which(!in.data[,to.name] %in% missing.values),]                #tester <-complete.lab[1:10,]
                distances <- append.dist.to.table2(base.table = complete.cases,from.name,to.name,name.string,index.string)
                write.csv(distances,file.name, row.names = FALSE)
        } #End Labs
        distances
                
}#end Run.All.the.things



barber.data <- read.csv("Barber_HCF_Locs_geocoded.csv", stringsAsFactors=F)



#Labs
lab.distances <- run.all.the.things(file.name ="Barber_Lab_Distance.csv", to.name = "Lab_Locati", name.string = "Lab")
#PCP
pcp.distances <- run.all.the.things(file.name ="Barber_PCP_Distance.csv", to.name = "PCP_Locati", name.string = "PCP")
#IDS
ids.distances <- run.all.the.things(file.name ="Barber_IDS_Distance.csv", to.name = "IDS_Locati", name.string = "IDS")
#Pharmacy
pharm.distances <- run.all.the.things(file.name ="Barber_pharm_Distance.csv", to.name = "Pharmacy_L", name.string = "Pharm",  missing.values = c(" ", "","mail order"))




final.merge <- lab.distances[c(2,6,7)]
final.merge <- merge(final.merge,pcp.distances[c(2,6,7)],by = index.string.all,all = T)
final.merge <- merge(final.merge,ids.distances[c(2,6,7)],by = index.string.all,all = T)
final.merge <- merge(final.merge,pharm.distances[c(2,6,7)],by = index.string.all,all = T)
output.merge <- merge(barber.data,final.merge,by = index.string.all, all = T)
write.csv(output.merge,"Barber_All_Distances.csv")
