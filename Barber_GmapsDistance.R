rm(list=ls(all=TRUE)) # clear memory

packages<- c("ggmap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

setwd("/mnt/smb/")
setwd("E:\\GISWork_2\\Barber_Distances\\")
#gmaps.key <- "AIzaSyDZy7HLVrouFTsULZ6D6ZyGub8iseJI_OU" #Unneeded!

# append.dist.to.table <- function(base.table, from.name, to.name, name.string="none",index.string) {
#         if (name.string =="none"){
#                 name.string = to.name 
#         }
#         cut.base <- base.table[,c(index.string, from.name,to.name)]
#         newt <- mapdist(cut.base[,from.name],cut.base[,to.name])
#         newt <- newt[c(4,7)]
#         names(newt)[names(newt) == 'km']    <- paste(name.string,"_dist",sep="")
#         names(newt)[names(newt) == 'minutes']    <- paste(name.string,"_time",sep="")
#         newt <-cbind(cut.base,newt)
#         newt
# }#end append.dist.to.table


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




barber.data <- read.csv("Barber_HCF_Locs.csv", stringsAsFactors=F)
if (file.exists("Barber_Lab_Distance.csv")){
        print("Loading file")
        lab.distances<-read.csv("Barber_Lab_Distance.csv")
}else{
        complete.lab <- barber.data[ which(barber.data$Lab.Location !=''),]
        #tester <-complete.lab[1:10,]
        lab.distances <- append.dist.to.table2(base.table = complete.lab,from.name = "Address",to.name = "Lab.Location",name.string = "Lab",index.string="PLWHIV.ID")
        write.csv(lab.distances,"Barber_Lab_Distance.csv")
}
if (file.exists("Barber_PCP_Distance.csv")){
        print("Loading file")
        pcp.distances<-read.csv("Barber_PCP_Distance.csv")
}else{
        complete.pcp <- barber.data[ which(barber.data$PCP.Location !=''),]
        pcp.distances <- append.dist.to.table2(base.table = complete.pcp,from.name = "Address",to.name = "PCP.Location",name.string = "PCP",index.string="PLWHIV.ID")
        write.csv(pcp.distances,"Barber_PCP_Distance.csv")
}
if (file.exists("Barber_IDS_Distance.csv")){
        print("Loading file")
        ids.distances<-read.csv("Barber_IDS_Distance.csv")
}else{
        complete.ids <- barber.data[ which(barber.data$IDS.Location !=''),]
        ids.distances <- append.dist.to.table2(base.table = complete.ids,from.name = "Address",to.name = "IDS.Location",name.string = "IDS",index.string="PLWHIV.ID")
        write.csv(ids.distances,"Barber_IDS_Distance.csv")
}

if (file.exists("Barber_pharm_Distance.csv")){
        print("Loading file")
        pharm.distances<-read.csv("Barber_pharm_Distance.csv")
}else{
        complete.pharm <- barber.data[ which(barber.data$Pharmacy.Location !=''& barber.data$Pharmacy.Location !='mail order'),]
        pharm.distances <- append.dist.to.table2(base.table = complete.pharm,from.name = "Address",to.name = "Pharmacy.Location",name.string = "Pharm",index.string="PLWHIV.ID")
        write.csv(pharm.distances,"Barber_pharm_Distance.csv")
}


final.merge <- lab.distances[c(2,6,7)]
final.merge <- merge(final.merge,pcp.distances[c(2,6,7)],by = "PLWHIV.ID",all = T)
final.merge <- merge(final.merge,ids.distances[c(2,6,7)],by = "PLWHIV.ID",all = T)
final.merge <- merge(final.merge,pharm.distances[c(2,6,7)],by = "PLWHIV.ID",all = T)
output.merge <- merge(barber.data,final.merge,by = "PLWHIV.ID", all = T)


##TESTING AREA

#newt <- mapdist(tester$Address,tester$Lab.Location)
base.table = complete.lab
from.name = "Address"
to.name = "Lab.Location"
name.string = "Lab"
index.string="PLWHIV.ID"

DF <- tester
DF$row.number <- 1:nrow(DF)      #create an index number for each row

for (i in DF$row.number){
        print(i)
        orig <- DF[i,c(from.name)] # get origin from DF in the position line 'i', column 'from'
        dest <- DF[i,c(to.name)]   # get origin from DF in the position line 'i', column 'to'
        a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
        a$row.number <- i # include in temp. df 'a' the index number of the row from DF
        #DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
        DF$hours[match(a$row.number, DF$row.number)] <- a$hours # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
        DF$km[match(a$row.number, DF$row.number)] <- a$km # ibdem
}
