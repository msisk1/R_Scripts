rm(list=ls(all=TRUE)) # clear memory
#test
packages<- c("foreign","ggmap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd("/mnt/smb/Research/OTool_Distances")
setwd("E:\\GISWork_2\\McCune_Geocoding")
raw.list <- read.csv("Survey Data - Spatial - Sheet1.csv")
check.master.list <- function(master.list = raw.list$Master.List,list.to.check){
        back.binary <- (master.list %in% list.to.check)
        return(back.binary)
}


raw.list$Completed.Survey.bin <- check.master.list(list.to.check = raw.list$Completed.Survey)
raw.list$Missed.Appointments.bin <- check.master.list(list.to.check = raw.list$Missed.Appointments)
raw.list$Most.Missed..3.or.more.bin <- check.master.list(list.to.check = raw.list$Most.Missed..3.or.more.)
raw.list$Want.Bus.Service..4.5.bin <- check.master.list(list.to.check = raw.list$Want.Bus.Service..4.5.)
raw.list$Want.Van.Service..4.5.bin <- check.master.list(list.to.check = raw.list$Want.Van.Service..4.5.)
raw.list$Transpo.Problems.bin <- check.master.list(list.to.check = raw.list$Transpo.Problems)
raw.list$Bus.Issues.bin <- check.master.list(list.to.check = raw.list$Bus.Issues)
raw.list$Clinic.Too.Far.bin <- check.master.list(list.to.check = raw.list$Clinic.Too.Far)

bin.list <- raw.list[,c(1,10:17)]
bin.list$Adress <- as.character(bin.list$Master.List)

bin.list$row.number <- 1:nrow(bin.list)      #create an index number for each row
for (i in bin.list$row.number){
        if (is.na(bin.list[i,c("lat")])){
                address <- bin.list[i,c("Adress")]
                each <- geocode(address, output = "more", source = "google")
                if (is.na(each$lon)){
                        print("geocoding failed")
                }else{
                        bin.list[i,c("lat")] <- each$lat
                        bin.list[i,c("lon")] <- each$lon
                        bin.list[i,c("type")] <- each$type
                        bin.list[i,c("new_addr")] <- each$address
                        
                }
                
        }else{
                print("already geocoded")
        }
                
}
bin.list$row.number <-NULL
nrow(unique(bin.list))

write.csv(unique(bin.list),"Binary_Geocoded2.csv",row.names = F)
