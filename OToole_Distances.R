rm(list=ls(all=TRUE)) # clear memory

packages<- c("ggmap","sp","taRifx.geo") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

setwd("/mnt/smb/Research/OTool_Distances")
#setwd("E:\\GISWork_2\\Barber_Distances\\")
#gmaps.key <- "AIzaSyDZy7HLVrouFTsULZ6D6ZyGub8iseJI_OU" #Unneeded!




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

all.respondants <- read.csv("all_participants_xy.csv")
xy.only <- all.respondants[c(2,3)]
unique.only <- unique(xy.only)

all.enrollment <- read.csv("all_enrollment_centers_xy.csv")
poin<-unique.only[1,]



#Task 1: Get the Euclidean closest (or two) to each enrollment center
enroll.points <- all.enrollment[c(1,2)]



#Working area
