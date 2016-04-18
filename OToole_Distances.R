rm(list=ls(all=TRUE)) # clear memory

packages<- c("ggmap","sp","taRifx.geo", "SpatialTools","plyr") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd("/mnt/smb/Research/OTool_Distances")
setwd("N:\\Research\\OTool_Distances")
#gmaps.key <- "AIzaSyDZy7HLVrouFTsULZ6D6ZyGub8iseJI_OU" #Unneeded!

output.in.progress.file <- "test.csv"


append.dist.to.table2 <- function(base.table, from.name, to.name, name.string="none",index.string) {
  if (name.string =="none"){name.string = to.name }
  DF <- base.table[,c(from.name,to.name)]
  DF$row.number <- 1:nrow(DF)      #create an index number for each row
  for (i in DF$row.number){
    print(i)
    orig <- DF[i,c(from.name)] # get origin from DF in the position line 'i', column 'from'
    dest <- DF[i,c(to.name)]   # get origin from DF in the position line 'i', column 'to'
    a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
    a$row.number <- i # include in temp. df 'a' the index number of the row from DF
    DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
    DF$km[match(a$row.number, DF$row.number)] <- a$km # ibdem
  }# end for loop
  names(DF)[names(DF) == 'km']    <- paste(name.string,"_dist",sep="")
  names(DF)[names(DF) == 'minutes']    <- paste(name.string,"_time",sep="")
  DF
}#end append.dist.to.table

#Import Data
all.respondants <- read.csv("all_participants_xy.csv")
unique.respondants <- unique(all.respondants[c(2,3)])
unique.respondants <- unique.respondants[complete.cases(unique.respondants),] #removing case #98797 which is NA for both coordinates

remove(all.respondants)

all.enrollment <- read.csv("all_enrollment_centers_xy.csv")
all.enrollment$ID <- paste("X", row.names(all.enrollment), sep="")


#TO CUT DOWN THE DATASET
unique.respondants <-unique.respondants[1:50,] #SAMPLE IT DOWN TO 50 FOR TESTINF


#Task 1: Get the Euclidean closest (or two) to each enrollment center
if (!exists(output.in.progress.file)){
        print("Processing file")
        respon.spdf <- SpatialPoints(coords = unique.respondants, proj4string=CRS("+proj=longlat +datum=WGS84"))
        enroll.spdf <- SpatialPoints(coords = all.enrollment[c(1,2)], proj4string=CRS("+proj=longlat +datum=WGS84"))
        eucDist.matrix <- data.frame (spDists(respon.spdf, enroll.spdf, longlat=T))
        min.distances <- data.frame(t(sapply(seq(nrow(eucDist.matrix)), function(i) {
                j <- which.min(eucDist.matrix[i,])
                c(colnames(eucDist.matrix)[j], eucDist.matrix[i,j])
        })))#end min distances
        names(min.distances)[names(min.distances) == 'X1']    <- "ID"
        names(min.distances)[names(min.distances) == 'X2']    <- "Dist_km"
        unique.respondants.with.closest <- cbind(unique.respondants,min.distances)
        
        unique.respondants.with.closest <- merge(unique.respondants.with.closest, all.enrollment, by="ID", all.x=T)
        unique.respondants.with.closest$cord_src <- paste(unique.respondants.with.closest$y, unique.respondants.with.closest$x,sep=", ")
        unique.respondants.with.closest$cord_dest <- paste(unique.respondants.with.closest$y, unique.respondants.with.closest$X,sep=", ")
        #write.csv(unique.respondants.with.closest,output.in.progress.file)
}else{
        unique.respondants.with.closest <- read.csv(output.in.progress.file)  
}


#Task 2: Run through the dataset and calculate driving distances

run.a.single.coord.pair <- function(orig2,dest2){
        a <- mapdist(from = orig2, to = dest2, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
        return(list(a$km,a$minutes))        
}

unique.respondants.with.closest$row.number <- 1:nrow(unique.respondants.with.closest)      #create an index number for each row
for (i in unique.respondants.with.closest$row.number){
        print(i)
        orig <- unique.respondants.with.closest[i,c("cord_src")] # get origin from DF in the position line 'i', column 'from'
        dest <- unique.respondants.with.closest[i,c("cord_dest")]   # get origin from DF in the position line 'i', column 'to'
        print(paste(orig, dest))
        pop <- run.a.single.coord.pair(orig,dest)
        unique.respondants.with.closest[i,c("dist_km")] <- pop[1]
        unique.respondants.with.closest[i,c("dist_time")] <- pop[2]
        
}# end for loop


orig <-unique.respondants.with.closest[2,"cord_src"]
dest <-unique.respondants.with.closest[2,"cord_dest"]

pop <- run.a.single.coord.pair(orig,dest)
for 














ape <- append.dist.to.table2(unique.respondants.with.closest, "cord_src", "cord_dest", name.string="none",index.string = "koop")

#Working area
i<-1
DF<-unique.respondants.with.closest
DF$row.number <- 1:nrow(DF)      #create an index number for each row

orig <- DF[i,c("cord_src")] # get origin from DF in the position line 'i', column 'from'
dest <- DF[i,c("cord_dest")]   # get origin from DF in the position line 'i', column 'to'
#a <- mapdist(from = (41.98015976, -88.13900757), to = (41.98015976, -87.74879), mode = "driving",output = "all") # create temp. df 'a' with the output from mapdist
google_results <- rbind.fill(apply(subset(DF, select=c("cord_src", "cord_dest")), 1, function(x) mapdist(x[1], x[2], mode="driving")))
