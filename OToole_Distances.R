rm(list=ls(all=TRUE)) # clear memory

packages<- c("ggmap","sp","taRifx.geo", "SpatialTools","plyr","gmapsdistance","RJSONIO","RCurl") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd("/mnt/smb/Research/OTool_Distances")
setwd("N:\\Research\\OTool_Distances")
gmaps.key <- "AIzaSyDZy7HLVrouFTsULZ6D6ZyGub8iseJI_OU" #Unneeded!

output.in.progress.file <- "unique_respon_processing.csv"

#Import Data




time.field <- "time_min"
distance.field <- "dist_km"
orig.field <- "cord_src"
dest.field <- "cord_dest"
max.bad <- 10
#TO CUT DOWN THE DATASET
#unique.respondants <-unique.respondants[1:2505,] #SAMPLE IT DOWN TO 50 FOR TESTINF


#Task 1: Get the Euclidean closest (or two) to each enrollment center
if (!file.exists(output.in.progress.file)){
        print("Processing file")
        all.respondants <- read.csv("all_participants_xy.csv")
        unique.respondants <- unique(all.respondants[c(2,3)])
        unique.respondants <- unique.respondants[complete.cases(unique.respondants),] #removing case #98797 which is NA for both coordinates
        remove(all.respondants)
        all.enrollment <- read.csv("all_enrollment_centers_xy.csv")
        all.enrollment$ID <- paste("X", row.names(all.enrollment), sep="")
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
        unique.respondants.with.closest[,c(orig.field)] <- paste(unique.respondants.with.closest$y, unique.respondants.with.closest$x,sep=", ")
        unique.respondants.with.closest[,c(dest.field)] <- paste(unique.respondants.with.closest$Y, unique.respondants.with.closest$X,sep=", ")
        unique.respondants.with.closest[,c(distance.field)] <- NA
        unique.respondants.with.closest[,c(time.field)] <- NA
        
        write.csv(unique.respondants.with.closest,output.in.progress.file)
        respon.processing<-unique.respondants.with.closest
        remove(all.enrollment,eucDist.matrix,min.distances,unique.respondants,unique.respondants.with.closest)
        

}else{
       respon.processing <- read.csv(output.in.progress.file,stringsAsFactors= F,row.names = 1)  
}


#Task 2: Run through the dataset and calculate driving distances


run.a.single.coord.pair <- function(orig2,dest2){

        out <- tryCatch(
                {
                        results <- gmapsdistance(gsub(" ", "", orig2), gsub(" ", "", dest2), "driving")
                        distance <-results$Distance/ 1000
                        time<- results$Time / 60
                        list(distance,time, "OK") #   
                },
                error=function(cond) {
                        message("Error: Probably over limit")
                        message(paste(orig2,dest2))
                        message(paste(cond,"\n",sep = ""))
                        #message("")
                        return(list(0,0,"FAIL"))
                },
                warning=function(cond) {
                        message("Original warning message:")
                        message(cond)
                        return(list(0,0,"FAIL"))
                },
                finally={
                        
                        #message("Some other message at the end")
                }
        )    
        return(out)
        
} #run.a.single.coord.pair

num.bad <- 0
respon.processing$row.number <- 1:nrow(respon.processing)      #create an index number for each row
for (i in respon.processing$row.number){
        #print(paste(orig, dest))
        if (is.na(respon.processing[i,c(distance.field)])){
                print(i)
                orig <- respon.processing[i,c("cord_src")] # get origin from DF in the position line 'i', column 'from'
                dest <- respon.processing[i,c("cord_dest")]   # get origin from DF in the position line 'i', column 'to'
                pop <- run.a.single.coord.pair(orig,dest)
                if (pop[3] == "OK"){
                        respon.processing[i,c(distance.field)] <- pop[1]
                        respon.processing[i,c(time.field)] <- pop[2]
                        
                } # end if (pop[3] == "FAIL"){ 
                else{
                        respon.processing[i,c(distance.field)] <- NA
                        respon.processing[i,c(time.field)] <- NA
                        num.bad <- num.bad +1 
                        print(paste("number bad:", num.bad))
                } # end if (pop[3] == "FAIL"){
                Sys.sleep(0.1)
                
        }# end if distance.field is not null
        if (num.bad > max.bad) break
}# end for loop

write.csv(respon.processing,output.in.progress.file)





working = F
#Working area
if(working == T){i<-1
orig <- respon.processing[i,c("cord_src")] # get origin from DF in the position line 'i', column 'from'
dest <- respon.processing[i,c("cord_dest")]

pop <- run.a.single.coord.pair(orig,dest)


#trying by hand
#https://maps.googleapis.com/maps/api/directions/json?origin=Boston,MA&destination=Concord,MA&waypoints=Charlestown,MA|via:Lexington,MA&key=YOUR_API_KEY

url1 <- paste("https://maps.googleapis.com/maps/api/directions/json?origin=",orig,"&destination=",dest,"&key=",gmaps.key, sep="")
url <- gsub(" ", "", url1)
geo_data <- getURL(url)
x <-fromJSON(geo_data)
distance <-x$routes[[1]]$legs[[1]]$distance$value / 1000
time<- x$routes[[1]]$legs[[1]]$duration$value / 60

#and now we are just trying by gmapsdistance
set.api.key(gmaps.key)
results = gmapsdistance(gsub(" ", "", orig), gsub(" ", "", dest), "driving")
distance <-results$Distance/ 1000
time<- results$Time / 60




# run.a.single.coord.pair.mapdist <- function(orig2,dest2){
#         out <- tryCatch(
#                 {
#                         a <- mapdist(from = orig2, to = dest2, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
#                         list(a$km,a$minutes) #
#                 },
#                 error=function(cond) {
#                         message("Original error message:")
#                         message(cond)
#                         return(list(0,0))
#                 },
#                 warning=function(cond) {
#                         message("Original warning message:")
#                         message(cond)
#                         return(list(0,0))
#                 },
#                 finally={
#  
#                         #message("Some other message at the end")
#                 }
#         )    
#         return(out)
# } #run.a.single.coord.pair
# 
# run.a.single.coord.pair.byhand <- function(orig2,dest2){
#                 
#                 url1 <- paste("https://maps.googleapis.com/maps/api/directions/json?origin=",orig2,"&destination=",dest2,"&key=",gmaps.key, sep="")
#                 url <- gsub(" ", "", url1) #removes spaces
#                 print(url)
#                 geo_data <- getURL(url)
#                 x <-fromJSON(geo_data, simplify = T)
#                 if(x$status=="OK") {
#                         distance <-x$routes[[1]]$legs[[1]]$distance$value / 1000
#                         time<- x$routes[[1]]$legs[[1]]$duration$value / 60
#                         return(list(distance,time, "OK")) #        
#                 }else{
#                         print("fail")
#                         print(paste(x$status, x$error_message, sep=": "))
#                         return(list(0,0,"FAIL"))
#                 }
#                 
#               
# } #run.a.single.coord.pair
}