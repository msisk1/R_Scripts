rm(list=ls(all=TRUE)) # clear memory

# packages<- c("ggmap","sp","taRifx.geo", "SpatialTools","plyr","gmapsdistance","RJSONIO","RCurl") # list the packages that you'll need
packages<- c("sp","RJSONIO","RCurl") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd("/mnt/smb/Research/OTool_Distances")
setwd("N:\\Research\\OTool_Distances")
gmaps.key <- "AIzaSyDZy7HLVrouFTsULZ6D6ZyGub8iseJI_OU" 

output.in.progress.file <- "unique_respon_processing.csv"

#Import Data




time.field <- "time_min"
distance.field <- "dist_km"
orig.field <- "cord_src"
dest.field <- "cord_dest"
max.bad <- 10
delay <- .1


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
        names(min.distances)[names(min.distances) == 'X2']    <- "Euc_Dist"
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
run.a.single.coord.pair.byhand <- function(orig2,dest2){
        json.worked <- F
        distance <- 0
        time <- 0
        status <- "Not_run"
        
        status.string <- '\"status\" : ' 
        legs.string <- "\"legs\" : "
        
        url <- URLencode(paste("https://maps.googleapis.com/maps/api/directions/json?origin=",orig2,"&destination=",dest2,"&key=",gmaps.key, sep=""))
        from.gm <- getURL(url)
        #Should be the try
        try({
                x <-fromJSON(from.gm, simplify = T)
                status <- x$status
                if (status == "OK"){
                        distance <-as.numeric(x$routes[[1]]$legs[[1]]$distance["value"]) / 1000
                        time<- as.numeric(x$routes[[1]]$legs[[1]]$duration["value"]) /60
                        json.worked <- T
                }

        })
        if (!json.worked){
                message("JSON Failed")
                all.lines <- strsplit(from.gm,"\n")[[1]]
                status.index <- as.numeric(grep(status.string,all.lines))
                status.line <- all.lines[status.index]
                status <- substr(status.line,16,nchar(status.line)-1 )
                if (status == "OK"){
                        
                        legs.index <- as.numeric(grep(legs.string,all.lines))
                        distance.line <- all.lines[legs.index+4]
                        distance <- as.numeric(substr(distance.line, 29, nchar(distance.line))) / 1000
                        time.line <- all.lines [legs.index+8]
                        time <- as.numeric(substr(time.line, 29, nchar(time.line))) / 60
                        message(paste("     grep success",distance,time))
                }
        }# end if json did not work
        return(list(distance,time,status))         
        
} # end run.a.single.coord.pair.byhand




#Task 2: Run through the dataset and calculate driving distances


num.bad <- 0
num.total <-0
respon.processing$row.number <- 1:nrow(respon.processing)      #create an index number for each row
for (i in respon.processing$row.number){
        #print(paste(orig, dest))
        if (is.na(respon.processing[i,c(distance.field)])){
                num.total <- num.total + 1
                print(i)
                orig <- respon.processing[i,c("cord_src")] # get origin from DF in the position line 'i', column 'from'
                dest <- respon.processing[i,c("cord_dest")]   # get origin from DF in the position line 'i', column 'to'
                pop <- run.a.single.coord.pair.byhand(orig,dest)
                if (pop[3] == "OK"){
                        respon.processing[i,c(distance.field)] <- pop[1]
                        respon.processing[i,c(time.field)] <- pop[2]
                        
                } # end if (pop[3] == "FAIL"){ 
                else if (pop[3] == "OVER_QUERY_LIMIT"){
                        print(paste(pop[3],num.total))
                        break
                } # end if (pop[3] == "FAIL"){
                else{
                        respon.processing[i,c(distance.field)] <- NA
                        respon.processing[i,c(time.field)] <- NA
                        num.bad <- num.bad +1 
                        print(paste("number bad:", num.bad, pop[3]))
                } # end if (pop[3] == "FAIL"){
                Sys.sleep(delay)
                
        }# end if distance.field is not null
        if (num.bad > max.bad) break
}# end for loop
print("Writing csv")
write.csv(respon.processing,output.in.progress.file)

write.as.shapefile(respon.processing[1:1000,],"temp_shape2")

# cat ("Done. Press [enter] to continue")
# line <- readline()
write.as.shapefile<- function(table, out.name){
        latlong <- "+init=epsg:4326"
        google <- "+init=epsg:3857"
        coordinates(table)=~x+y
        proj4string(table) <- CRS(latlong)
        writeOGR(table, dsn="." ,layer=out.name,driver="ESRI Shapefile")
        
}
