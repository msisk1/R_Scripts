rm(list=ls(all=TRUE)) # clear memory

# packages<- c("ggmap","sp","taRifx.geo", "SpatialTools","plyr","gmapsdistance","RJSONIO","RCurl") # list the packages that you'll need
packages<- c("sp","RJSONIO","RCurl","reshape2") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd("/mnt/smb/Research/OTool_Distances")
setwd("E:\\GISWork_2\\OTool_Distances")
gmaps.key <- "AIzaSyDZy7HLVrouFTsULZ6D6ZyGub8iseJI_OU" 

output.in.progress.file <- "unique_respon_processing.csv"

#Import Data




time.field <- "time_min"
distance.field <- "dist_km"
orig.field <- "cord_src"
dest.field <- "cord_dest"
max.bad <- 10
delay <- 0


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
                print(paste(num.total,i,sep = ": "))
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


data.prep.and.processing<-function(){
        library("rgdal")
        all.enrollment <- read.csv("all_enrollment_centers_xy.csv")
        all.enrollment$EnrolID <- paste("X", row.names(all.enrollment), sep="")
        # 
        # enroll.spdf <- SpatialPointsDataFrame(data = all.enrollment,coords = all.enrollment[c(1,2)], proj4string=CRS("+proj=longlat +datum=WGS84")) 
        # writeOGR(enroll.spdf, dsn="." ,layer="trans_enroll",driver="ESRI Shapefile")
        
        
        
        respon.processing$responID <- paste("Y", row.names(respon.processing), sep="")
        # respon.spdf <- SpatialPointsDataFrame(data = respon.processing,coords = respon.processing[c(2,3)], proj4string=CRS("+proj=longlat +datum=WGS84")) 
        # writeOGR(respon.spdf, dsn="." ,layer="trans_respon",driver="ESRI Shapefile")
        
        
        a1<- read.csv("closest_1-10091.csv")
        a2<- read.csv("closest_2-23827.csv")      
        a3<- read.csv("closest_3-40257.csv")
        a4<- read.csv("closest_4-55110.csv")
        a5<- read.csv("closest_5-75003.csv")
        a6<- read.csv("closest_6-100054.csv")
        a7<- read.csv("closest_7-130877.csv")
        a8<- read.csv("closest_8-165019.csv")
        a.all <- rbind(a1,a2,a3,a4,a5,a6,a7,a8)
        
        
        just.road.closest <- colsplit(a.all$Name," - ",c("responID","EnrolID"))
        
        all.enrollment[,c(dest.field)] <- paste(all.enrollment$Y, all.enrollment$X,sep=", ")
        enrollIDDEst <- all.enrollment[,c(dest.field,"EnrolID")]
        just.road.closest <- merge(just.road.closest,enrollIDDEst,by="EnrolID",all.x = T)
        remove(a1,a2,a3,a4,a5,a6,a7,a8)
        
        
        
        names(respon.processing)[names(respon.processing) == 'ID']    <- "EnrolID.OLD"
        names(respon.processing)[names(respon.processing) == 'cord_dest']    <- "cord_dest.OLD"
        respon.processing <- merge(respon.processing, just.road.closest, by = "responID", all.x = T)
                
        #respon.processing2$rerun <- ifelse(respon.processing2$EnrolID.OLD == respon.processing2$EnrolID,0,1)
        #respon.processing2$rerun[is.na(respon.processing$dist_km)] <- 1
        respon.processing$dist_km[respon.processing$EnrolID.OLD != respon.processing$EnrolID] <- NA
        respon.processing$time_min[respon.processing$EnrolID.OLD != respon.processing$EnrolID] <- NA
        
        respon.processing <- respon.processing[order(respon.processing$row.number),]
        respon.processing <- respon.processing[,c(13,1:12,14:15)]
        row.names(respon.processing) <- respon.processing$row.number
        #FINAL SECTION
        respon.processing <- read.csv(output.in.progress.file,stringsAsFactors= F,row.names = 1)  
        
        respon.processing$noRoad <-0
        respon.processing$noRoad[is.na(respon.processing$cord_dest)] <- 1
        respon.processing$EnrolID <- ifelse(!is.na(respon.processing$EnrolID), respon.processing$EnrolID, respon.processing$EnrolID.OLD)
        respon.processing$cord_dest <- ifelse(!is.na(respon.processing$cord_dest), respon.processing$cord_dest, respon.processing$cord_dest.OLD)
        
        
        
        write.csv(respon.processing,output.in.progress.file)
        
        
}
