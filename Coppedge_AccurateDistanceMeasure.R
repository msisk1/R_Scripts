rm(list=ls(all=TRUE)) # clear memory

packages<- c("maptools","rgdal","foreign","cshapes","reshape2") 
lapply(packages, require, character.only=T) 
setwd("E:\\GISWork_2\\Coppedge_HistoricPoliticalMaps\\Pairwise_DistanceOverTime\\VDEM_Data")



distmatrix.custom <- function(tolerance=0.1, useGW=T, path ="C:\\Temp\\1901_working_hires.shp", ID_Var = "TEXT_ID") {
        print(path)
        # check input
        
        if (tolerance<0) {
                stop("Tolerance must be >=0")
        }
        
        
        # load the dataset
        cshp.full <- readShapePoly(path, proj4string=CRS("+proj=longlat +ellps=WGS84"))
        
        
        cshp.part <- subset(cshp.full, cshp.full$InThisYEar == 1)
        
        # compute pairwise distances
        cshp.part <- cshp.part[order(cshp.part$TEXT_ID),]
        ccodes <- cshp.part$TEXT_ID
        
        resultmatrix <- matrix(0, nrow=length(ccodes), ncol=length(ccodes))
        colnames(resultmatrix) <- ccodes
        rownames(resultmatrix) <- ccodes
        
        # simplify the polygons
        cshp.simple <- thinnedSpatialPoly(cshp.part, tolerance, minarea=0, avoidGEOS=T)
        
        for (c1 in 1:(length(ccodes)-1)) {
                for (c2 in (c1+1):length(ccodes)) {
                        
                        # compute distance
                        dist <- cshp.mindist(cshp.simple[c1,], cshp.simple[c2,])
                        resultmatrix[c1,c2] <- dist
                        resultmatrix[c2,c1] <- dist 
                }
        }
        
        resultmatrix
}

cshp.mindist <- function(polygon1, polygon2) {
        
        # create matrices containing all points of the polygons
        p1 <- ldply(polygon1@polygons[[1]]@Polygons, function(y) {y@coords})
        p2 <- ldply(polygon2@polygons[[1]]@Polygons, function(y) {y@coords})
        
        # use spDists function to compute distances between all pairs of points
        min(spDists(as.matrix(p1), as.matrix(p2), longlat=T))
}

each.thing <- function(filename){
        index <- regexpr("/",filename)[1]
        basename <- substr(filename,index+1, index +5)
        out.name <- paste(basename,"_matrix.csv",sep="")
        #print(paste("working: ",basename))
        each.matrix <- distmatrix.custom(path = filename)
        melted<- melt(each_csv, id.vars=c("X"), value.name = "dist")
        names(melted)[names(melted) == 'X']    <- 'From'
        names(melted)[names(melted) == 'variable']    <- 'To'
        melted$Year<-as.numeric(basename)
        write.csv(melted, out.name)
}


shp_files <- list.files(pattern = '.shp$')

lapply(shp_files, each.thing)




#Opening all the csvs and restructuring them

merge.a.stack.of.csv<-function(csv_files,path="",needs.melting = FALSE){
        first <- TRUE
        all_merged <- NULL
        for (filename in csv_files){
                basename <- substr(filename,1, 4)
                each_csv <- read.csv(paste(path,filename,sep=""))     
                if (needs.melting){
                        melted<- melt(each_csv, id.vars=c("X"), value.name = "dist")
                        names(melted)[names(melted) == 'X']    <- 'From'
                        names(melted)[names(melted) == 'variable']    <- 'To'
                        melted$Year<-as.numeric(basename)
                        each_csv <- melted
                }#end if needs melting
                if (first){
                        all_merged <- each_csv
                        first<-F
                } else{
                        all_merged <-rbind(all_merged,each_csv)
                }#end else      
                
        }
        all_merged
}

csv.files.geachron <- list.files(path = "geachron",pattern = '.csv$')
all.vdem.geachron <- merge.a.stack.of.csv(csv.files.geachron, path = "geachron\\",needs.melting = TRUE)
write.csv(all.vdem.geachron,"1900-1945_allPairwise.csv")

#Cshapes Stuff

years <- seq(1946, 2012, by=1)
for (year in years){
        print(year)
        mati <- as.data.frame(distmatrix(as.Date(paste(year,"-1-1", sep="")),type="mindist",tolerance =0.1, useGW=FALSE))
        mati$From = rownames(mati)
        melted<- melt(mati, id.vars=c("From"), value.name = "dist")
        names(melted)[names(melted) == 'variable']    <- 'To'
        melted$Year<-as.numeric(year)
        write.csv(melted, paste("cshapes\\",year,"_matrix.csv",sep=""))
        
}


csv.files.cshapes <- list.files(path = 'cshapes\\', pattern = '.csv$')
all.vdem.cshapes <- merge.a.stack.of.csv(csv.files.cshapes,path = 'cshapes\\')

#DELETE ME
all.vdem.cshapes <- read.csv("1946-2012_allPairwise.csv")
#End DELETE ME

cow.vdem.eqiv <- read.csv("cowv-demequivalency.csv")

names(cow.vdem.eqiv)[names(cow.vdem.eqiv) == 'country_text_id']    <- 'From_vdem'
names(cow.vdem.eqiv)[names(cow.vdem.eqiv) == 'cowcode']    <- 'From'
all.vdem.cshapes <- merge(all.vdem.cshapes,cow.vdem.eqiv, by="From",all.x=TRUE)

names(cow.vdem.eqiv)[names(cow.vdem.eqiv) == 'From']    <- 'To'
names(cow.vdem.eqiv)[names(cow.vdem.eqiv) == 'From_vdem']    <- 'To_vdem'
all.vdem.cshapes <- merge(all.vdem.cshapes,cow.vdem.eqiv, by="To",all.x=TRUE)

names(all.vdem.cshapes)[names(all.vdem.cshapes) == 'From']    <- 'From_cow'
names(all.vdem.cshapes)[names(all.vdem.cshapes) == 'To']    <- 'To_cow'
all.vdem.cshapes$X <-NULL
all.vdem.cshapes$X.1 <-NULL

all.vdem.cshapes <- all.vdem.cshapes [order(all.vdem.cshapes$Year),]
all.vdem.cshapes$dist <- as.integer(all.vdem.cshapes$dist)

write.csv(all.vdem.cshapes,"1946-2012_allPairwise.csv", row.names=FALSE)

#Renaming the Geachron data
all.vdem.geachron<- read.csv("1900-1945_allPairwise.csv")

cow.vdem.eqiv <- read.csv("cowv-demequivalency.csv")

names(cow.vdem.eqiv)[names(cow.vdem.eqiv) == 'country_text_id']    <- 'From'
names(cow.vdem.eqiv)[names(cow.vdem.eqiv) == 'cowcode']    <- 'From_cow'
all.vdem.geachron <- merge(all.vdem.geachron,cow.vdem.eqiv, by="From",all.x=TRUE)

names(cow.vdem.eqiv)[names(cow.vdem.eqiv) == 'From']    <- 'To'
names(cow.vdem.eqiv)[names(cow.vdem.eqiv) == 'From_cow']    <- 'To_cow'
all.vdem.geachron <- merge(all.vdem.geachron,cow.vdem.eqiv, by="To",all.x=TRUE)

names(all.vdem.geachron)[names(all.vdem.geachron) == 'From']    <- 'From_vdem'
names(all.vdem.geachron)[names(all.vdem.geachron) == 'To']    <- 'To_vdem'
all.vdem.geachron$X <-NULL
all.vdem.geachron$X.1 <-NULL

all.vdem.geachron <- all.vdem.geachron [order(all.vdem.geachron$Year),]
all.vdem.geachron$dist <- as.integer(all.vdem.geachron$dist)

write.csv(all.vdem.geachron,"1900-1945_allPairwise.csv", row.names=FALSE)
