# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory



packages<- c("rgdal","raster","gdistance") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first



setwd("E:/GISWork_2/Hui_China") #WIndows maching
# setwd("/home/matthew/Documents/HUI") #Linux machine
latlong <- "+init=epsg:4326" #This is the espg code for the WGS 1984 geographic projection


dem <- raster("chgis_dem.tif")   #load the raster: Make sure the auxillary files are present
dem.low <-aggregate(dem, fact =  5)
proj4string(dem) #verify that this is non-NA

#Open the list of points


trial.sites <- read.csv("test_locations.csv")  #Read the table with Lat and lon
# trial.sites.table <- trial.sites
coordinates(trial.sites) <- ~E + N             #Define the coordinates to convert it to a spatial points data frame
proj4string(trial.sites) <- CRS(latlong)  

trial.sites <- spTransform(trial.sites, CRS(proj4string(dem)))
(proj4string(trial.sites) == proj4string(dem))


scaleBB <- function(box){
        # box <- bbox(trial.sites[1:2,])
        # box <- bbox(trial.sites[x:x+1,])
        ew <- (box["E","max"] -box["E","min"]) / 4
        ns <- (box["N","max"] -box["N","min"]) / 4
        M <- matrix(c(-ew,-ns,ew,ns), ncol = 2)
        return(box + M)
}




altDiff <- function(x){x[2] - x[1]}

calculateConductance <- function(in.raster){
        hd <- transition(in.raster, altDiff, 8, symm=FALSE)
        slope <- geoCorrection(hd)
        # plot(dem.low)
        adj <- adjacent(in.raster, cells=1:ncell(in.raster), pairs=TRUE, directions=8)
        speed <- slope
        speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
        cond <- geoCorrection(speed)  
        return(cond)
}


first <- T
for (x in 1:(nrow(trial.sites)-1)){
        # x <- 1
        print(paste(x,x+1))
        clip_dem <- crop(dem,scaleBB(bbox(trial.sites[x:(x+1),])))
        Conductance <- calculateConductance(clip_dem)
        each.line <- shortestPath(Conductance, trial.sites@coords[x,], trial.sites@coords[x+1,], output="SpatialLines")
        if (first){
                all <- each.line
                first <- FALSE
        }else{
                all <- all + each.line
        }
}

plot(dem.low)
plot(trial.sites, add=T)
plot(all, add=TRUE)
all.df <- SpatialLinesDataFrame(all, data.frame(ID = c(1:length(all))))

writeOGR(obj = all.df, dsn= ".", layer = "test", driver="ESRI Shapefile") #writes the spatial points data frame as a shapefile

# plot(AtoB, add=TRUE)
# plot(trial.sites,add=T)
