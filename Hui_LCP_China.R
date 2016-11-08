# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory



packages<- c("rgdal","raster","gdistance") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first



setwd("E:/GISWork_2/Hui_China") 
dem <- raster("chgis_dem.tif")
altDiff <- function(x){x[2] - x[1]}


latlong <- "+init=epsg:4326" #This is the espg code for the WGS 1984 geographic projection
dem_latlong = projectRaster(dem, crs = latlong)


dem.low <-aggregate(dem_latlong, fact =  10)


trial.sites <- read.csv("test_locations.csv")  #Read the table with Lat and lon
trial.sites.table <- trial.sites
coordinates(trial.sites) <- ~E + N             #Define the coordinates to convert it to a spatial points data frame
proj4string(trial.sites) <- CRS(latlong)  
# plot(trial.sites)
# plot(dem_latlong, add=T)
# plot(trial.sites, add=T)

#Clipping the raster
clip_dem <- crop(dem_latlong,bbox(trial.sites))


hd <- transition(clip_dem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
plot(dem.low)

adj <- adjacent(clip_dem, cells=1:ncell(clip_dem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)

#A <- c(17000000,5000000)
#B <- c(21500000,1000000)
A <- c(trial.sites.table[1,3],trial.sites.table[1,2])
B <- c(trial.sites.table[2,3],trial.sites.table[2,2])
C <- c(trial.sites.table[3,3],trial.sites.table[3,2])

AtoB <- shortestPath(Conductance, A, B, output="SpatialLines")
BtoC <- shortestPath(Conductance, B, C, output="SpatialLines")
all <- union(AtoB,BtoC)
plot(BtoC, add=TRUE)
