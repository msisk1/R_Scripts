# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory



packages<- c("rgdal","raster","gdistance") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first



# setwd("E:/GISWork_2/Hui_China") #WIndows maching
setwd("/home/matthew/Documents/HUI") #Linux machine
latlong <- "+init=epsg:4326" #This is the espg code for the WGS 1984 geographic projection



dem <- raster("chgis_dem.tif")   #load the raster: Make sure the auxillary files are present
proj4string(dem) #verify that this is non-NA

#Open the list of points


trial.sites <- read.csv("test_locations.csv")  #Read the table with Lat and lon
trial.sites.table <- trial.sites
coordinates(trial.sites) <- ~E + N             #Define the coordinates to convert it to a spatial points data frame
proj4string(trial.sites) <- CRS(latlong)  

trial.sites <- spTransform(trial.sites, CRS(proj4string(dem)))
(proj4string(trial.sites) == proj4string(dem))

# plot(trial.sites)
plot(dem)
plot(trial.sites, add=T)

#process the raster to match the list of points to be done

dem.low <-aggregate(dem_latlong, fact =  5)
clip_dem <- crop(dem,bbox(trial.sites))
plot(clip_dem)
plot(trial.sites,add=T)


#prepare the raster for gdistance

altDiff <- function(x){x[2] - x[1]}
hd <- transition(clip_dem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
# plot(dem.low)
adj <- adjacent(clip_dem, cells=1:ncell(clip_dem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)

#A <- c(17000000,5000000)
#B <- c(21500000,1000000)
A <- c(trial.sites@coords[1,1],trial.sites@coords[1,2])
B <- c(trial.sites@coords[2,1],trial.sites@coords[2,2])
C <- c(trial.sites.table[3,3],trial.sites.table[3,2])

AtoB <- shortestPath(Conductance, A, B, output="SpatialLines")
BtoC <- shortestPath(Conductance, B, C, output="SpatialLines")
all <- union(AtoB,BtoC)
plot(AtoB, add=TRUE)
