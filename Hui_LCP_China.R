# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory


packages<- c("rgdal","raster","gdistance","rgeos","sp","maptools", "dplyr") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first


run.at.low.res <- T
correct.format.check <- T


# setwd("E:/GISWork_2/Hui_China") #WIndows maching
setwd("/home/matthew/Documents/Hui_China") #Linux machine

# exten <- "2018-04-02"
# out.path <- paste("paths_",exten,sep="")
# 
# in.files <- list.files(path = out.path, pattern = ".csv$")
# in.file <- in.files[2]

#Functions
scaleBB <- function(box){
  # box <- bbox(site.points[1:2,])
  # box <- bbox(site.points[x:x+1,])
  #renamed bbox fields? This needed to be switched to indices
  # ew <- (box["E","max"] -box["E","min"]) / 4
  # ns <- (box["N","max"] -box["N","min"]) / 4
  ew <- (box[1,1] -box[1,2]) / 4
  ns <- (box[2,1] -box[2,2]) / 4
  if (ns < 1000){
    ns <- ew
  }
  # M <- matrix(c(-ew,-ns,ew,ns), ncol = 2)
  M <- matrix(c(-ew,-ns,ew,ns), ncol = 2)
  
    return(box - M)
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



#Variables
latlong <- "+init=epsg:4326" #This is the espg code for the WGS 1984 geographic projection
google <- "+init=epsg:3857"  #This is the espg code for the web mercator projection used by google earth

#open the raster 
if (!file.exists("chgis_dem_processed.tif")){
  dem <- raster("chgis_dem.tif")   #load the raster: Make sure the auxillary files are present
  proj4string(dem)
  proj4string(dem) <- crs("+init=epsg:2333")
  proj4string(dem)
  library(rworldmap)
  world.map <- getMap()
  # proj4string(world.map)
  dem <- projectRaster(dem, crs = google)
  
  asia.map <- world.map[world.map@data$continent == "Eurasia"& !is.na(world.map@data$continent),]
  asia.map <- spTransform(asia.map, crs(proj4string(dem)))
  dem <- mask(dem, asia.map)
  dem[dem == -32768] <- NA #Filtering out NA Values
  writeRaster(dem,"chgis_dem_processed.tif")
  
}else{
  dem <- raster("chgis_dem.tif")   #load the raster: Make sure the auxillary files are present  
  dem[dem == -32768] <- NA #Filtering out NA Values
}
if (!file.exists("chgis_dem_low.tif")){
  print("no.file")
  dem.low <-aggregate(dem, fact =  5)
  writeRaster(dem.low,"chgis_dem_low.tif", overwrite = T)
}else{
  dem.low <- raster("chgis_dem_low.tif")
}

proj4string(dem) #verify that this is non-NA

# dem[is.na(dem)] <- 100000
if (run.at.low.res) {
  dem <- dem.low
}

date.string <- "2020-03-28"
file.names <- list.files(path = paste0("paths_",date.string, "/"), pattern = "*.csv$")
first.of.the.files <- TRUE
for (each.file in file.names){
  file.string <- tools::file_path_sans_ext(each.file)
  print(file.string)
  raw.file <- paste("paths_",date.string, "/",file.string, ".csv", sep = "")
  #new data processing section
  master.file <- read.csv(raw.file, stringsAsFactors = F, encoding = "UTF-8")
  # print(names(master.file))
# }#fake end
  
  
  master.file$N <- as.numeric(master.file$N)
  master.file$N.1 <- as.numeric(master.file$N.1)
  
  master.file <- master.file %>%
    filter(!is.na(N),
           !is.na(N.1))
  #ADDING OPTIONAL CHECK FOR CORRECT DATA
  if (correct.format.check){
    library(tidyverse)
    templat <- if_else(master.file$N > 70, true = master.file$E,false = master.file$N)
    templon <- if_else(master.file$N > 70, true = master.file$N,false = master.file$E)  
    master.file$N <- templat
    master.file$E <- templon
    
    templat <- if_else(master.file$N.1 > 70, true = master.file$E.1,false = master.file$N.1)
    templon <- if_else(master.file$N.1 > 70, true = master.file$N.1,false = master.file$E.1)  
    master.file$N.1 <- templat
    master.file$E.1 <- templon
    
  }
  
  x<- 1
  subset.file <- master.file[master.file$Identification.Number ==1,]
  first <- T
  out.file.base <- paste(file.string, "_",date.string, sep = "")
  
  
  if (file.exists(paste(out.file.base,".rData", sep=""))){
    print("loading existing file")
    load(paste(out.file.base,".rData", sep=""))
  }else{
    for (each.id in row.names(master.file)){
      
      subset.file <- master.file[row.names(master.file) ==each.id,]
      print(paste(each.id,x,sep=": "))
      x <- x+1
      subset.origins <- subset.file
      coordinates(subset.origins) <- ~E + N             #Define the coordinates to convert it to a spatial points data frame
      proj4string(subset.origins) <- CRS(latlong)
      subset.origins <- spTransform(subset.origins, CRS(proj4string(dem)))
      # 
      # 
      subset.dest <- subset.file
      coordinates(subset.dest) <- ~E.1 + N.1             #Define the coordinates to convert it to a spatial points data frame
      proj4string(subset.dest) <- CRS(latlong)
      subset.dest <- spTransform(subset.dest, CRS(proj4string(dem)))
      subset.all <- subset.dest + subset.origins
      q <- bbox(subset.all)
      q[,"max"] <- q[,"max"] + res(dem)
      q[,"min"] <- q[,"min"] - res(dem)
      clip_dem <- crop(dem,scaleBB(q))
      #crate concordance for the clip
      Conductance <- calculateConductance(clip_dem)
      
      #shortest
      each.line <- shortestPath(Conductance,origin = subset.origins@coords[1,],goal = subset.dest@coords[1,] , output="SpatialLines")
      each.sldf <- SpatialLinesDataFrame(sl = each.line, data = subset.file, match.ID = F)
      
      
      #end point for reloading
      # row.names(each.sldf) <- row.names(subset.file)
      if (first){
        all <- each.sldf
        first <- F
      }else{
        all <-all + each.sldf
      }
    }# end else all
    
    writeOGR(dsn=".", layer = paste("paths_",date.string, "/",out.file.base, ".shp", sep = ""),obj=all, driver = "ESRI Shapefile" )
    # plot(all)
    all$dynasty <- out.file.base
    
    save(all,file=paste(out.file.base,".rData", sep=""))
    save(all,file=paste("/home/matthew/GIT/R_Scripts/ShinyApps/HUI_ChinaPathsApp_v3_Many",out.file.base,".rData", sep=""))
    
  }
  
  if (first.of.the.files){
    totally.all <- all
    first.of.the.files <- FALSE
  }else{#not first file
    totally.all <- rbind(totally.all, all)
  } 
  } #end for loop to do the whole list of files
save(totally.all,file=paste("/home/matthew/GIT/R_Scripts/ShinyApps/HUI_ChinaPathsApp_v3_Many/data/allData",date.string,".rData", sep=""))
