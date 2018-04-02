# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory

#Todo:
# Get the polylines all in one
# Make it so changes to the factor for the route names does not change the order
# 

packages<- c("rgdal","raster","gdistance","rgeos","sp","maptools") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first



# setwd("E:/GISWork_2/Hui_China") #WIndows maching
setwd("/home/matthew/Documents/HUI") #Linux machine

exten <- "2018-03-27"
in.file <- paste("All_Locations_", exten, ".csv", sep="")
in.file <-"1 West Han Routes.csv"
out.path <- paste("paths_",exten,sep="")

#Functions
scaleBB <- function(box){
  # box <- bbox(site.points[1:2,])
  # box <- bbox(site.points[x:x+1,])
  ew <- (box["E","max"] -box["E","min"]) / 4
  ns <- (box["N","max"] -box["N","min"]) / 4
  if (ns < 1000){
    ns <- ew
  }
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



#Variables
latlong <- "+init=epsg:4326" #This is the espg code for the WGS 1984 geographic projection

#open the raster 
dem <- raster("chgis_dem.tif")   #load the raster: Make sure the auxillary files are present

dem[dem == -32768] <- NA #Filtering out NA Values
if (!file.exists("chgis_dem_low.tif")){
  print("no.file")
  dem.low <-aggregate(dem, fact =  5)
  writeRaster(dem.low,"chgis_dem_low.tif")
}else{
  dem.low <- raster("chgis_dem_low.tif")
}

proj4string(dem) #verify that this is non-NA

#Open the list of points
site.points.all <- read.csv(in.file)  #Read the table with Lat and lon
# site.points.table <- site.points
coordinates(site.points.all) <- ~E + N             #Define the coordinates to convert it to a spatial points data frame
proj4string(site.points.all) <- CRS(latlong)  
site.points.all <- spTransform(site.points.all, CRS(proj4string(dem)))
(proj4string(site.points.all) == proj4string(dem))



all.path.descriptions <- unique(site.points.all$Desc)
plot(dem.low)
xxx<-0
first.file <- TRUE
for (each.path in all.path.descriptions){
  print(each.path)
  xxx<-xxx+1
  out.name <- paste("Route_",as.numeric(all.path.descriptions[all.path.descriptions==each.path]),sep="")
  # out.name2 <- paste("RouteSimp_",as.numeric(all.path.descriptions[all.path.descriptions==each.path]),sep="")
  site.points <- site.points.all[(site.points.all$Desc == each.path),]
  if (file.exists(paste(out.path,"/",out.name,".shp",sep=""))){
    print("ouput already exists")
    all <- readOGR(dsn=out.path, layer = out.name)
    #next()
  }else{
    # print(site.points)
    first <- T
    for (x in 1:(nrow(site.points)-1)){
      print(paste(x,x+1))
      clip_dem <- crop(dem,scaleBB(bbox(site.points[x:(x+1),])))
      Conductance <- calculateConductance(clip_dem)
      each.line <- shortestPath(Conductance, site.points@coords[x,], site.points@coords[x+1,], output="SpatialLines")
      if (first){
        all <- each.line
        first <- FALSE
      }else{
        all <- all + each.line
      }#end else
    }# end interior for loop
    all <- gLineMerge(all)
    # all2 <- gSimplify(all, 500, topologyPreserve=FALSE)
    
    all.df <- SpatialLinesDataFrame(all, data.frame(Name = each.path))
    # all.df2 <- SpatialLinesDataFrame(all2, data.frame(Name = each.path))
    # all.df <- SpatialLinesDataFrame(all, data.frame(ID = c(1:length(all))))
    writeOGR(obj = all.df, dsn= out.path, layer = out.name, driver="ESRI Shapefile",overwrite_layer=T) #writes the spatial points data frame as a shapefile
    # writeOGR(obj = all.df2, dsn= out.path, layer = out.name2, driver="ESRI Shapefile",overwrite_layer=T) #writes the spatial points data frame as a shapefile
    
  }#end else if file exists
  plot(site.points, add=T)
  plot(all, add=TRUE)
  # if (first.file){
  #   all.in.one <-all.df
  #   first.file <- FALSE
  # }else{
  #   all.in.one <- rbind(all.in.one,all.df,make.row.names = TRUE)
  # }
  
}#end exterior for loop



#Adding a china boundry
china.outline<-getData("GADM",country="China",level=0)
# writeOGR(obj=china.outline,dsn=".",layer="china_outline",driver="ESRI Shapefile",overwrite_layer=T)
plot(spTransform(china.outline, CRS(proj4string(dem))), col = "transparent", add=T)




