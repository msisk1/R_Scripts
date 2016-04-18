rm(list=ls(all=TRUE)) # clear memory

packages<- c("ggmap","sp","taRifx.geo", "SpatialTools","plyr","rgdal") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd("/mnt/smb/Research/OTool_Distances")
setwd("N:\\Research\\OTool_Distances")
#gmaps.key <- "AIzaSyDZy7HLVrouFTsULZ6D6ZyGub8iseJI_OU" #Unneeded!


IntersectPtWithPoly <- function(x, y) { 
        # Extracts values from a SpatialPolygonDataFrame with SpatialPointsDataFrame, and appends table (similar to 
        # ArcGIS intersect) 
        # Args: 
        #   x: SpatialPoints*Frame 
        #   y: SpatialPolygonsDataFrame 
        # Returns: 
        # SpatialPointsDataFrame with appended table of polygon attributes 
        
        # Set up overlay with new column of join IDs in x 
        z <- over(x, y) 
        
        # Bind captured data to points dataframe 
        x2 <- cbind(x, z) 
        
        # Make it back into a SpatialPointsDataFrame 
        # Account for different coordinate variable names 
        if(("coords.x1" %in% colnames(x2)) & ("coords.x2" %in% colnames(x2))) { 
                coordinates(x2) <- ~coords.x1 + coords.x2 
        } else if(("x" %in% colnames(x2)) & ("x" %in% colnames(x2))) { 
                coordinates(x2) <- ~x + y 
        } #end elseif 
        
        #    # Reassign its projection if it has one 
        #         if(is.na(CRSargs(x@proj4string)) == "FALSE") { 
        #            x2@proj4string <- x@proj4string 
        #         } #end if 
        return(x2) 
} #End IntersectPtWithPoly

CSv.to.Shapefile <- function(df,x.coord, y.coord,projection, out.name = NULL, save = FALSE){
        coordinates(df)=df[c(x.coord,y.coord)]
        proj4string(df)=CRS(projection) # set it to lat/long
        if (save){
                writeOGR(df, dsn="." ,layer=out.name,driver="ESRI Shapefile")        
        }
        return (df)
}#end CSV.to.Shapefile


all.respondants <- read.csv("all_participants_xy.csv")
unique.respondants <- unique(all.respondants[c(2,3)])
unique.respondants <- unique.respondants[complete.cases(unique.respondants),] #removing case #98797 which is NA for both coordinates

remove(all.respondants)


#unique.respondants <-unique.respondants[1:50,] #SAMPLE IT DOWN TO 50 FOR TESTINF
blocks.spdf <- readOGR(dsn = "GISData", layer = "Chicago_Blocks_Project") # you load a shapefile with the directory in dsn and the name of the file minus .shp as the layer
proj <- proj4string(blocks.spdf)
respon.spdf <- SpatialPoints(coords = unique.respondants, proj4string=CRS(proj))




test<- IntersectPtWithPoly(respon.spdf,blocks.spdf)

 #gives the projection information


