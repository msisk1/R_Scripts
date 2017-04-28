
#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("rgdal","sp","SpatialTools","Imap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("E:\\GISWork_2\\Martin_Distances") 

# distances <- readOGR(dsn=".",layer = "municipal_capitals_elevation")



ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
        # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
        # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
        
        if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
        if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
        else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
        else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
        m[tri] <- t(m)[tri]
        return(m)
}

GeoDistanceInMetresMatrix <- function(df.geopoints){
        # Returns a matrix (M) of distances between geographic points.
        # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
        # (df.geopoints$lat[j], df.geopoints$lon[j]).
        # The row and column names are given by df.geopoints$name.
        
        GeoDistanceInMetres <- function(g1, g2){
                # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
                # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
                # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
                # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
                # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
                DistM <- function(g1, g2){
                        require("Imap")
                        return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="m")))
                }
                return(mapply(DistM, g1, g2))
        }
        
        n.geopoints <- nrow(df.geopoints)
        
        # The index column is used to ensure we only do calculations for the upper triangle of points
        df.geopoints$index <- 1:n.geopoints
        
        # Create a list of lists
        list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
        
        # Get a matrix of distances (in metres)
        mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
        
        # Set the row and column names
        rownames(mat.distances) <- df.geopoints$name
        colnames(mat.distances) <- df.geopoints$name
        
        return(mat.distances)
}


cities <- read.csv("municipal_capitals_proc.csv", stringsAsFactors = F)
redo.list <- TRUE
if (redo.list){
        cities.redos <- read.csv("municipal_capitals_redos_Geocoded.csv", stringsAsFactors = F)
        cities.redos <- cities.redos[,1:3]
        names(cities.redos)<- c("x_o","y_o","Origmun")
        cities.redos$new <- 1
        cities3 <- merge(cities,cities.redos,by="Origmun",all.x=T)
        cities3$new[which(is.na(cities3$new))] <- 0
        cities <- cities3
        cities$X[((cities$new==1))]<-cities$x_o
        cities$X[cities$new==1] <- cities$x_o
        cities$X <- ifelse(cities$new==1,cities$x_o,cities$X)
        cities$Y <- ifelse(cities$new==1,cities$y_o,cities$Y)
        
}


cities.code <- cities[,c(2,3,1)]
names(cities.code) <- c("lon","lat","name")
cities.code.matrix <- data.frame(GeoDistanceInMetresMatrix(cities.code))


# n2  <- sapply(cities.code.matrix, as.integer)
write.csv(cities.code.matrix, "euclidean_distance_matrix_code.csv")

# n2 <- as.integer(cities.code.matrix)


write.csv(sapply(cities.code.matrix, as.integer), "Updated_euclidean_distance_matrix_code.csv")




cities.name <- cities[,c(1,2,4)]
names(cities.name) <- c("lon","lat","name")


cities.name.matrix <- data.frame(GeoDistanceInMetresMatrix(cities.name))



library(reshape2)
path.distance.file <- read.csv("Path_distance_list_code.csv", stringsAsFactors = F)
path.distance.file$ObjectID <- NULL
path.distance.file$OriginID <- colsplit(path.distance.file$Name," - ", c("Origin","Diestination"))[,1]
path.distance.file$DestinationID <- colsplit(path.distance.file$Name," - ", c("Origin","Diestination"))[,2]
write.csv(path.distance.file,"Path_distance_list_code.csv", row.names = F)
