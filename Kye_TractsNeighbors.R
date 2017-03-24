#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("rgdal","sf","rgeos","foreign","spdep") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

setwd("E:\\GISWork_2\\Kye_Tracts_ZipCodes\\2017-03-08_GeocodingToTract")

latlong <- "+init=epsg:4326"
google <- "+init=epsg:3857"
# Five.miles.in.meters <- 8046.72

useful.tracts <- read.csv("whgentrlist.csv", stringsAsFactors = F)

if(FALSE){
        tracts_poly <- readOGR(dsn = "..", layer = "US_tract_2000", stringsAsFactors = F)
        tracts_poly$tractid <- paste(as.numeric(tracts_poly$NHGISST)/10, substring(tracts_poly$NHGISCTY,1,3), substring(tracts_poly$GISJOIN,9), sep="")
        
        
        tracts_poly <- merge(tracts_poly, useful.tracts, by="tractid", all.x= T)
        tracts_smaller <- tracts_poly[!is.na(tracts_poly$city),]
} #commented out because once done, it is not necessary any more


# tracts_larger <- gIntersection(tracts_poly,tracts_smaller) #This never worked well for me, so I did it in ArcGIS, let me know if you need the reduced file
tracts_larger <- readOGR(dsn=".",layer = "US_tract2000_WithUsefulAndNeighbors",stringsAsFactors = F)
tracts_larger$tractid <- paste(as.numeric(tracts_larger$NHGISST)/10, substring(tracts_larger$NHGISCTY,1,3), substring(tracts_larger$GISJOIN,9), sep="") #creating the proper tractid field

points <-  read.csv("SingeTracts_Fixed.csv", stringsAsFactors = F)
coordinates(points) <- ~X + Y             #Define the coordinates to convert it to a spatial points data frame
proj4string(points) <- CRS(latlong)  #giving it a coordinate system
points <- spTransform(points,CRS(proj4string(tracts_larger))) #reprojecting it so the coordinate system matches

##Within Tracts
points@data$tractid <- over(points, tracts_larger)$tractid  #appending the tractid
points <- points[points$Addr_type %in% c("PointAddress","StreetAddress") ,] #tossing values other than street or point addresses

schools.in.tract <- aggregate(EPIN ~ tractid, data = points@data, paste, collapse = " ") #Generating list  of schools in each tract
names (schools.in.tract) <- c("tractid","epin_in") #changing names
tots <- data.frame(table(points$tractid)) #genrating number of schools in each tract
names(tots) <- c("tractid","freq_in") #changing names
schools.in.tract <- merge(schools.in.tract,tots,by="tractid") #adding it together

#Within neighbors
tracts_larger$nb <- poly2nb(tracts_larger) #Generating a list of neighbors for each tract

tracts.df <- tracts_larger@data #copying the data from a spatial data frame to a regular data frame
tracts.df$rows <- as.character(as.integer(row.names(tracts.df)) + 1) #No idea really why the +1 is necessary, maybe because of one without neighbors?
tie.table <- tracts.df[,c("tractid","rows")] #isolating the stuff needed
names(tie.table) <- c("tractid_nb","rowid") #renaming fields

final.duped <- data.frame(rowid= unlist(tracts_larger$nb), tracts_larger[ rep(1:nrow(tracts_larger), sapply(tracts_larger$nb, length)) ,  ] ) #duplicating records based on neighbors
final.duped <- merge(final.duped,tie.table,by="rowid", all.x=T) # adding the tractid for neighbors back on: formerly it was just a rowID
neighbor.table <- final.duped[,c(2,13)] #dropping most fields
neighbor.table <- merge(neighbor.table,schools.in.tract,by.x = "tractid_nb", by.y = "tractid", all.x = T)

schools.in.neighbors <- aggregate(epin_in ~ tractid, data = neighbor.table, paste, collapse = " ")
names (schools.in.neighbors) <- c("tractid","epin_nb") #changing names

tots2 <- aggregate(freq_in ~ tractid, data = neighbor.table, FUN = sum)
names(tots2) <- c("tractid","freq_nb") #changing names
schools.in.neighbors <- merge(schools.in.neighbors,tots2,by="tractid") #adding it together
all.schools <- merge(schools.in.tract,schools.in.neighbors,by="tractid", all=T)
all.schools$freq_in[is.na(all.schools$freq_in)] <- 0
all.schools$freq_nb[is.na(all.schools$freq_nb)] <- 0
all.schools$epin_in[is.na(all.schools$epin_in)] <- ""
all.schools$epin_nb[is.na(all.schools$epin_nb)] <- ""


all.schools$freq_nb <- all.schools$freq_in + all.schools$freq_nb
all.schools$epin_nb <- paste(all.schools$epin_in, all.schools$epin_nb, sep=" ")

for.export <- merge (useful.tracts, all.schools, by = "tractid",all.x = T) #merging onto the tracts needed, will toss those from just neighbors
write.csv(for.export, "Schools_in_tractAndNeighbors.csv", row.names = F)


#FIXING the bad EPINs earlier.
# load("E:/GISWork_2/Kye_Tracts_ZipCodes/2017-03-08_GeocodingToTract/school9192.rdata")
# coded <- read.csv("2017-03-08_GeocodingToTract\\SingleTracts.txt",stringsAsFactors = F)
# school$Field1 <- row.names(school)
# school2 <- school[,c(8,7)]
# coded$EPIN <- NULL
# coded2 <- merge(coded,school2,by="Field1",all.x =T)
# coded2 <- coded2[,c("Addr_type", "X", "Y", "Field1", "ISR","EINST",  "EADDRS", "ECITY", "ESTABB", "EZIP", "EPIN", "NHGISST", "NHGISCTY" ,"GISJOIN", "GISJOIN2")]
# write.csv(coded2,"SingeTracts_Fixed.csv", row.names = F)
