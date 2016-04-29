#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("maptools","rgdal","leaflet","htmlwidgets") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("E:\\GISWork_2\\HUEND\\SouthBend\\WebApplication\\")
#out.location <- "N:\\www\\testing\\"
active.sections <- c("West Washington","Downtown","Chapin Park","West North Shore")

#Loading and subsetting historic district data
historic.districts <- readOGR(dsn = ".", layer = "Historic_Districts_LL")
proj4string(historic.districts) <- CRS("+init=epsg:3857")  #I have no idea why this is required.
historic.districts<- spTransform(historic.districts, CRS("+init=epsg:3857"))
sub.districts<- historic.districts[historic.districts$District_N %in% active.sections,]

#adding label locations to historic districts
label.locs <- read.csv("District_Label_locs.csv")
label.locs$lab_both <- paste("[",label.locs$lab_Lat,",",label.locs$lab_Lon,"]",sep = "")

historic.districts <- merge(historic.districts,label.locs,by="District_N")



#Loading and subsetting building data
buildings.table <- read.csv("Historic-District-Structures_ALL.csv")
coordinates(buildings.table)=~Long+Lat
proj4string(buildings.table) <- CRS("+init=epsg:3857")
sub.buildings <- buildings.table[sub.districts, ]

historic.districts@data$used <- 0
historic.districts@data$used[historic.districts@data$District_N %in% active.sections] <- 1


#writeOGR(sub.districts, dsn="sub.districts.kml", layer= "sub.districts", driver="KML", dataset_options=c("NameField=name"))


writeOGR(historic.districts, "test_geojson", layer="historic.districts", driver="GeoJSON")
text.jspn <- readChar("test_geojson", file.info("test_geojson")$size)
text.jspn2 <- paste("var districts = ",text.jspn,";", sep="") 
write(text.jspn2, "again.js")






# 
# #Creating the leaflet instance
# 
# m <- leaflet() %>%
#         addProviderTiles("Esri.WorldTopoMap", group = "ESRI World Topo") %>%
# #         addTiles(group = "OpenStreetMap") %>%  # Add default OpenStreetMap map tiles
# # #         addMarkers(data=sub.buildings, group = "Locations") %>%
# #         addCircleMarkers(data=sub.buildings,
# #                          radius = 3,
# #                          color = "green",
# #                          stroke = FALSE, fillOpacity = 1, group = "Locations") %>%
#         addPolygons(data=sub.districts, group = "Historic Disctricts")
# # %>%
# #         addLayersControl(
# #                 baseGroups = c("ESRI World Topo","OpenStreetMap","Letarouilly"),
# #                 overlayGroups = c("Locations", "Historic Disctricts"),
# #                 options = layersControlOptions(collapsed = FALSE)
# #         )
# m
# 

# saveWidget(widget = m, file=paste("","your_map.html",sep=""), selfcontained = FALSE)