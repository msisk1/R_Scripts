rm(list=ls(all=TRUE)) # clear memory


library(leaflet)
library(sf)
library(tidyverse)
library(readODS)

setwd("E:\\GISWork_2\\Copic_HomelessMap")

loc.file  <- "Poverty Studies - Homeless Encampment.kml"
layers <- st_layers(loc.file)

color.for.points <- "#b41f1f"
color.for.path <- "#340909"
cluster.code <- paste("function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:",color.for.points, "\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }", sep="")



locations.all <- st_read(loc.file, layer = layers$name[1], stringsAsFactors = F)%>%
  st_zm()

descriptions <- read_ods("Poverty Studies Map Points.ods")

locations.all <- locations.all %>%
  full_join(descriptions, by = c("Name" = "Point"))

locations.all$popup <- paste("<b>", locations.all$Name, "</b><br>",
                             "<hr>",
                             # ifelse(is.na(locations.all$`Stakeholder Name on Map`),"", paste0(locations.all$`Stakeholder Name on Map`,"<br>")),
                             ifelse(is.na(locations.all$Text),"", paste0(locations.all$Text,"<br>")),
                             # "<img src =",locations.all$Thumbnail, "><br>",
                             ifelse(is.na(locations.all$`Youtube Embed`),"", paste0("<br>",locations.all$`Youtube Embed`,"<br>")),
                             
                             sep = ""
                             
                             
                             )





borders <- locations.all %>%
  filter(st_geometry_type(locations.all) == "MULTIPOLYGON")

locations <- locations.all %>%
  filter(st_geometry_type(locations.all) == "POINT")



walk <- st_read(loc.file, layer = layers$name[2])
walk.point <- walk %>%
  filter(st_geometry_type(walk)=="POINT")
walk.route <- walk %>%
  filter(st_geometry_type(walk)=="LINESTRING") %>%
  st_zm()



walks.name <- "Walking Route"
loc.name <- "Locations"


m <- leaflet()%>%
  addProviderTiles(provider = providers$Stamen.Toner)%>%
  addCircleMarkers(data = locations, group = loc.name, popup =~popup, color = color.for.points, 
                   fillColor = color.for.points, fillOpacity = 1,
                   clusterOptions = markerClusterOptions(freezeAtZoom=15,
                                                         iconCreateFunction =JS(cluster.code)
                                                         
                                                         ),
                   popupOptions = popupOptions(maxWidth = 560))%>%
  addPolygons(data = borders, group = loc.name, color = color.for.points, fillColor = color.for.points, 
              fillOpacity = .2, popup =~popup, popupOptions = popupOptions(maxWidth = 560))%>%
  addPolylines(data = walk.route, group = walks.name, color = color.for.path, opacity = 1, popup = ~Name)%>%
  addCircleMarkers(data = walk.point, group = walks.name, color = color.for.path, fillColor = color.for.path,
                   fillOpacity = 1,
                   popup = ~Name) %>%
  addLayersControl(overlayGroups = c(loc.name, walks.name),
                   options = layersControlOptions(collapsed = FALSE))%>%
  hideGroup(walks.name)
m

library(htmlwidgets)
saveWidget(m, file="Map_v01.html")
saveWidget(m, file="E:\\GIT_Checkouts\\pages\\msisk1.github.io\\Map_v01.html")




