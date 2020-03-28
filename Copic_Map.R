rm(list=ls(all=TRUE)) # clear memory


library(leaflet)
library(sf)
library(tidyverse)
library(readODS)
library(googlesheets4)


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



# descriptions <- read_ods("Poverty Studies Map Points.ods")
descriptions <- read_sheet("https://docs.google.com/spreadsheets/d/1qyaIRIc_PQXw4VNke2rOTwstqICKH7IVnVbp99uqCZ4/edit?ts=5e4b08d7#gid=0")


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


locations.all$shelter <- if_else(grepl("Shelter", locations.all$`Categories (filters)`),1,0)
locations.all$media <- if_else(grepl("Media", locations.all$`Categories (filters)`),1,0)
locations.all$sp <- if_else(grepl("Service", locations.all$`Categories (filters)`),1,0)
locations.all$Advocate <- if_else(grepl("Advocate", locations.all$`Categories (filters)`),1,0)
locations.all$Resident <- if_else(grepl("Resident", locations.all$`Categories (filters)`),1,0)
locations.all$Business <- if_else(grepl("Business", locations.all$`Categories (filters)`),1,0)


# locations <- st_centroid(locations.all)


borders <- locations.all %>%
  filter(st_geometry_type(locations.all) == "MULTIPOLYGON")

locations <- locations.all %>%
  filter(st_geometry_type(locations.all) == "POINT")
# locations <- st_centroid(locations.all)
locations <- rbind(locations, st_centroid(borders))


locations.c <-locations%>%
  group_by(Name)%>%
  summarise(n = n()) %>%
  st_set_geometry(NULL)
locations <- left_join(locations, locations.c, "Name")




locations <- rbind(locations%>%filter(n==1),locations%>%
                filter(n>1)%>%
                st_jitter(factor = .02))

# 

walk <- st_read(loc.file, layer = layers$name[2])
walk.point <- walk %>%
  filter(st_geometry_type(walk)=="POINT")
walk.route <- walk %>%
  filter(st_geometry_type(walk)=="LINESTRING") %>%
  st_zm()



walks.name <- "Walking Route"
loc.name <- "Locations"


m <- leaflet()%>%
  addProviderTiles(provider = providers$Stamen.TonerLite)%>%
  addCircleMarkers(data = st_jitter(locations), group = loc.name, popup =~popup, color = color.for.points, 
                   fillColor = color.for.points, fillOpacity = 1,
                   clusterOptions = markerClusterOptions(freezeAtZoom=18,
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



icon.shelter <- makeAwesomeIcon(icon = "bed", markerColor = color.for.points, library = "fa",iconColor = "black")
icon.media <- makeAwesomeIcon(icon = "microphone", markerColor = color.for.points, library = "fa",iconColor = "black")
icon.sp <- makeAwesomeIcon(icon = "building", markerColor = color.for.points, library = "fa",iconColor = "black")
icon.Advocate <- makeAwesomeIcon(icon = "gavel", markerColor = color.for.points, library = "fa",iconColor = "black")
icon.Resident <- makeAwesomeIcon(icon = "home", markerColor = color.for.points, library = "fa",iconColor = "black")
icon.Business <- makeAwesomeIcon(icon = "briefcase", markerColor = color.for.points, library = "fa",iconColor = "black")



hacked.name <- paste0(walks.name,"<br><hr><b>",loc.name,":</b>")

m2 <- leaflet()%>%
  addProviderTiles(provider = providers$Stamen.TonerLite)%>%
  # addCircleMarkers(data = st_jitter(locations), group = loc.name, popup =~popup, color = color.for.points, 
  #                  fillColor = color.for.points, fillOpacity = 1,
  #                  clusterOptions = markerClusterOptions(freezeAtZoom=18,
  #                                                        iconCreateFunction =JS(cluster.code)
  #                                                        
  #                  ),
  #                  popupOptions = popupOptions(maxWidth = 560))%>%
  addAwesomeMarkers(data = locations%>%filter(shelter > 0), icon = icon.shelter, group = "Shelter", popup =~popup,
                                   popupOptions = popupOptions(maxWidth = 560))%>%
  addAwesomeMarkers(data = locations%>%filter(media > 0), icon = icon.media, group = "Media", popup =~popup,
                    popupOptions = popupOptions(maxWidth = 560))%>%
  addAwesomeMarkers(data = locations%>%filter(sp > 0), icon = icon.sp, group = "Service Provider", popup =~popup,
                    popupOptions = popupOptions(maxWidth = 560))%>%
  addAwesomeMarkers(data = locations%>%filter(Resident > 0), icon = icon.Resident, group = "Resident", popup =~popup,
                    popupOptions = popupOptions(maxWidth = 560))%>%
  addAwesomeMarkers(data = locations%>%filter(Advocate > 0), icon = icon.Advocate, group = "Advocate", popup =~popup,
                    popupOptions = popupOptions(maxWidth = 560))%>%
  addAwesomeMarkers(data = locations%>%filter(Business > 0), icon = icon.Business, group = "Business", popup =~popup,
                    popupOptions = popupOptions(maxWidth = 560))%>%
  
    addPolygons(data = borders, group = "Renaissance District", color = color.for.points, fillColor = color.for.points, 
              fillOpacity = .2)%>%
  addPolylines(data = walk.route, group = hacked.name, color = color.for.path, opacity = 1, popup = ~Name)%>%
  addCircleMarkers(data = walk.point, group = hacked.name, color = color.for.path, fillColor = color.for.path,
                   fillOpacity = 1,
                   popup = ~Name) %>%
  addLayersControl(overlayGroups = c(hacked.name,"Renaissance District","Shelter","Media","Service Provider","Resident","Advocate","Business"),
                   options = layersControlOptions(collapsed = FALSE))%>%
  hideGroup(hacked.name)
m2




library(htmlwidgets)
saveWidget(m, file="Map_v01.html")
saveWidget(m, file="E:\\GIT_Checkouts\\pages\\msisk1.github.io\\Map_v01.html")
saveWidget(m2, file="E:\\GIT_Checkouts\\pages\\msisk1.github.io\\Map_v02.html")


save(borders,descriptions,layers,locations,locations.all, walk, walk.point,walk.route, file = "Homelessmap\\homelessMap.rdata")


