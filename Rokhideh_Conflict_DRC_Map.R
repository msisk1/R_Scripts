rm(list=ls(all=TRUE))

#Todo:
  #Change the Title to " Violent Events: 2016 to present"
  #Label the  (DR Congo, Rwanda, Uganda, Burundi, Tanzania, Zambia, and Angola) 
  #have a bolder line outlining the Great Lakes countries, which are DR Congo, Rwanda, Ugandan, and Burundi? 


library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library("sf")
latlong <- 4326

setwd("/home/matthew/Documents/GoogleSync/GIS_Work2/ Rokhideh_Conflict_DRC")
all.africa.conflict <- read_csv("Africa_1997-2020_Mar28-1.csv", col_types = 
                                  cols(
                                    .default = col_character(),
                                    ISO = col_double(),
                                    EVENT_ID_NO_CNTY = col_double(),
                                    YEAR = col_double(),
                                    TIME_PRECISION = col_double(),
                                    INTER1 = col_double(),
                                    INTER2 = col_double(),
                                    INTERACTION = col_double(),
                                    ADMIN3 = col_character(),
                                    LATITUDE = col_double(),
                                    LONGITUDE = col_double(),
                                    GEO_PRECISION = col_double(),
                                    FATALITIES = col_double(),
                                    TIMESTAMP = col_double()
                                  )
                                 )

#Filter out 2016-2020
all.africa.conflict <- all.africa.conflict %>%
  filter(YEAR >= 2016, LATITUDE < 8) %>% 
  sf::st_as_sf(coords = c("LONGITUDE","LATITUDE")) %>% 
  sf::st_set_crs(latlong) 
#download country borders
africa <- ne_countries(scale = "medium", returnclass = "sf", continent = "Africa")
great.lake.countries <- africa%>%
  filter(sov_a3 %in% c("BDI","COD","RWA","UGA"))%>%
  st_union()
plot(great.lake.countries)
#create map
pal <- c("orange", "blue", "green","yellow","black","red")

#creating anchors for labels
# label.points<- st_centroid(africa)
# label.points <- cbind(africa, st_coordinates(st_centroid(africa$geometry)))%>%
#     filter(sov_a3 %in% c("BDI","COD","RWA","UGA","TZA","ZMB","AGO"))
# st_write(label.points, "label_points.shp")
label.points <- st_read("label_manual.shp", stringsAsFactors = F)
label.points[label.points$sov_a3 == "COD",]$geounit <- paste("Democratic Republic", "of the Congo", sep = "\n")

#testing
ggplot(data = africa) +
  geom_sf() +
  geom_sf_text(data= label.points,aes(x=X, y=Y, label=geounit),
            color = "black", check_overlap = F) +
  coord_sf(xlim = c(12, 40), ylim = c(-13, 6), expand = FALSE)

#end testing


year.cutoff <- 2016
p.2016.col.size <- ggplot()+
  geom_sf(data = africa)+
  geom_sf(data = all.africa.conflict%>%filter(YEAR >= year.cutoff), aes(color = EVENT_TYPE, size = FATALITIES), show.legend = 'point')+
  scale_colour_manual(values = pal)+
  scale_size(range = c(1,10))+
  geom_sf(data = africa,color = "black", fill = NA,)+
  geom_sf(data = great.lake.countries, color = "black", fill = NA, size = 1.5)+
  geom_sf_text(data= label.points,aes(x=X, y=Y, label=geounit),
               color = "black", check_overlap = F, fontface = "italic") +
  coord_sf(xlim = c(12, 40), ylim = c(-13.5, 6), expand = FALSE)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  ggtitle(paste0("Violent Events: ", year.cutoff, " to present"))
p.2016.col.size
ggsave("2016_Color_size_v3.png",width = 10, height =6, plot = p.2016.col.size)

year.cutoff <- 2020
p.2020.col.size <- ggplot()+
  geom_sf(data = africa)+
  geom_sf(data = all.africa.conflict%>%filter(YEAR >= year.cutoff), aes(color = EVENT_TYPE, size = FATALITIES), show.legend = 'point')+
  scale_colour_manual(values = pal)+
  scale_size(range = c(1,10))+
  geom_sf(data = africa,color = "black", fill = NA,)+
  geom_sf(data = great.lake.countries, color = "black", fill = NA, size = 1.5)+
  geom_sf_text(data= label.points,aes(x=X, y=Y, label=geounit),
               color = "black", check_overlap = F, fontface = "italic") +
  coord_sf(xlim = c(12, 40), ylim = c(-13.5, 6), expand = FALSE)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  ggtitle(paste0("Violent Events: ", year.cutoff, " to present"))
p.2020.col.size
ggsave("2020_Color_size_v3.png",width = 10, height =6, plot = p.2020.col.size)

    # 
# p.2016.col <- ggplot()+
#   geom_sf(data = africa)+
#   geom_sf(data = all.africa.conflict%>%filter(YEAR >= year.cutoff), aes(color = EVENT_TYPE), show.legend = 'point')+
#   scale_colour_manual(values = pal)+
#   coord_sf(xlim = c(12, 40), ylim = c(-13, 6), expand = FALSE)+
#   annotation_scale(location = "bl", width_hint = 0.5) +
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
#                          style = north_arrow_fancy_orienteering) +
#   ggtitle(paste0("ACLED Conflicts: ", year.cutoff, " to present"))
# p.2016.col
# ggsave("2016_Color_only.png",width = 10, height =4, plot = p.2016.col)
# 
# all.africa.conflict$eventTime <- as.Date(all.africa.conflict$EVENT_DATE, format = "%d-%B-%Y")
library(rgdal)
writeOGR(dsn = ".", layer = "ACLED_Africa_2016_2020", obj = as_Spatial(all.africa.conflict), driver = "ESRI Shapefile", overwrite_layer = T)
# st_write(all.africa.conflict, "ACLED_Africa_2016_2020.shp", delete_dsn = T) #No ideal why this did not work
st_write(africa, "Africa.shp")


#creat leaflet / shiny object
library(leaflet)
leaflet()%>%
  addTiles()%>%
  addCircleMarkers(data=all.africa.conflict, size = 3)