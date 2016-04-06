#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("maptools","rgdal","leaflet","raster") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

setwd("E:\\GISWork_2\\Ferri_ImaginaryPlaces2\\Map\\")

latlong <- "+init=epsg:4326"
google <- "+init=epsg:3857"

# image <- stack("First_Trial_CopyRaster.tif") #stack instead of raster for multiband image
image <- stack("Mappa_piccola_rect.tif") #stack instead of raster for multiband image
projection(image) <- google
writeRaster(image,"Mappa_piccola_rect_Crs.tif",overwrite=TRUE)



crs(image) <- sp::CRS(google)
qq <- leaflet() %>%
        #setView(lng = 12.5, lat = 41.9, zoom = 12) %>% 
#         addRasterImage(image) %>% 
        addTiles(group = "OpenStreetMap") %>%  # Add default OpenStreetMap map tiles
        addTiles(
                                 "http://www.nd.edu/~msisk1/testing/testing/rect/{z}/{x}/{y}.png",
#                 "Letarouilly_LoRes/{z}/{x}/{y}.png",
                
                options = tileOptions(tms=TRUE),
                group = "Custom"
        ) 
#         addLayersControl(
#                 #                 baseGroups = c("OpenStreetMap","Custom"),
#                 overlayGroups = c("OpenStreetMap","Custom"),
#                 options = layersControlOptions(collapsed = FALSE)
#         )
qq

addRasterImage(r, colors = pal, opacity = 0.8)