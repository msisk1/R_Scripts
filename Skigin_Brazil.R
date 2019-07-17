rm(list=ls(all=TRUE)) # clear memory

library(rgdal)
library(raster)
library(dplyr)
library(maptools)


setwd("E:\\GISWork_2\\Skigin_Brazil")

brazil.admin2 <- readOGR(dsn="Admin", layer = "BRA_adm2", encoding = "ISO-8859") #This opens the boundary file for the departments of Brazil, a shapefile


all <- 1979:2013 #years of interest
all.means <- stack() #Creates a blank raster stack to hold the climate data
save.clips <- TRUE #Just a binary about what to save
# tpye <- "Precip" #either "Precip" or "Temp
tpye <- "Temp" #either "Precip" or "Temp


for (each.year in all){
  print(each.year)
  pat <- paste("^.*",each.year,".*.tif$", sep = "")
  climate.files2 <- list.files(paste("CHELSA\\",tpye,"\\World",sep = ""), pattern=pat, full.names=TRUE) #Loading all of the Precipitation Files
  if (length(climate.files2) == 12){ #Just a check to make sure all of the files were downloaded properly
    print("    Loading")
    all.climate2 <- stack(climate.files2) #Creating a stack of all of them, makes it much easier to calculate means and clipe
    print("    Clipping")
    brazil.climate.year <- crop(all.climate2, brazil.admin2) #Cut down the data for the whole world to just that for Brazil
    if (save.clips){
      print("    Saving clips")
      writeRaster(brazil.climate.year, filename=file.path(paste("CHELSA\\",tpye,"\\Brazil",sep = ""),names(brazil.climate.year)), bylayer=TRUE,format="GTiff") 
      #Saving it this way saves each layer as a seprate file
    }
    print("     Calculating mean")
    each.mean <- mean(brazil.climate.year) #Calcualtes the mean for each pixel of the year
    names(each.mean) <- paste(tpye,"_",each.year,sep = "") #Renames that mean so that it contains the variable and the year
    all.means <- addLayer(all.means,each.mean) #Adds it into the stack of all means. This will make it easier to sample all years later
    print("   All Good")
  }else{
    print(length(climate.files2))
  } #end else
      
}# end for loop


writeRaster(all.means, filename=file.path(paste("CHELSA\\",tpye,"\\Means",sep = ""),names(all.means)), bylayer=TRUE,format="GTiff")  #writing out all of the mean years

only.points <- rasterToPoints(all.means,spatial = T) #converting the raster stack to a series of points
#it seems counterintiative, but this runs a lot faster than as a raster stack
ord <- over(only.points,brazil.admin2) #Figuring out which deparment every point is in
union.table <- cbind(only.points@data, as.data.frame(ord)) # rejoining the data back on
union.table <- union.table[,c(1:35,41,42)] #Dropping unnecessary fields

#Creating final summary table
summary <- union.table %>%
  filter(!is.na(NAME_2)) %>%
  group_by(ID_2,NAME_2)%>%
  add_tally() %>% 
  summarise_all(funs(mean))
write.csv(summary, paste(tpye,"_summary.csv",sep=""), row.names = F)

