
library(rgdal)
library(ggmap)


register_google(key = "AIzaSyBoxrcK9_pF4WtocZt4eSqrte8XVz_dpMk")
a <- geocode("Hesburgh Library, Notre Dame IN", output= "more")

ec.data <- read.csv("C:\\Temp\\Emannuel\\AddressList.csv", stringsAsFactors = F)
ec.data$FullAdd <- paste(ec.data$Street.., ec.data$N.W, ec.data$Street.Name,ec.data$Street.Type, ec.data$City, ",", ec.data$State, sep = " ")

length(unique(ec.data$FullAdd))
geocoded.list <- geocode((ec.data$FullAdd), output = "more")
geocoded.list <- mutate_geocode(data = ec.data, location = FullAdd, output = "more")
ec.data$l

for(i in 1:nrow(ec.data)){
  result <- geocode((ec.data$FullAdd[i]), output = "latlona", source = "google")
  while(is.na(result[1])){ #checks if the latitude is NA and reruns if it is
    Sys.sleep(2) #Pauses for a minute to let the API Catch up
    result <- geocode((ec.data$FullAdd[i]), output = "latlona", source = "google")
  } 
  ec.data$lon[i] <- as.numeric(result[1])
  ec.data$lat[i] <- as.numeric(result[2])
  ec.data$geoAddress[i] <- as.character(result[3])
}
write.csv(ec.data, "test.csv")
ec.data <- read.csv("c:\\Temp\\Emannuel\\test.csv", stringsAsFactors = F)
