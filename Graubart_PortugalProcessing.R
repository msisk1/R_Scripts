#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("dplyr","foreign","rgdal") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("E:\\GISWork_2\\Graubart_Lima\\Portugal\\2018_Rebuild") 

input.table <- read.csv("Vecinos final title without dupes.csv",stringsAsFactors=F)[,1:8]
name.to.number.table <- read.csv("Names_To_Numbers.csv",stringsAsFactors=F)
old.data <- read.csv("Old_Data.csv",stringsAsFactors = F)


input.table$collacion[input.table$collacion  == " "] <- ""

input.table<- input.table[(input.table$collacion != ""),]

input.table$collacion[input.table$collacion  == "santa maria"] <-"Santa Marina"
input.table$collacion[input.table$collacion  == "San Isidoro"] <-"San Isidoro"
input.table$collacion[input.table$collacion  == "Santa Maria Blanca"] <-"Santa Maria la Blanca"
input.table$collacion[input.table$collacion  == "Santa Maria la Blanca"] <-"Santa Maria la Blanca"
input.table$collacion[input.table$collacion  == "San isidoro"] <-"San Isidoro"
input.table$collacion[input.table$collacion  == "Santa Cruz"] <-"Santa Cruz"
input.table$collacion[input.table$collacion  == "San Ildefonso"] <-"San Ildefonso"
input.table$collacion[input.table$collacion  == "San Pedro"] <-"San Pedro"
input.table$collacion[input.table$collacion  == "San Andres"] <-"San Andres"           
input.table$collacion[input.table$collacion  == "San Juan"] <-"San Juan" 
input.table$collacion[input.table$collacion  == "San Marcos"] <-"San Marcos"
input.table$collacion[input.table$collacion  == "Santa Maria"] <-"Santa Maria la Blanca"
input.table$collacion[input.table$collacion  == "San Esteban"] <-"San Esteban"
input.table$collacion[input.table$collacion  == "Omnium Sanctorum"] <-"Omnium Sanctorum"	 
input.table$collacion[input.table$collacion  == "Santa Catalina"] <-"Santa Catalina"
input.table$collacion[input.table$collacion  == "San Roman"] <-"San Roman"
input.table$collacion[input.table$collacion  == "Santiago"] <-"Santiago"
input.table$collacion[input.table$collacion  == "San Nicolas"] <-"San Nicolas"
input.table$collacion[input.table$collacion  == "San Salvador"] <-"Salvadoe"     
input.table$collacion[input.table$collacion  == "San pedro"] <-"San Pedro"       
input.table$collacion[input.table$collacion  == "San Bartolome"] <-"San Bartolome Viejo"

#Fixing titles

input.table$Title[input.table$Title %in% c("dona","DoÃ±a","dona ","Dona ")] <- "Dona"
input.table$Title[input.table$Title  == "don "] <-"Don"
input.table$Title[input.table$Title  == "Mastre"] <-"Maestre"
input.table$Title[input.table$Title  == "Rabi "] <-"Rabi"



input.table %>%
  group_by(collacion) %>%
  summarise(tot  = n(),
            Status01 = sum(religion == "judio"& Title %in% c("Don","Dona")))

#Making New Tables
# 1: Jewish men and women called "don" or "Dona": Status titles
input.table$Status01 <- ifelse((input.table$religion == "judio" & input.table$Title %in% c("Don","Dona")),1,0)

# 2: Jewish men called "Rabi" or "cabildo" or "judio mayor" - not differentiated, just call this one "Jewish Aljama titles"
input.table$JewAlj02 <- ifelse((input.table$religion == "judio" & input.table$Title %in% c("Rabi","cabildo","judio mayor")),1,0)

# 3: Men with title "maestre" (could be Jewish or Muslim): Artisan titles
input.table$Artisan03 <- ifelse((input.table$Title == "Maestre"),1,0)

# 4: Muslim Men with title "alfaqui" or "alcalde:" Muslim aljama titles
input.table$MusAljama04  <- ifelse((input.table$religion == "moro" & input.table$Title %in% c("Alfaqui maestre", "Alcalde maestre")),1,0)


# Then I need two maps you already produced for me redone.  In these, I want to indicate density of population rather than simply religion (like you did in Lima maps). You can use the old database - if you need me to resend, just say the word.
old.data <- read.csv("Old_Data.csv",stringsAsFactors = F)
names(old.data) <- c( "id","Year","Title","Name","Name_2","religion","occup","collacion","specifics","notes","Number","Moro","coverso","judio")

# old.data$JewPre1483  <- ifelse((old.data$religion ==  "judio" & old.data$Year <= 1493),1,0)
#2020-01-14: Revising to include all jews and muslims before 1495
old.data$JewPre1483  <- ifelse((old.data$religion ==  "judio"),1,0)

# 6 Muslims before 1483, both indicating density
old.data$MusPre1483  <- ifelse((old.data$religion ==  "moro" & old.data$Year <= 1493),1,0)

# 7 Muslims after 1483 by density
old.data$MusPost1483  <- ifelse((old.data$religion ==  "moro" & old.data$Year > 1493),1,0)



oop <- old.data %>%
  group_by(collacion) %>%
  summarise(tot  = n(),
            JewPre1483 =  sum(religion == "judio"),
            MusPre1483 =  sum(religion == "moro" & Year <= 1493),
            MusPost1483 = sum(religion == "moro" & Year > 1493),
            MusTotal = sum(religion == "moro"))



proc.table <- merge(input.table, name.to.number.table, by.x = "collacion", by.y = "Name", all.x = T)

temp1 <- proc.table[,c(9:13)]
temp1 <-aggregate(. ~ Number, data=temp1, FUN=sum)

temp2 <- old.data[,c(11, 15:17)]
temp2 <-aggregate(. ~ Number, data=temp2, FUN=sum)

temp1 <- merge(temp1,temp2, by = "Number", all = T)

Vecinos.spdf <- readOGR(".", layer = "Vecinos_base")
hhop <- merge(Vecinos.spdf, temp1, by = "Number", all.x = T)
library(sf)
out.poly <- st_as_sf(hhop)
out.point <- st_centroid(out.poly)

st_write(out.poly, "2018_Rebuild_Vecinos.shp", delete_layer = T)
st_write(out.point, "2018_Rebuild_Vecinos_Points.shp", delete_layer = T)

writeOGR(dsn=".", obj = hhop, layer="2018_Rebuild_Vecinos", driver = "ESRI Shapefile", overwrite_layer = T )



#2020 Rebuild
rm(list=ls(all=TRUE)) # clear memory
library(tidyverse)
library(sf)
setwd("E:\\GISWork_2\\Graubart_Lima\\Portugal\\2018_Rebuild") 



name.to.number.table <- read.csv("Names_To_Numbers.csv",stringsAsFactors=F)
raw.2020 <- read_csv("Vecinos for Mat 2020.csv")
raw.2020 <- raw.2020 %>%
  mutate(Title = recode(Title,
              "dona" = "Dona",
              "Doña" = "Dona",
              "Mastre" = "Maestre",
              "don" = "Don"),
         collacion= recode(collacion,
                           "San bartolome" = "San Bartolome Viejo",
                           "San Ildefonso ." = "San Ildefonso",
                           "San isidoro"= "San Isidoro",
                           "San pedro" = "San Pedro",
                           "San Salvador"= "Salvadoe",
                           "San Bartolome" = "San Bartolome Viejo" ),
         religion = recode(religion,
                           "Moro" = "moro",
                           "Judio" = "judio"))




# unique(raw.2020$Title)
# unique(raw.2020$collacion)
# unique(raw.2020$religion)
# unique(raw.2020$occup_class)


last.year <- 1483
sumTable.2020 <- raw.2020 %>%
  group_by(collacion) %>%
  summarise(tot  = n(),
            JewPre1483 =  sum(religion == "judio"),
            MusPre1483 =  sum(religion == "moro" & Year <= last.year),
            MusPost1483 = sum(religion == "moro" & Year > last.year),
            MusTotal = sum(religion == "moro"),
            Tex_pr1483 = sum(occup_class %in% c("textiles","tailor")& Year <= last.year),
            Mas_pr1483 = sum(occup_class %in% c("mason") & Year <= last.year),
            boc_pr1483 = sum(occup_class %in% c("borceguinero") & Year <= last.year),
            mT_pr1483 = sum(religion == "moro" & Year <=last.year & !is.na(Title)),
            muT_pr1483 = sum(religion == "moro" & Year <=last.year & is.na(Title)))


name.to.number.table <- rename(name.to.number.table, "collacion" = "Name")

sumTable.2020 <- left_join(x = sumTable.2020, y=name.to.number.table, by = "collacion")


Vecinos.sf <- st_read("Vecinos_base.shp")
out.poly <- left_join(x = Vecinos.sf, y = sumTable.2020, by = "Number")
out.point <- st_centroid(out.poly)


st_write(out.poly, "2020_Rebuild_Vecinos.shp", delete_layer = T)
st_write(out.point, "2020_Rebuild_Vecinos_Points.shp", delete_layer = T)



# masons, borceguineros, and textiles (and here include tailors in textiles, it looks like I kept them separate).


