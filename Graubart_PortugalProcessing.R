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

input.table$Title[input.table$Title %in% c("dona","Doña","dona ","Dona ")] <- "Dona"
input.table$Title[input.table$Title  == "don "] <-"Don"
input.table$Title[input.table$Title  == "Mastre"] <-"Maestre"
input.table$Title[input.table$Title  == "Rabi "] <-"Rabi"


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

old.data$JewPre1483  <- ifelse((old.data$religion ==  "judio" & old.data$Year <= 1493),1,0)

# 6 Muslims before 1483, both indicating density
old.data$MusPre1483  <- ifelse((old.data$religion ==  "moro" & old.data$Year <= 1493),1,0)

# 7 Muslims after 1483 by density
old.data$MusPost1483  <- ifelse((old.data$religion ==  "moro" & old.data$Year > 1493),1,0)





proc.table <- merge(input.table, name.to.number.table, by.x = "collacion", by.y = "Name", all.x = T)

temp1 <- proc.table[,c(9:13)]
temp1 <-aggregate(. ~ Number, data=temp1, FUN=sum)

temp2 <- old.data[,c(11, 15:17)]
temp2 <-aggregate(. ~ Number, data=temp2, FUN=sum)

temp1 <- merge(temp1,temp2, by = "Number", all = T)

Vecinos.spdf <- readOGR(".", layer = "Vecinos_base")
hhop <- merge(Vecinos.spdf, temp1, by = "Number", all.x = T)
writeOGR(dsn=".", obj = hhop, layer="2018_Rebuild_Vecinos", driver = "ESRI Shapefile")

