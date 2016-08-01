#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("plyr","foreign","rgdal") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("E:\\GISWork_2\\Graubart_Lima\\Lima") 


input.table <- read.csv("1613_census_table.csv",stringsAsFactors=F)
# input.table <- read.csv2("1613_census_table.csv",stringsAsFactors=F,encoding = "ISO-8859-1")


input.table <- input.table[,c("block.no","female.","title.or.status","ident","occupation","marital.stat","cargo")]


#fixing errors in original table
input.table$marital.stat[input.table$marital.stat  == "c "] <- "c"
input.table$marital.stat[input.table$marital.stat  == "C"] <- "c"
input.table$marital.stat[input.table$marital.stat  == " "] <- ""
input.table$marital.stat[input.table$marital.stat  == "divorciada"] <- "d"
input.table$ident[input.table$ident  == "MU "] <- "MU"
input.table$ident[input.table$ident  == "NE "] <- "NE"
input.table$ident[input.table$ident  == "ME "] <- "ME"
input.table$cargo[input.table$cargo == "do�a"] <- "dona"
input.table$cargo[input.table$cargo == "sacrist�n de San Marcelo "] <- "sacristan de San Marcelo"
input.table$cargo[input.table$cargo == "sacrist�n de Capilla de Palacio "] <- "sacristan de Capilla de Palacio"
input.table$cargo[input.table$cargo == "Ca�ar"] <- "Canar"
input.table$female.[input.table$female.  == " f"] <- "f"

input.table <- input.table[!(input.table$block.no %in% c(""," ")),]

# sort(unique(input.table$cargo))


#1: black population
input.table$black01 <- ifelse(  input.table$ident %in% c("MU","MO","NE","CU"),1,0)
#2: merchants
input.table$merc02 <- ifelse(   input.table$occupation %in% c("mercachifle","VEN-mercachifle", "VEN-mercachifle ","VEN-caxonero", "VEN","VEN-mercader"),1,0)
#3: shoemakers
input.table$shoe03 <- ifelse(   startsWith(input.table$occupation,"ZAP"),1,0)
#4: Tailors
input.table$tail04 <- ifelse((  startsWith(input.table$occupation,"SAS") | 
                                input.table$occupation %in% c("RAS y SAS","CHA y SAS") ),1,0)
#5: Fish
input.table$fish05 <- ifelse(   startsWith(input.table$occupation,"PES"),1,0)
#6: Chichero
input.table$chic06 <- ifelse(   startsWith(input.table$occupation,"CHI"),1,0)
#7: chacarerro
input.table$chac07 <- ifelse((  startsWith(input.table$occupation,"CHA") | 
                                input.table$occupation %in% c("SAS-botonero / CHA") ),1,0)
#8: slaves
input.table$slave08 <- ifelse(( startsWith(input.table$occupation,"SER") | 
                                startsWith(input.table$occupation,"ESC") | 
                                input.table$occupation %in% c("SAS/SER","ZAP/SER","SAS/esclavo") | 
                                input.table$title.or.status == "esclava" ),1,0)
#9: rastrero
input.table$rast09 <- ifelse(   startsWith(input.table$occupation,"RAS"),1,0)
#10: Botonero
input.table$bot10 <- ifelse(    startsWith(input.table$occupation,"SAS-botonero"),1,0)
#11: Abridor
input.table$abr11 <- ifelse(    startsWith(input.table$occupation,"ABR"),1,0)

#12: don/dona/nusta/palla
input.table$don12 <- ifelse((   input.table$title.or.status %in% c("dona","dona","don","nusta")|
                                startsWith(input.table$cargo,"don")),1,0)
#13: alcalde
input.table$alc13 <- ifelse((   startsWith(input.table$cargo,"alcalde") | 
                                startsWith(input.table$cargo,"alguacil") | 
                                startsWith(input.table$cargo,"capitan") | 
                                startsWith(input.table$cargo,"sargento")),1,0)
input.table$mar14 <- ifelse((   startsWith(input.table$cargo,"mayordomo") | 
                                startsWith(input.table$cargo,"sacristan") | 
                                startsWith(input.table$cargo,"sirve") | 
                                startsWith(input.table$cargo,"fiscal") |
                                input.table$cargo == "don; mayordomo cof Rosario"),1,0)

new.vars <- input.table[,c(1,8:21)]
new.vars2 <-aggregate(. ~ block.no, data=new.vars, FUN=sum)

#OLDS

#Processing the shapetable to make a tie between blocks and ids
blocks.spdf <- readOGR(".", layer = "Blocks")


# shp.table <- read.dbf("Export_Output_2.dbf",as.is=T)
shp.table <- blocks.spdf@data
shp.table <- shp.table[complete.cases(shp.table),]



y<-strsplit(as.character(shp.table$Indices)  ,";", fixed=TRUE)
shp.table2 <- data.frame(block.no= unlist(y), shp.table[ rep(1:nrow(shp.table), sapply(y, length)) ,  ] )
shp.table2$Id <- NULL
shp.table2$Indices <- NULL
row.names(shp.table2) <- NULL
shp.table2 <- data.frame(lapply(shp.table2, as.character), stringsAsFactors=FALSE)
remove(shp.table)

#Mergin New Data




#gender
gender <- as.data.frame.matrix(xtabs(~block.no+female., data=input.table))
gender$block.no <- as.numeric(rownames(gender))
gender$V1 <- NULL

#marital Status

mar <- as.data.frame.matrix(xtabs(~block.no+marital.stat, data=input.table))
mar$block.no <- as.numeric(rownames(mar))
mar$V1 <- NULL
both <-merge(mar,gender,by="block.no")

#totals
totals <-as.data.frame(table(input.table$block.no))
names(totals)[names(totals) == 'Var1']    <- 'block.no'
names(totals)[names(totals) == 'Freq']    <- 'total'

old.vars<-merge(both, totals, by="block.no",all.x = T)
all.vars <- merge(new.vars2, old.vars, by="block.no",all.x = T)



with.blockID <- merge(shp.table2, all.vars, by="block.no", all.x = T)
with.blockID$block.no <- NULL
with.blockID$Id <- NULL
with.blockID$Indices <- NULL
agged <-aggregate(. ~ Block_ID, data=with.blockID, FUN=sum)
write.csv(agged,"2016-07-27_AllData.csv")
