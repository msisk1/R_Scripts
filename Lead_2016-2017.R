# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory



packages<- c("rgdal","sp","lubridate","ggplot2","doBy","dplyr","foreign","grid","tidyr") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first


# setwd("E:/GISWork_2/Hui_China") #WIndows maching
setwd("E:/GISWork_2/Beidinger_Lead") #Linux machine



which <- "Tracts"
# which <- "Block Groups"
individuals <- T
max.age <- 5



if (individuals){
  labe <- "Individuals"
}else{
  labe <- "Tests"
  
}

if (which == "Tracts"){
  id.field <- "GEO_ID"
  in.shape <- "StJoe_Tracts_Blank"
  out.pdf <- paste("LeadPercentMaps_Tract_",labe,".pdf",sep="")
  out.shape <- "StJosephTractsLeadAggregated"
  
}else if (which == "Block Groups"){
  id.field <- "BG_GEOID"
  in.shape <- "StJoe_BlockGroups"
  out.pdf <- paste("LeadPercentMaps_BlockGroups_",labe,".pdf",sep="")
  out.shape <- "StJoseph_BG_LeadAggregated"
  
}
# 



# lead.data.old <- read.csv("HBB312017.csv", stringsAsFactors = F)                  #OLD DATA
lead.data <- read.csv("no_tests_final_unique_IDs_removed_UPDATED_Sisk.csv", stringsAsFactors = F)

lead.2017 <- read.csv("OtherData\\St Joseph County Lead Test Data 2017.csv", stringsAsFactors = F)
lead.2016 <- read.csv("OtherData\\St Joseph County Lead Test Data 2016.csv", stringsAsFactors = F)


# lead.data<- read.csv("UniqueID_BG_May2017.csv", stringsAsFactors = F)

#Attempting to read Michelle's spss file
# lead.data <- read.spss("no_tests_final_unique_IDs_removed_UPDATED_Sisk.sav", to.data.frame=TRUE)
# write.csv(dataset,"no_tests_final_unique_IDs_removed_UPDATED.csv",row.names = F)


lead.data$Month <- month(as.Date(lead.data$SPEC_DT, format = "%m/%d/%Y"))
lead.data$year <- year(as.Date(lead.data$SPEC_DT, format = "%m/%d/%Y"))
lead.data$real_spec <- as.Date(lead.data$SPEC_DT, format = "%m/%d/%Y")
lead.data$real_dob <- as.Date(lead.data$DOB, format = "%m/%d/%Y")
lead.data$age <- as.numeric((lead.data$real_spec- lead.data$real_dob))%/%365

#Tossing values we don't need
lead.data <- lead.data[which (lead.data$code != "Postal" & lead.data$code != "Locality"),]
lead.data <- lead.data[which (as.integer(lead.data$AGE_TRUNC) >=0 & as.integer(lead.data$AGE_TRUNC) < max.age),]

#keeping only the first record (i.e. converting from tests to individuals)
if (individuals){
  lead.ind<- lead.data %>%
    group_by(UniqueID) %>%
    arrange(real_spec) %>%
    slice(1L)
  lead.ind <-data.frame(lead.ind)
  # # Original way of taking the first chronological test
  
  df1 <- lead.data %>%
    group_by(UniqueID) %>%
    summarize(maxVen = max(PB_RESULT[SAMPLE_TYPE_TEXT == "Venous"], na.rm = T),
              MinCap = min(PB_RESULT[SAMPLE_TYPE_TEXT=="Capillary"], na.rm = T),
              MinUnk = min(PB_RESULT[SAMPLE_TYPE_TEXT=="Unknown"], na.rm = T))
  df1$bestPB <- ifelse(df1$maxVen>-Inf, df1$maxVen, ifelse(df1$MinCap<Inf,df1$MinCap, df1$MinUnk))
  lead.ind <-  merge(lead.ind,df1, by = "UniqueID")
}

lead.ind2 <- lead.ind[,c("UniqueID","age","SEX","RACE_TEXT","ETHNIC_TEXT","Month","year","PB_RESULT","PBUNITS_TEXT","SAMPLE_TYPE_TEXT","MEDICAID","GEO_ID")]
lead.2017$year <- 2017
lead.2016$year <- 2016
lead.2017$month <- month(as.Date(lead.2017$Test_Month, format = "%m/%d/%Y"))
lead.2016$month <- month(as.Date(lead.2016$Test_Month, format = "%m/%d/%Y"))
lead.new <- rbind(lead.2016,lead.2017)
lead.new$Test_Month <- NULL
lead.new$TOWNSHIP <- NULL
library(stringr)

lead.new$CENSUS_TRACT <- paste("1400000US18141",as.character(str_pad(as.character(lead.new$CENSUS_TRACT*100), 6, pad = "0")),sep="")
names(lead.new) <- c("UniqueID","age","SEX","RACE_TEXT","ETHNIC_TEXT","PB_RESULT","PBUNITS_TEXT","SAMPLE_TYPE_TEXT","MEDICAID","GEO_ID", "year","Month" )
length(unique(lead.ind2$GEO_ID))
lead.ind <- rbind(lead.new,lead.ind2)
length(unique(lead.ind$GEO_ID))
save(lead.data, lead.ind, file = "2005-2017.RData")

#making table of tests vs individual
yearly.table.of.tests.individuals <- rbind( table(lead.data$year),table(lead.ind$year))
yearly.table.of.tests.individuals[1,"2016"]<-NA
yearly.table.of.tests.individuals[1,"2017"]<-NA
barplot(yearly.table.of.tests.individuals, beside = T,col = c("blue","green"),legend = c("Tests","Individuals"),main=paste("Children ",max.age," and under",sep=""), xlab = "Year",args.legend = list(x =10, bty='n'))
lead.data <- lead.ind

lead.agg1 <-  lead.data %>%
  group_by(GEO_ID, year) %>%
  summarize(av_pb = mean(PB_RESULT))%>%
  spread(year,av_pb) %>%
  setNames(c(names(.)[1], paste0("Mean_",names(.)[-1]))) 


lead.agg <-  lead.data %>%
  group_by(GEO_ID, year) %>%
  count()%>%
  spread(year,n) %>%
  setNames(c(names(.)[1], paste0("N_",names(.)[-1]))) %>%
  full_join(lead.agg1,by = "GEO_ID")

lead.agg1 <- lead.agg

lead.agg <-  lead.data %>%
  group_by(GEO_ID, year) %>%
  filter(PB_RESULT>=5)%>%
  count()%>%
  spread(year,n) %>%
  setNames(c(names(.)[1], paste0("E_",names(.)[-1]))) %>%
  full_join(lead.agg1,by = "GEO_ID")




lead.agg <- aggregate(PB_RESULT ~ get(id.field), data = lead.data, FUN = mean, na.remove = F)

names(lead.agg) <- c(id.field,"Tot_Mean")

total.all <- as.data.frame(table(lead.data[,c(id.field)]))
names(total.all) <- c(id.field,"N_all")
total.elevated <- as.data.frame(table(lead.data[which (lead.data$PB_RESULT>=5),][,c(id.field)]))
names(total.elevated) <- c(id.field,"N_elev")


lead.agg <- merge(lead.agg,total.all,by=id.field,all=T)
lead.agg <- merge(lead.agg,total.elevated,by=id.field,all=T)
lead.agg$p_all <- lead.agg$N_elev / lead.agg$N_all
lead.agg[is.na(lead.agg)] <- 0


names(lead.agg) <- c(id.field,"Tot_Mean","N_all","N_elev","p_all")
remove(total.all,total.elevated)


#By year

lead.agg2 <- data.frame(with(lead.data, tapply(PB_RESULT, list(get(id.field), year), mean)))

lead.agg2[,c(id.field)] <- row.names(lead.agg2)
names(lead.agg2) <- c( "m2005","m2006","m2007","m2008","m2009","m2010","m2011","m2012","m2013","m2014","m2015","m2016","m2017",id.field)
lead.agg2[is.na(lead.agg2)] <- -1


lead.agg <- merge(lead.agg,lead.agg2,by=id.field,all.x=T)

yearly.totals <- as.data.frame.matrix(with(lead.data, table(get(id.field),year)))
names(yearly.totals) <- paste("t_",names(yearly.totals),sep="")
yearly.totals[,c(id.field)] <- row.names(yearly.totals)

yearly.elevated <- as.data.frame.matrix(with(lead.data[which (lead.data$PB_RESULT>=5),], table(get(id.field),year)))
names(yearly.elevated) <- paste("e_",names(yearly.elevated),sep="")
yearly.elevated[,c(id.field)] <- row.names(yearly.elevated)

yearly.everything <-  merge(yearly.elevated,yearly.totals,by=id.field,all=T)
yearly.everything[is.na(yearly.everything)] <- 0

lead.agg <- merge(lead.agg,yearly.everything,all.x=T,by=id.field)
for (each in 2005:2017){
  print (each)
  lead.agg[c(paste("p_",each,sep=""))] <- lead.agg[c(paste("e_",each,sep=""))] / lead.agg[c(paste("t_",each,sep=""))]
}



remove(yearly.elevated,yearly.totals, lead.agg2, yearly.everything)







st.joe.spatial <- readOGR(dsn=".",layer=in.shape)
if (which == "Block Groups"){
  st.joe.spatial@data[,c(id.field)]<- paste("14",substr(st.joe.spatial@data$GEO_ID,3,21),sep="")
}

st.joes.lead <-merge(st.joe.spatial,lead.agg,by=id.field,all.x=T)
# writeOGR(st.joes.lead,dsn=".",layer=out.shape,driver = "ESRI Shapefile", overwrite_layer=T)
pdf(out.pdf)
spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_2007","p_2008","p_2005","p_2006"),names.attr = c("2007","2008","2005","2006"))
spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_2011","p_2012","p_2009","p_2010"),names.attr = c("2011","2012","2009","2010"))
spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_2015","p_2016","p_2013","p_2014"),names.attr = c("2015","2016","2013","2014"))
spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("t_2015","t_2016"),names.attr = c("2015","2016"))

spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("t_2017","N_all"),names.attr = c("2017","All Years"))


tiff("Plot4.tif", width = 8, height = 8, units = 'in', res = 200)
spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_all"),main = c("2005-2015"))
# list("sp.text", coordinates(x), label, cex=0.5, col="green")

a<- table(lead.data$PB_RESULT<5, lead.data$year)
b<- rbind(table(lead.data[lead.data$PB_RESULT==0,]$year),table(lead.data[lead.data$PB_RESULT>0 & lead.data$PB_RESULT<5,]$year),table(lead.data[lead.data$PB_RESULT>=5 & lead.data$PB_RESULT<10,]$year),table(lead.data[lead.data$PB_RESULT>=10,]$year))

c <- as.data.frame(b)
row.names(c)<-c("0","1-4","5-9",">10")
c$caty <- row.names(c)
d<- gather(c, key = c("caty"))


library(dplyr)
library(tidyr)


barplot(b, col = c("blue","green","yellow","red"),legend.text = c("0","1-4","5-9",">10"),args.legend = list(x=10,y=4500),xlab = "Year",ylab = paste("Number of ",labe,sep=""))




#zip code section
library(tidycensus)
variab <- c("B01001_003","B01001_027", "B01001_001")
# li <- load_variables(year = 2015, dataset = "acs5")

zcta.all <- get_acs(geography="zcta", variables = variab,year=2015, output="wide", geometry = TRUE, survey = "acs5")
zcta.all <- zcta.all%>%
  rename("u5_Male" = 'B01001_003E',"u5_Female" = "B01001_027E", "totalpop" = 'B01001_001E' )%>%
  select(GEOID,u5_Male,u5_Female,totalpop,geometry)

zcta.all$u5_all <- zcta.all$u5_Female + zcta.all$u5_Male
zcta.sub <- filter(zcta.all, GEOID %in% c("46544","46545","46561"))
# new.census$GEO_ID <- paste("1400000US",new.census$GEOID,sep="")


tract.sjc <- get_acs(geography="tract", variables = variab,year=2016, output="wide", state="IN", county=141, geometry = TRUE, survey = "acs5")
tract.sjc <- tract.sjc %>%
  rename("u5_Male" = 'B01001_003E',"u5_Female" = "B01001_027E", "totalpop" = 'B01001_001E' )%>%
  select(GEOID,u5_Male,u5_Female,totalpop,geometry)

tract.sjc$u5_all <- tract.sjc$u5_Female + tract.sjc$u5_Male
tract.sjc$GEO_ID <- paste("1400000US",tract.sjc$GEOID,sep="")


# library(sf)
# st_write(zcta.sub, "zcta_sub.shp")



working <- st.joes.lead@data
working2 <- merge(working,tract.sjc, by = "GEO_ID")

a<- working2%>% 
  group_by(City)%>%
  summarise_if(is.numeric,sum)%>%
  bind_rows(working2[working2$NAME=="116.02",])
write.csv(a[,1:70],"MishOc.csv", row.names = F)




res <- lead.data %>% mutate(category=cut(PB_RESULT, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("0","1-4","5-9", ">10")))
