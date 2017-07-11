# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory



packages<- c("rgdal","sp","lubridate","ggplot2","doBy","dplyr","foreign","grid") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#TODO:
# 1). Use Michelle's data
# 2). Stop dropping bad geocodes for mapping
# 3). Data rework so individuals, cases, and mappables are seperate files

# setwd("E:/GISWork_2/Hui_China") #WIndows maching
setwd("E:/GISWork_2/Beidinger_Lead") #Linux machine


spss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")


which <- "Tracts"
# which <- "Block Groups"
individuals <- TRUE

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
lead.data <- read.csv("N:\\no_tests_final_unique_IDs_removed_UPDATED_Sisk.csv", stringsAsFactors = F)

# lead.data<- read.csv("UniqueID_BG_May2017.csv", stringsAsFactors = F)

#Attempting to read Michelle's spss file
# lead.data <- read.spss("no_tests_final_unique_IDs_removed_UPDATED_Sisk.sav", to.data.frame=TRUE)
# write.csv(dataset,"no_tests_final_unique_IDs_removed_UPDATED.csv",row.names = F)


lead.data$Month <- month(as.Date(lead.data$SPEC_DT, format = "%m/%d/%Y"))
lead.data$year <- year(as.Date(lead.data$SPEC_DT, format = "%m/%d/%Y"))
lead.data$real_spec <- as.Date(lead.data$SPEC_DT, format = "%m/%d/%Y")
lead.data$real_dob <- as.Date(lead.data$DOB, format = "%m/%d/%Y")
lead.data$age <- (lead.data$real_spec- lead.data$real_dob)/365
#Tossing values we don't need
 lead.data <- lead.data[which (lead.data$code != "Postal" & lead.data$code != "Locality"),]
 lead.data <- lead.data[which (as.integer(lead.data$AGE_TRUNC) >=0 & as.integer(lead.data$AGE_TRUNC) < 7),]

#keeping only the first record (i.e. converting from tests to individuals)
if (individuals){
        lead.ind<- lead.data %>%
                group_by(UniqueID) %>%
                arrange(real_spec) %>%
                slice(1L)
        lead.ind <-data.frame(lead.ind)
        # 
}

#making table of tests vs individual
yearly.table.of.tests.individuals <- rbind( table(lead.data$year),table(lead.ind$year))
barplot(yearly.table.of.tests.individuals, beside = T,col = c("blue","green"),legend = c("Tests","Individuals"),main="Children 7 and under", xlab = "Year",args.legend = list(x ='topright', bty='n'))
lead.data <- lead.ind


#Another Table for number of elevated individuals

table(lead.ind$year)
as.data.frame(table(lead.ind[which(lead.ind$PB_CDC>=5 &lead.ind$PB_CDC < 10),]$year))

as.data.frame(table(lead.ind[which(lead.ind$pb_grp == 3),]$year))

table(lead.ind$year,lead.ind$pb_grp)
table(lead.ind[which(lead.ind$PB_CDC >= 10),]$year)


preg <- lead.data[which (lead.data$PREGNANT =="Y"),]

#For all
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
names(lead.agg2) <- c( "m2005","m2006","m2007","m2008","m2009","m2010","m2011","m2012","m2013","m2014","m2015",id.field)
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
for (each in 2005:2015){
        print (each)
        lead.agg[c(paste("p_",each,sep=""))] <- lead.agg[c(paste("e_",each,sep=""))] / lead.agg[c(paste("t_",each,sep=""))]
        }



remove(yearly.elevated,yearly.totals, lead.agg2, yearly.everything)




#bring in spatial data 
st.joe.spatial <- readOGR(dsn=".",layer=in.shape)
if (which == "Block Groups"){
        st.joe.spatial@data[,c(id.field)]<- paste("14",substr(st.joe.spatial@data$GEO_ID,3,21),sep="")
}

st.joes.lead <-merge(st.joe.spatial,lead.agg,by=id.field,all.x=T)

pdf(out.pdf)
spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_2007","p_2008","p_2005","p_2006"),names.attr = c("2007","2008","2005","2006"))
spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_2011","p_2012","p_2009","p_2010"),names.attr = c("2011","2012","2009","2010"))
spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_2015","p_all","p_2013","p_2014"),names.attr = c("2015","All Years","2013","2014"))


spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_all"),main = c("2005-2015"))
grid.text("Percentage of children (7 and under) with lead test > 5", x=unit(0.95, "npc"), y=unit(0.50, "npc"), rot=-90)

spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_2015"),main = c("2015"))
grid.text("Percentage of children (5 and under) with lead test > 5", x=unit(0.95, "npc"), y=unit(0.50, "npc"), rot=-90)


# lead.agg2 <- merge(lead.agg2,lead.agg,by="GEO_ID")
# write.csv(lead.agg2,"lead_test2.csv")


#getting monthly averages
# mon.av <- aggregate( PB_RESULT ~ Month, data = lead.data, FUN= mean,na.remove = T)
# mon.av <- aggregate( PB_RESULT ~ Month, data = lead.data, FUN =  function(x) c( SD = sd(x), MN= mean(x) ) ) 
mon.av <- summaryBy(PB_RESULT ~ Month, data = lead.data,FUN=c(length, mean,sd))
mon.av$se <- mon.av$PB_RESULT.sd / sqrt(mon.av$PB_RESULT.length)

# mon.av <- with(lead.data, aggregate(PB_RESULT ~ Month, FUN =  function(x) c( SD = sd(x), MN= mean(x) ) ) )




p1 <- ggplot(data=mon.av, aes(x=Month, y=PB_RESULT.mean)) + geom_line(size = 1.5) +
        geom_point(size=3) +
        geom_errorbar(aes(x=Month, ymin=PB_RESULT.mean-se, ymax=PB_RESULT.mean+se), width=0.25) + 
        scale_x_continuous(breaks=seq(1,12,1))+
        labs(x="Month", y="Average Lead Result") +
        ggtitle("All children (under 7) 2005-2015")
p1

#GET URBAN CHILDREN MEASURE FROM TRACTS


# lead.month2 <- aggregate( PB_RESULT ~ Month, data = lead.data, FUN = mean, na.remove = F)
# lead.month2 <- aggregate( PB_RESULT ~ Month, data = lead.data[which (lead.data$GEO_ID=="1400000US18141000600"),], FUN = mean, na.remove = F)
#number of tests
# hist(lead.data$year,col="blue",main="All children 2005-2015")
# aa <- as.data.frame(table(lead.data$year[which (lead.data$PB_RESULT >= 5)]))
a<- table(lead.data$PB_RESULT<5, lead.data$year)
b<- rbind(table(lead.data[lead.data$PB_RESULT==0,]$year),table(lead.data[lead.data$PB_RESULT>0 & lead.data$PB_RESULT<5,]$year),table(lead.data[lead.data$PB_RESULT>=5 & lead.data$PB_RESULT<10,]$year),table(lead.data[lead.data$PB_RESULT>=10,]$year))


barplot(b, col = c("blue","green","yellow","red"),legend = c("0","1-4","5-9",">10"),xlab = "Year",ylab = paste("Number of ",labe,sep=""))


dev.off()
# writeOGR(st.joes.lead,dsn=".",layer=out.shape,driver = "ESRI Shapefile", overwrite_layer=T)

# table(lead.data$year)

#DO NUMBER OF UNIQUE KIDS BASED ON CENSUS TRACT POPULATION

#This section is for creating a table
lead.data$AGEL <- as.double(lead.data$AGE)
lead.data$AGE_TRUNC <- as.integer(lead.data$AGE_TRUNC)
lead.data$ageclass3 <-  ifelse(lead.data$AGE_TRUNC == 0, "0-1", 
                        ifelse(lead.data$AGE_TRUNC == 1, "1-1.99",
                        ifelse(lead.data$AGE_TRUNC == 2,"2-2.99",
                        ifelse(lead.data$AGE_TRUNC == 3,"3-3.99",
                        ifelse(lead.data$AGE_TRUNC == 4,"4-4.99",
                        ifelse(lead.data$AGE_TRUNC == 5,"5-5.99",
                        ifelse(lead.data$AGE_TRUNC == 6,"6- 6.99",
                        ifelse(lead.data$AGE_TRUNC <=18,"7-18",
                        ifelse(lead.data$AGE_TRUNC <=45,"18-45",
                        ifelse(lead.data$AGE_TRUNC >45,"45 +","ERROR"))))))))))

# aggregate(PB_RESULT~ageclass3, data=lead.data[which(lead.data$PB_CDC > 0),], FUN=mean)
# summaryBy(PB_RESULT ~ ageclass3, data = lead.data,FUN=c(length, mean,sd))

demog.table03 <- merge(summaryBy(PB_CDC ~ ageclass3, data = lead.data,FUN=c(length, mean,sd,max),var.names = "all",fun.names=c("n","mean","sd","max")),summaryBy(PB_CDC ~ ageclass3, data = lead.data[which(lead.data$PB_CDC > 0),],FUN=c(length, mean,sd),var.names = "ov0",fun.names=c("n","mean","sd")),by="ageclass3")
# write.csv(demog.table03,"DemographicTable_under7.csv",row.names = F)




#Section for creating census tracts table
pov.table <- read.csv("OtherData//ACS_Tables//ACS_2015_5year_Poverty.csv",stringsAsFactors = F)
census2010 <-read.csv("OtherData//ACS_Tables//C2010_Children.csv",stringsAsFactors = F)
acsAll <-read.csv("OtherData//ACS_Tables//ACS_5yr_compiled_NoHeader.csv",stringsAsFactors = F)
all.census <- merge(census2010,pov.table,by="Geo_FIPS",all=T)
all.census <- merge(all.census,acsAll,by="Geo_FIPS",all=T)
all.census$GEO_ID <- paste("1400",substr(all.census$Geo_GEOID.y,3,25),sep="")

all.census.final <- merge(st.joes.lead@data,all.census,by="GEO_ID",all=T)
all.census.final$hh_povPer <- 100* all.census.final$X2015_povFam / all.census.final$X2015_numFam

exporter <- all.census.final[,c("GEO_ID","NAME","YearBuilt","N_all","N_elev","p_all","e_2015","t_2015","p_2015","hh_povPer","X2015_SE_T007_002")]
write.csv(exporter,"Report//TractTable.csv",row.names = F)
