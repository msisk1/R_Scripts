# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory



packages<- c("rgdal","sp","lubridate","ggplot2","doBy") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first



# setwd("E:/GISWork_2/Hui_China") #WIndows maching
setwd("E:/GISWork_2/Beidinger_Lead") #Linux machine

id.field <- "GEO_ID"



lead.data <- read.csv("HBB312017.csv", stringsAsFactors = F)
lead.data$Month <- month(as.Date(substr(lead.data$SPEC_DT,1,10), format = "%m/%d/%Y"))
lead.data$year <- year(as.Date(substr(lead.data$SPEC_DT,1,10), format = "%m/%d/%Y"))


lead.data$age <- (as.Date(substr(lead.data$SPEC_DT,1,10), format = "%m/%d/%Y")- as.Date(substr(lead.data$DOB,1,10), format = "%m/%d/%Y")) / 365
#Tossing values we don't need
lead.data <- lead.data[which (lead.data$code != "Postal" & lead.data$code != "Locality"),]
lead.data <- lead.data[which (lead.data$age >=0 & lead.data$age < 7),]

#For all
lead.agg <- aggregate( PB_RESULT ~ GEO_ID, data = lead.data, FUN = mean, na.remove = F)
total.all <- as.data.frame(table(lead.data$GEO_ID))
names(total.all) <- c(id.field,"N_all")
total.elevated <- as.data.frame(table(lead.data[which (lead.data$PB_RESULT>=5),]$GEO_ID))
names(total.elevated) <- c(id.field,"N_elev")


lead.agg <- merge(lead.agg,total.all,by=id.field,all=T)
lead.agg <- merge(lead.agg,total.elevated,by=id.field,all=T)
lead.agg$p_all <- lead.agg$N_elev / lead.agg$N_all
lead.agg[is.na(lead.agg)] <- 0


names(lead.agg) <- c(id.field,"Tot_Mean","N_all","N_elev","p_all")
remove(total.all,total.elevated)


#By year

lead.agg2 <- data.frame(with(lead.data, tapply(PB_RESULT, list(GEO_ID, year), mean)))
lead.agg2$GEO_ID <- row.names(lead.agg2)
names(lead.agg2) <- c( "m2005","m2006","m2007","m2008","m2009","m2010","m2011","m2012","m2013","m2014","m2015","GEO_ID")
lead.agg2[is.na(lead.agg2)] <- -1


lead.agg <- merge(lead.agg,lead.agg2,by=id.field,all.x=T)

yearly.totals <- as.data.frame.matrix(with(lead.data, table(GEO_ID,year)))
names(yearly.totals) <- paste("t_",names(yearly.totals),sep="")
yearly.totals$GEO_ID <- row.names(yearly.totals)

yearly.elevated <- as.data.frame.matrix(with(lead.data[which (lead.data$PB_RESULT>=5),], table(GEO_ID,year)))
names(yearly.elevated) <- paste("e_",names(yearly.elevated),sep="")
yearly.elevated$GEO_ID <- row.names(yearly.elevated)

yearly.everything <-  merge(yearly.elevated,yearly.totals,by=id.field,all=T)
yearly.everything[is.na(yearly.everything)] <- 0

lead.agg <- merge(lead.agg,yearly.everything,all.x=T,by=id.field)
for (each in 2005:2015){
        print (each)
        lead.agg[c(paste("p_",each,sep=""))] <- lead.agg[c(paste("e_",each,sep=""))] / lead.agg[c(paste("t_",each,sep=""))]
        }



remove(yearly.elevated,yearly.totals, lead.agg2, yearly.everything)



#bring in spatial data 
st.joe.tracts <- readOGR(dsn=".",layer="StJoe_Tracts_Blank")
st.joes.lead <-merge(st.joe.tracts,lead.agg,by=id.field,all.x=T)

pdf("LeadPercentMaps.pdf")
spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_2007","p_2008","p_2005","p_2006"),names.attr = c("2007","2008","2005","2006"))
spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_2011","p_2012","p_2009","p_2010"),names.attr = c("2011","2012","2009","2010"))
spplot(st.joes.lead[which (st.joes.lead$City == 2 ),], c("p_2015","p_all","p_2013","p_2014"),names.attr = c("2015","All Years","2013","2014"))
# lead.agg2 <- merge(lead.agg2,lead.agg,by="GEO_ID")
# write.csv(lead.agg2,"lead_test2.csv")


#getting monthly averages
# mon.av <- aggregate( PB_RESULT ~ Month, data = lead.data, FUN= mean,na.remove = T)
# mon.av <- aggregate( PB_RESULT ~ Month, data = lead.data, FUN =  function(x) c( SD = sd(x), MN= mean(x) ) ) 
mon.av <- summaryBy(PB_RESULT ~ Month, data = lead.data,FUN=c(length, mean,sd))
mon.av$se <- mon.av$PB_RESULT.sd / sqrt(mon.av$PB_RESULT.length)

# mon.av <- with(lead.data, aggregate(PB_RESULT ~ Month, FUN =  function(x) c( SD = sd(x), MN= mean(x) ) ) )



c(mean = mean(x), sd = sd(x))
# plot(mon.av, main="All children 2005-2015",type="l")

p1 <- ggplot(data=mon.av, aes(x=Month, y=PB_RESULT.mean)) + geom_line(size = 1.5) +
        geom_point(size=3) +
        geom_errorbar(aes(x=Month, ymin=PB_RESULT.mean-se, ymax=PB_RESULT.mean+se), width=0.25) + 
        scale_x_continuous(breaks=seq(1,12,1))+
        labs(x="Month", y="Average Lead Result") +
        ggtitle("All children 2005-2015")
p1
qplot(x,y)+geom_errorbar(aes(x=x, ymin=y-sd, ymax=y+sd), width=0.25)

summarySE
#GET URBAN CHILDREN MEASURE FROM TRACTS


# lead.month2 <- aggregate( PB_RESULT ~ Month, data = lead.data, FUN = mean, na.remove = F)
# lead.month2 <- aggregate( PB_RESULT ~ Month, data = lead.data[which (lead.data$GEO_ID=="1400000US18141000600"),], FUN = mean, na.remove = F)
#number of tests
# hist(lead.data$year,col="blue",main="All children 2005-2015")
aa <- as.data.frame(table(lead.data$year[which (lead.data$PB_RESULT >= 5)]))
a<- table(lead.data$PB_RESULT<5, lead.data$year)

barplot(a, col = c("red","grey"),legend = c("elevated","<5"),xlab = "Year",ylab = "Number of Tests",main="All children 2005-2015")
dev.off()
writeOGR(st.joes.lead,dsn=".",layer="StJosephTractsLeadAggregated",driver = "ESRI Shapefile", overwrite_layer=T)

# table(lead.data$year)

#DO NUMBER OF UNIQUE KIDS BASED ON CENSUS TRACT POPULATION

