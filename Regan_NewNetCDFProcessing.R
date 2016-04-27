rm(list=ls(all=TRUE)) # clear memory

packages<- c("ncdf4","chron","rgdal","sp","reshape2") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd("/mnt/smb/Research/OTool_Distances")
setwd("E:\\GISWork_2\\Regan_Conflict\\2016-04-21_worldClimate")
#Functions
open.netcdf.return.df<-function(file.name, outfield.name = "nothing", cut.year = NA, drop.na = FALSE){
        options(chron.year.abb=FALSE)
        
        netcdf <- nc_open(file.name)
        lon <- ncvar_get(netcdf, "lon")
        lat <- ncvar_get(netcdf, "lat")
        # processing time
        time <- ncvar_get(netcdf, "time")
        time.units <- ncatt_get(netcdf, 'time', "units")$value
        tustr <- strsplit(time.units, " ")
        unit<- unlist(tustr)[1]
        tdstr <- strsplit(unlist(tustr)[3], "-")
        tmonth = as.integer(unlist(tdstr)[2])
        tday = as.integer(unlist(tdstr)[3])
        tyear = as.integer(unlist(tdstr)[1])
        if (unit=="days"){
                div.num <- 1
        }else if (unit=="seconds"){
                div.num <- 86400
        }else if (unit=="hours"){
                div.num <- 24
        }else {
                message("ERROR: Fix the time")
                return(NULL)
                stop()
        }
        
        time.readable = as.Date(chron(time / div.num, origin=c(month=tmonth,day=tday,year=tyear), out.format = "y-m-d"))
        #New pre Data.frame cutting based on time
        time.binary <- (time.readable >= paste(cut.year,"-1-1",sep=""))
        # time.binary <- (time.readable == "1980-1-1")
        
        first.index <- min(which(time.binary == T))
        time.cut<- time.readable[time.binary]
        
        
        
        
        field.name <- names(netcdf$var)[[1]]
        val <- (ncvar_get(netcdf, field.name,start=c(1,1,first.index)))
        tmp.vec.long <- as.vector(val)
        val.as.df <-data.frame(tmp.vec.long)
        df.out <- expand.grid(lon, lat, time.cut)
        netcdf.for.export <- cbind(df.out, val.as.df)
        if (outfield.name == "nothing"){ outfield.name  <- field.name}
        names(netcdf.for.export) <- c("lon", "lat","date",outfield.name)
        # if(!is.na(cut.year)){
        #         netcdf.for.export<- netcdf.for.export[(netcdf.for.export$date > paste(cut.year,"-1-1",sep="")),]
        # }
        if (drop.na){
                netcdf.for.export <- netcdf.for.export[complete.cases(netcdf.for.export),] 
        }
        
        return(netcdf.for.export)
}





convert.necdfs <- FALSE


#1). converting netcdfs to csv


if (convert.necdfs){
        precip.data <- open.netcdf.return.df(file.name = "NetCDFs\\precip.mon.mean.nc", outfield.name = "precip", cut.year = 1980, drop.na = F)
        write.csv(precip.data,"precipitation_old.csv",row.names = F)
        
        temp.data <- open.netcdf.return.df(file.name = "NetCDFs\\air.2x2.1200.mon.anom.land.nc", outfield.name = "temp", cut.year = 1980, drop.na = T)
        write.csv(temp.data,"temp.csv",row.names = F)
        
        pdsi.data <- open.netcdf.return.df(file.name = "NetCDFs\\pdsi.mon.mean.selfcalibrated.nc", outfield.name = "pdsi", cut.year = 1980, drop.na = T)
        write.csv(pdsi.data,"pdsi.csv",row.names = F)
        
        smOLD.data <- open.netcdf.return.df(file.name = "NetCDFs\\soilw.mon.mean.v2.nc", outfield.name = "smOLD", cut.year = 1980, drop.na = T)
        #write.csv(smOLD.data,"soilmoisture_old.csv",row.names = F)
        for (i in  seq(1, nrow(smOLD.data),1000000)){
                write.table(smOLD.data[i:(i + 999999),],"soilmoisture_old.csv",row.names = F, append = T,sep=",")
        }
        
}

#2 adding the gridcell to the csvs
append.gridcell.values<-function(point.df,gridcells.spdf,x.coord = "lon", y.coord = "lat", throw.out.nulls = TRUE){
        
        unique.latlons <- unique(point.df[,c(x.coord,y.coord)])
        if(min(unique.latlons$lon)> 0){
                unique.latlons$lon <- unique.latlons$lon - 180
                point.df$lon<- point.df$lon-180
                print("moved coords")
        }
        
        unique.ll.spatial <- unique.latlons
        
        proj <- proj4string(grid.cells) #gives the projection information
        coordinates(unique.ll.spatial)=unique.ll.spatial[c(x.coord,y.coord)]
        proj4string(unique.ll.spatial)=CRS(proj) # set it to lat/long
        
        over.df <- over(unique.ll.spatial, grid.cells) 
        ull.with.gridID <- cbind(unique.latlons, over.df[,c("Id","All_GWNO")]) # I think this needs more work
        
        ull.with.gridID$ll <- paste(ull.with.gridID$lon,ull.with.gridID$lat)
        ull.with.gridID[,x.coord] <- NULL
        ull.with.gridID[,y.coord] <- NULL
        
        point.df$ll <- paste(point.df$lon,point.df$lat)
        #out.data.old <- merge(point.df,ull.with.gridID,by="ll",all.x=T)
        out.data <- merge(point.df,ull.with.gridID,by="ll")
        out.data$ll <- NULL
        
        
        
        names(out.data)[names(out.data) == 'Id']    <- 'GridID' 
        if (throw.out.nulls){
                out.data<-out.data[!is.na(out.data$GridID),]
        }
        out.data$Year <- substr(out.data$date,1,4)
        out.data$month <- substr(out.data$date,6,7)
        #table.to.process$GC_Month <- paste(table.to.process$GridID,table.to.process$month, sep='-') #creates the year month field
        out.data$yearMonth <- paste(out.data$Year, out.data$month, sep='') #creates the year month field
        out.data$gymID <- paste(out.data$GridID, out.data$yearMonth, sep='-') #creates the gymID field  
        return(out.data)
        
}
createStatsTable <- function(in.table, summary.variable, aggregate.variable){
        print("Calculating Mean")
        table.mean <- aggregate(in.table[[summary.variable]], list(in.table[[aggregate.variable]]), FUN=mean)
        names(table.mean)[names(table.mean) == 'x'] <- paste0(summary.variable, "_mean")
        
        print("Calculating Min")
        table.min <- aggregate(in.table[[summary.variable]], list(in.table[[aggregate.variable]]), FUN=min)
        names(table.min)[names(table.min) == 'x'] <- paste0(summary.variable, "_min")
        
        print("Calculating Max")
        table.max <- aggregate(in.table[[summary.variable]], list(in.table[[aggregate.variable]]), FUN=max)
        names(table.max)[names(table.max) == 'x'] <- paste0(summary.variable, "_max")
        
        print("Calculating count")
        table.n <- aggregate(in.table[[summary.variable]], list(in.table[[aggregate.variable]]), FUN=function(x) sum( !is.na(x)))
        names(table.n)[names(table.n) == 'x'] <- paste0(summary.variable, "_n")
        
        summ.table <- merge(table.mean,table.min,by="Group.1")
        summ.table <- merge(summ.table,table.max,by="Group.1")
        summ.table <- merge(summ.table,table.n,by="Group.1")
        names(summ.table)[names(summ.table) == 'Group.1'] <- aggregate.variable
        
        summ.table
        
}#end createStatsTable

grid.cells <- readOGR("Shapefiles", layer = "WorldGridcells_2d30s")
if (!file.exists("precip_table.csv")){
        precip.data <- read.csv("precipitation.csv", stringsAsFactors = T)
        
        precip.data.processed <- append.gridcell.values(point.df = precip.data,gridcells.spdf = grid.cells)
        precip.table <- createStatsTable(in.table= precip.data.processed,summary.variable= "precip",aggregate.variable="gymID")
        write.csv(precip.table,"precip_table.csv", row.names = F)
        
} else{
        precip.table <- read.csv("precip_table.csv")
}

if (!file.exists("temp_table.csv")){
        temp.data <- read.csv("temp.csv", stringsAsFactors = T)
        
        temp.data.processed <- append.gridcell.values(point.df = temp.data,gridcells.spdf = grid.cells)
        temp.table <- createStatsTable(in.table= temp.data.processed,summary.variable= "temp",aggregate.variable="gymID")
        write.csv(temp.table,"temp_table.csv", row.names = F)
}else{
        temp.table <- read.csv("temp_table.csv")
}

if (!file.exists("pdsi_table.csv")){
        pdsi.data <- read.csv("pdsi.csv", stringsAsFactors = T)
        
        pdsi.data.processed <- append.gridcell.values(point.df = pdsi.data,gridcells.spdf = grid.cells)
        pdsi.table <- createStatsTable(in.table= pdsi.data.processed,summary.variable= "pdsi",aggregate.variable="gymID")
        write.csv(pdsi.table,"pdsi_table.csv", row.names = F)
}else{
        pdsi.table <- read.csv("pdsi_table.csv")
}
if (!file.exists("smold_table.csv")){
        sm.data <- read.csv("soilmoisture_oldTotal.csv", stringsAsFactors = T)
        
        sm.data.processed <- append.gridcell.values(point.df = sm.data,gridcells.spdf = grid.cells)
        sm.table <- createStatsTable(in.table= sm.data.processed,summary.variable= "smOLD",aggregate.variable="gymID")
        write.csv(sm.table,"smold_table.csv", row.names = F)
}else{
        sm.table <- read.csv("smold_table.csv", row.names = F)
}

#3 Aggrigating, merging and duplicating on gwno
table.gwno <- grid.cells@data
table.gwno <- table.gwno[complete.cases(table.gwno),]
# write.csv(table.gwno,"table_GWNO.CSV")
table.gwno <- table.gwno[,c("Id","All_GWNO")]
names(table.gwno) <- c("GridID", "All_GWNO")




master.table <- merge(precip.table,pdsi.table,by="gymID", all=T)
master.table <- merge(master.table,temp.table,by="gymID", all=T)
master.table$GridID <- colsplit(master.table$gymID,"-",c("gridID","YearMonth"))[,1]
master.table <- merge(master.table,table.gwno,by="GridID",all.x=T)
master.table02 <- master.table[!is.na(master.table$All_GWNO),]

#Keep temp varience only
master.table02$precip_min <- NULL
master.table02$precip_max <- NULL
master.table02$pdsi_min <- NULL
master.table02$pdsi_max <- NULL
master.table02$precip_n <- NULL
master.table02$pdsi_n <- NULL
names(master.table02)[names(master.table02) == 'pdsi_mean']   <- "pdsi"
names(master.table02)[names(master.table02) == 'precip_mean']   <- "precip"


#Splitting off into the 
y<-strsplit(as.character( master.table02$All_GWNO)  , " ", fixed=TRUE)
final.data <- data.frame(GWNO_sing= unlist(y), master.table02[ rep(1:nrow(master.table02), sapply(y, length)) , -1 ] )


write.csv(final.data, "final_all_but_SM.csv")