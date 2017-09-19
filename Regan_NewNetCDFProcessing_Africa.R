rm(list=ls(all=TRUE)) # clear memory
#test
packages<- c("ncdf4","chron","rgdal","sp","reshape2","cmsaf","stringr", "foreign","readstata13", "rgeos") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd("/mnt/smb/Research/OTool_Distances")
setwd("E:\\GISWork_2\\Regan_Conflict\\2016-06-15_TotalRebuild")


#To add 7/13/2016: Sensitivity (2014), aridity, sm (non anomaly)

#Functions
open.netcdf.return.df<-function(file.name, outfield.name = "nothing", cut.year = 1980, drop.na = FALSE,lat.range=c(-40,55),lon.range = c(-30,70),write.netcdf = FALSE, funky.override = FALSE){
        if (FALSE){
                
                
                
                file.name = paste(netcdf.folder,"soilw.mon.mean.v2.nc",sep = "")
                
                funky.override <- TRUE
                
                
                outfield.name = "sm"
                cut.year = 1980
                drop.na = T
                write.netcdf = TRUE
                
                
                
                
                lat.range=c(-40,55)
                lon.range = c(-30,70)
        }
        
        min.lat = lat.range[1]
        max.lat = lat.range[2]
        min.lon = lon.range[1]
        max.lon = lon.range[2]
        ncFile <- nc_open( file.name )
        lon <- ncvar_get(ncFile, "lon")
        if (funky.override | (min(lon) < -100) ){
                funky.longitude  <- FALSE
        }else{
                funky.longitude <- TRUE
        }
        lat <- ncvar_get(ncFile, "lat")
        time <- ncvar_get(ncFile, "time")
        
        
        field.name <- names(ncFile$var)[[1]]
        field.description <- ncatt_get(ncFile,field.name , "long_name")$value
        field.units <-  ncatt_get(ncFile,field.name , "units")$value
        time.units <- ncatt_get(ncFile, 'time', "units")$value
        
        
        #Processing Time
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
        
        origin.date <- c(month=tmonth,day=tday,year=tyear)
        time.readable = as.Date(chron(time / div.num, origin=origin.date, out.format = "y-m-d"))
        # New pre Data.frame cutting based on time
        remove(tustr,unit,tdstr,tmonth,tday,tyear)
        
        
        LatIdx <- which( ncFile$dim$lat$vals >= min.lat & ncFile$dim$lat$vals <=max.lat)
        if (funky.longitude){
                LonIdx <- c(which( ncFile$dim$lon$vals > (min.lon+360)) , which( ncFile$dim$lon$vals < (max.lon)) )
                scale <- 360
        }else{
                LonIdx <- which( ncFile$dim$lon$vals >= min.lon & ncFile$dim$lon$vals <= max.lon)
                scale <- 0
                
        }
        
        
        TimeIdx <- which(time.readable >= paste(cut.year,"-1-1",sep=""))
        par.val <- ncvar_get( ncFile, field.name)[ LonIdx, LatIdx,TimeIdx]
        lon.subset <- ncFile$dim$lon$val[LonIdx]
        lat.subset <- ncFile$dim$lat$val[LatIdx]
        time.subset <- ncFile$dim$time$val[TimeIdx]
        
        
        tmp.vec.long <- as.vector(par.val)
        val.as.df <-data.frame(tmp.vec.long)
        df.out <- expand.grid(lon.subset, lat.subset, time.subset)
        netcdf.for.export <- cbind(df.out, val.as.df)
        remove(val.as.df)
        if (outfield.name == "nothing"){ outfield.name  <- field.name}
        names(netcdf.for.export) <- c("lon", "lat","date",outfield.name)
        netcdf.for.export$time <- as.Date(chron(netcdf.for.export$date / div.num, origin=origin.date, out.format = "y-m-d"))
        netcdf.for.export$date <- NULL
        if (drop.na){
                netcdf.for.export <- netcdf.for.export[complete.cases(netcdf.for.export),] 
        }
        
        #outputing intermediate
        #write.csv(netcdf.for.export[which(netcdf.for.export$time == paste(cut.year,"-1-1",sep="")),],"ally2new.csv", row.names = F)
        
        #Creating subset netcdf output: works but need to switch the indices so it does not spread across the world
        if (write.netcdf){
                dim_x <- ncdim_def("lon","degrees_east",lon.subset-scale)
                dim_y <- ncdim_def("lat","degrees_north",lat.subset)
                dim_time <- ncdim_def("time",time.units,time.subset)
                var_out <- ncvar_def( field.name, units = field.units, longname= field.description, list(dim_x,dim_y,dim_time), -9999, prec="double")
                
                nc = nc_create(paste(field.name,"_subset.nc",sep = ""), var_out)
                ncvar_put(nc, var_out, par.val)
                nc_close(nc)
        }
        nc_close(ncFile)
        return(netcdf.for.export)
}

append.gridcell.values<-function(point.df,gridcells.spdf,x.coord = "lon", y.coord = "lat", time.field = "time", throw.out.nulls = TRUE, date.string = "%Y/%m/%d", weirdness = FALSE){
        if(FALSE){
                rm(list=ls(all=TRUE)) # clear memory
                setwd("E:\\GISWork_2\\Regan_Conflict\\2016-06-15_TotalRebuild")
                
                packages<- c("ncdf4","chron","rgdal","sp","reshape2","cmsaf","stringr") # list the packages that you'll need
                lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
                
                all.pre1980 <- read.csv("sm_nasa_pre1980.csv",stringsAsFactors = FALSE)
                all.pre1980$Year <- NULL
                all.pre1980$month <- NULL
                all.pre1980$lon <- ifelse(all.pre1980$lon > 180, all.pre1980$lon - 360, all.pre1980$lon) 
                
                grid.cells <- readOGR("Shapefiles", layer = "Grid_Cells_2d30s")
                
                x.coord = "lon"
                y.coord = "lat"
                
                point.df = precip2.post1980
                gridcells.spdf = grid.cells
                time.field = "time"
                date.string = "%Y/%m/%d"
                throw.out.nulls = TRUE
        }
        unique.latlons <- unique(point.df[,c(x.coord,y.coord)])
        # if(min(unique.latlons$lon)> 0){
        #         unique.latlons$lon <- unique.latlons$lon - 180
        #         point.df$lon<- point.df$lon-180
        #         print("moved coords")
        # }
        
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
        if (weirdness){
                out.data$dater <- as.Date(out.data[,time.field])
        }else{
                out.data$dater <- as.Date(out.data[,time.field], date.string)        
        }
        
        
        
        out.data$Year <- as.numeric(format(out.data$dater, format = "%Y"))
        out.data$month <-  as.numeric(format(out.data$dater, format = "%m"))
        out.data$dater<- NULL
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

write.large.csv <- function(df,out.name){
        first <- T
        for (i in  seq(1, nrow(df),1000000)){
                higher <- (i + 999999)
                if (higher > nrow(df)){
                        higher <- nrow(df)
                }
                print(paste(i,as.integer(higher)))
                if (first){
                        write.table(df[i:higher,],out.name,row.names = F, append = F,sep=",")
                        first <- F
                }else{
                        write.table(df[i:higher,],out.name,row.names = F, col.names = F, append = T,sep=",")
                }
        }
}




conflicts.custom.gridcells <- function(grid.cells.active = grid.cells.1.5x, conflicts.spdf.active= conflicts.spdf, intro.string = "_15"){
  if(FALSE){
    grid.cells.active = grid.cells.1.5x
    conflicts.spdf.active= conflicts.spdf
    intro.string = "_15"
  }
  
  conflicts <- conflicts.spdf.active@data
  conflicts <- conflicts[,c("uniq","gwno","lat","lon","date_start","type_of_vi")]
  
  conflicts.with.gridcell <- append.gridcell.values(point.df = conflicts, gridcells.spdf = grid.cells, time.field = "date_start", date.string =  "%Y/%m/%d")
  conflicts.with.gridcell$temp <- paste(conflicts.with.gridcell$Year, str_pad(conflicts.with.gridcell$month, 2, pad = "0"), sep='') #creates the year month field
  conflicts.with.gridcell$gymID <- paste(conflicts.with.gridcell$GridID, conflicts.with.gridcell$temp, sep='-') #creates the year month field
  conflicts.with.gridcell$temp <- NULL
  # conflicts.with.gridcell <- conflicts.with.gridcell[order("uniq"),]
  # row.names(grid.cells.active)<-grid.cells.active@data$Id
  contains.active <- as.data.frame(gContains(grid.cells.active,conflicts.spdf.active,byid=T))
  names(contains.active) <- grid.cells.active$Id
  row.names(contains.active) <- conflicts.spdf$uniq
  contains.list.active <- as.data.frame(apply(  contains.active, 1, function(u) paste( names(which(u)), collapse="," )))
  names(contains.list.active) <- "listactive"
  contains.list.active$uniq <- row.names(contains.list.active)
  
  contains.list.active <- merge (conflicts.with.gridcell,contains.list.active,by="uniq",all=T)
  
  
  # unspliting
  y<-strsplit(as.character( contains.list.active$listactive)  , ",", fixed=TRUE)
  contains.list.active.dedup <- data.frame(ID_sp= unlist(y), contains.list.active[ rep(1:nrow(contains.list.active), sapply(y, length)) ,  ] )
  
  
  
  conflict.types <- as.data.frame.matrix ( table ( contains.list.active.dedup$gymID, contains.list.active.dedup$type_of_vi ) [,] )
  conflict.types$gymID <- row.names(conflict.types)
  row.names(conflict.types) <- NULL
  conflict.types <- conflict.types[,c(4,1:3)]
  conflict.types$cnfts_t <- rowSums(conflict.types[2:4])
  names(conflict.types) <- c("gymID",paste(c("cnfts_1","cnfts_2","cnfts_3","cnfts_T"),intro.string,sep=""))
  
  return(conflict.types)
  
}


extended.gridcells <- FALSE
convert.necdfs <- FALSE
process.ECI.soilMoisture <- FALSE
aggrigate.environmental.data <- FALSE
create.env.stats.tables <- FALSE

netcdf.folder <- "E:\\GISWork_2\\Regan_Conflict\\NetCDF\\" 
grid.cells <- readOGR("Shapefiles", layer = "Grid_Cells_2d30s")
#1a). Processing the ESI Soil Moisture daily data

if (process.ECI.soilMoisture){
        
        #Local Variables
        
        esc.root.folder <- "E:\\GISWork_2\\Regan_Conflict\\NetCDF\\ESA_SoilMoisture\\"
        out.file <- "ESA_sm2_all_monthly.nc"
        temp.file <- "ESA_sm2_all_monthly_temp.nc"
        wd.old <- getwd()
        setwd(esc.root.folder)
        clip.and.merge.ECI.sm <- function(var.to.subset = "sm", root.path = "E:\\GISWork_2\\Regan_Conflict\\NetCDF\\ESA_SoilMoisture\\", year, lat.range=c(-40,55), lon.range = c(-30,70)){
                path <- paste(root.path,year,sep="")
                #print(year)
                # print (path)
                all.file <- paste(year,"_all_cut.nc",sep = "")
                monthly.file <- paste(year,"_monthly.nc",sep = "")
                box_mergetime(var = var.to.subset,path = path,"E*.nc",lon1 = lon.range[1],lon2 = lon.range[2],lat1=lat.range[1],lat2 = lat.range[2],all.file)
                monmean(var.to.subset, all.file, monthly.file)
                
        }
        #create an aggrigate daily file for each year
        for (each.year in years){
                print(each.year)
                clip.and.merge.ECI.sm(year = each.year)
        }
        #merge each yearly file into a massive daily file
        box_mergetime(var = "sm",path = getwd(),"_all_cut.nc",lon1 = lon.range[1],lon2 = lon.range[2],lat1=lat.range[1],lat2 = lat.range[2],"allDaily.nc")
        #calculate the montly mean for each the massive daily file (this is because the last timestamp is technically in the month before, so january/december might be messed up)
        monmean("sm", out.file, "all_monthly.nc")
        #reopen the file and rewrite it to ditch some variables from the date range        
        ncFile <- nc_open( temp.file )
        lon <- ncvar_get(ncFile, "lon")
        lat <- ncvar_get(ncFile, "lat")
        time <- ncvar_get(ncFile, "time")
        field.name <- names(ncFile$var)[[1]]
        field.description <- ncatt_get(ncFile,field.name , "long_name")$value
        field.units <-  ncatt_get(ncFile,field.name , "units")$value
        time.units <- ncatt_get(ncFile, 'time', "units")$value
        par.val <- ncvar_get( ncFile, field.name)
        
        
        
        dim_x <- ncdim_def("lon","degrees_east",lon)
        dim_y <- ncdim_def("lat","degrees_north",lat)
        dim_time <- ncdim_def("time",time.units,as.integer(time))
        var_out <- ncvar_def( field.name, units = field.units, longname= field.description, list(dim_x,dim_y,dim_time), -9999, prec="double")
        
        nc <- nc_create(paste(netcdf.folder, out.file,sep = ""), var_out)
        ncvar_put(nc, var_out, par.val)
        nc_close(nc)
        nc_close(ncFile)
        
        
        
        setwd(wd.old)
        
}


#1). converting netcdfs to csv




if (convert.necdfs){
        
        precip.data <- open.netcdf.return.df(file.name = paste(netcdf.folder,"precip.mon.mean.nc",sep = ""), outfield.name = "precip", cut.year = 1980, drop.na = F,write.netcdf = TRUE)
        write.csv(precip.data,"EnvironmentalData\\precip.csv",row.names = F)
        
        temp.data <- open.netcdf.return.df(file.name = paste(netcdf.folder,"air.2x2.1200.mon.anom.land.nc",sep = ""), outfield.name = "temp", cut.year = 1980, drop.na = T,write.netcdf = TRUE)
        write.csv(temp.data,"EnvironmentalData\\temp.csv",row.names = F)
        
        pdsi.data <- open.netcdf.return.df(file.name = paste(netcdf.folder,"pdsi.mon.mean.selfcalibrated.nc",sep = ""), outfield.name = "pdsi", cut.year = 1980, drop.na = T,write.netcdf = TRUE)
        write.csv(pdsi.data,"EnvironmentalData\\pdsi.csv",row.names = F)
        
        
        sm2.data <- open.netcdf.return.df(file.name = paste(netcdf.folder,"ESA_sm2_all_monthly.nc",sep = ""), outfield.name = "sm", cut.year = 1980, drop.na = T,write.netcdf = TRUE, funky.override = TRUE)
        #write.csv(sm2.data,"sm.csv",row.names = F)
        write.large.csv(df=sm2.data, out.name = "EnvironmentalData\\sm_ECI.csv")
        
}


#1a). Using the different soil moisture measure (creating the pre-1980 mean)
if (!file.exists("EnvironmentalData\\SM_nasa_table.csv")){
        if (!file.exists("EnvironmentalData\\sm_nasa_1948.csv")){
                sm.nasa.data.all <- open.netcdf.return.df(file.name = paste(netcdf.folder,"soilw.mon.mean.v2.nc",sep = ""), outfield.name = "soilw", cut.year = 1948, drop.na = F,write.netcdf = TRUE)
                # write.csv(sm2.data,"sm_nasa_1948.csv",row.names = F)
                sm.nasa.data.all$Year <- colsplit(sm.nasa.data.all$time,"-",c("year","month","day"))[,1]
                sm.nasa.data.all$month <- colsplit(sm.nasa.data.all$time,"-",c("year","month","day"))[,2]        
                
                
                
                
        }else{
                if (!file.exists("EnvironmentalData\\sm_nasa_pre1980.csv")& !file.exists("EnvironmentalData\\sm_nasa_post1980.csv")){
                        sm.nasa.data.all <- read.csv("EnvironmentalData\\sm_nasa_1948.csv",stringsAsFactors = FALSE)        
                        all.pre1980 <- sm.nasa.data.all[which(sm.nasa.data.all$Year<1980),]
                        
                        all.post1980 <- sm.nasa.data.all[which(sm.nasa.data.all$Year>=1980),]
                        
                        write.large.csv(df=all.pre1980, out.name = "EnvironmentalData\\sm_nasa_pre1980.csv")
                        write.large.csv(df=all.post1980, out.name = "EnvironmentalData\\sm_nasa_post1980.csv")
                }else{
                        all.pre1980 <- read.csv("EnvironmentalData\\sm_nasa_pre1980.csv",stringsAsFactors = FALSE)
                        all.post1980 <-read.csv("EnvironmentalData\\sm_nasa_post1980.csv",stringsAsFactors = FALSE)
                }
                
                
                
                
        }
        if (!file.exists("EnvironmentalData\\nasa_sm_monthlymean.csv")){
                all.pre1980$Year <- NULL
                all.pre1980$month <- NULL
                all.pre1980$lon <- ifelse(all.pre1980$lon > 180, all.pre1980$lon - 360, all.pre1980$lon) 
                
                
                
                all.pre1980.proc <- append.gridcell.values(point.df = all.pre1980,gridcells.spdf = grid.cells, weirdness = TRUE)
                all.pre1980.proc <- all.pre1980.proc[!is.na(all.pre1980.proc$GridID),]
                # all.prec1980.proc.ull <- unique(all.pre1980.proc[,c("lon","lat")])
                # write.csv(all.prec1980.proc.ull,"nasa_sm_test2.csv")
                all.pre1980.proc$GC_Month <- paste(all.pre1980.proc$GridID, str_pad(all.pre1980.proc$month, 2, pad = "0"), sep='-') #creates the year month field
                all.pre1980.proc <- all.pre1980.proc[!is.na(all.pre1980.proc$soilw),]
                
                monthy.mean <- aggregate(all.pre1980.proc$soilw, list(all.pre1980.proc$GC_Month), FUN=mean)
                names(monthy.mean)[names(monthy.mean) == 'x'] <- 'prec_pre1980monthlyMean'
                names(monthy.mean)[names(monthy.mean) == 'Group.1'] <- 'GC_Month'
                write.csv(monthy.mean,"EnvironmentalData\\nasa_sm_monthlymean.csv",row.names=F)
        }else{
                monthy.mean <- read.csv("EnvironmentalData\\nasa_sm_monthlymean.csv")
        }
        #processing the nasa soil moisture
        all.post1980.proc <- append.gridcell.values(point.df = all.post1980,gridcells.spdf = grid.cells)
        
        # write.large.csv(df = all.post1980.proc, out.name = "sm_nasa_post1980_proc.csv")
        
        # all.post1980.proc<-read.csv("EnvironmentalData\\sm_nasa_post1980_proc.csv",stringsAsFactors = F)
        all.post1980.proc <- all.post1980.proc[!is.na(all.post1980.proc$soilw),]
        #fixing the gymID field for months <10
        all.post1980.proc$temp <- paste(all.post1980.proc$Year, str_pad(all.post1980.proc$month, 2, pad = "0"), sep='') #creates the year month field
        all.post1980.proc$gymID <- paste(all.post1980.proc$GridID, all.post1980.proc$temp, sep='-') #creates the year month field
        all.post1980.proc$temp <- NULL
        
        sm.nasa.table <- createStatsTable(in.table= all.post1980.proc,summary.variable= "soilw",aggregate.variable="gymID")
        sm.nasa.table$GridID <- lapply(strsplit(as.character(sm.nasa.table$gymID), "-"), "[", 1)
        sm.nasa.table$GC_Month <- paste(sm.nasa.table$GridID, str_sub(sm.nasa.table$gymID, -2, -1), sep='-') #creates the year month field
        
        
        sm.nasa.table$gridId <- NULL
        
        sm.with.baseline <-merge(sm.nasa.table, monthy.mean,by="GC_Month", all.x=T)
        sm.with.baseline$sm_meanA <-  sm.with.baseline$soilw_mean - sm.with.baseline$prec_pre1980monthlyMean
        sm.with.baseline$sm_minA <-  sm.with.baseline$soilw_min - sm.with.baseline$prec_pre1980monthlyMean
        sm.with.baseline$sm_maxA <- sm.with.baseline$soilw_max - sm.with.baseline$prec_pre1980monthlyMean
        sm.with.baseline <- sm.with.baseline[,c("gymID", "soilw_mean","soilw_min","soilw_max","sm_meanA", "sm_minA","sm_maxA","soilw_n" )]
        names(sm.with.baseline)<-               c("gymID","sm_mean","sm_min","sm_max","smA_mean", "smA_min","smA_max","sm_n" )
        write.csv(sm.with.baseline,"EnvironmentalData\\SM_nasa_table.csv", row.names = F)
}else{
        sm.with.baseline<- read.csv("EnvironmentalData\\SM_nasa_table.csv", stringsAsFactors = FALSE)
}

#1b). Creating the precipitation anolaly measurement
#data: ftp://ftp.cdc.noaa.gov/Datasets/udel.airt.precip/precip.mon.total.v401.nc
#http://www.esrl.noaa.gov/psd/data/gridded/data.UDel_AirT_Precip.html
if (!file.exists("EnvironmentalData\\precip2_anomaly_table.csv")){
        if (!file.exists("EnvironmentalData\\precip2_dev_1948.csv")){
                precip2.data.all <-open.netcdf.return.df(file.name = paste(netcdf.folder,"precip.mon.total.v401.nc",sep = ""), outfield.name = "precip", cut.year = 1948, drop.na = F,write.netcdf = TRUE)
                
                #write.csv(precip2.data.all,"EnvironmentalData\\precip2_dev_1948.csv",row.names = F)
                
                precip2.data.all$Year <- colsplit(precip2.data.all$time,"-",c("year","month","day"))[,1]
                precip2.data.all$month <- colsplit(precip2.data.all$time,"-",c("year","month","day"))[,2]        
                write.large.csv(df=precip2.data.all, out.name = "EnvironmentalData\\precip2_dev_1948.csv")
                
                
                
                
        }else{
                if (!file.exists("EnvironmentalData\\precip2_dev_pre1980.csv")& !file.exists("EnvironmentalData\\precip2_dev_post1980.csv")){
                        precip2.data.all <- read.csv("EnvironmentalData\\precip2_dev_1948.csv",stringsAsFactors = FALSE)        
                        precip2.pre1980 <- precip2.data.all[which(precip2.data.all$Year<1980),]
                        
                        precip2.post1980 <- precip2.data.all[which(precip2.data.all$Year>=1980),]
                        
                        write.large.csv(df=precip2.pre1980, out.name = "EnvironmentalData\\precip2_dev_pre1980.csv")
                        #precip2.post1980$month <- colsplit(precip2.post1980$time,"-",c("year","month","day"))[,2]        
                        write.large.csv(df=precip2.post1980, out.name = "EnvironmentalData\\precip2_dev_post1980.csv")
                }else{
                        precip2.pre1980 <- read.csv("EnvironmentalData\\precip2_dev_pre1980.csv",stringsAsFactors = FALSE)
                        precip2.post1980 <-read.csv("EnvironmentalData\\precip2_dev_post1980.csv",stringsAsFactors = FALSE)
                }
                
                
                
                
        }
        if (!file.exists("EnvironmentalData\\precip2_monthlymean.csv")){
                precip2.pre1980$Year <- NULL
                precip2.pre1980$month <- NULL
                precip2.pre1980$lon <- ifelse(precip2.pre1980$lon > 180, precip2.pre1980$lon - 360, precip2.pre1980$lon) 
                
                
                
                precip2.pre1980.proc <- append.gridcell.values(point.df = precip2.pre1980,gridcells.spdf = grid.cells, weirdness = TRUE)
                precip2.pre1980.proc <- precip2.pre1980.proc[!is.na(precip2.pre1980.proc$GridID),]
                # all.prec1980.proc.ull <- unique(all.pre1980.proc[,c("lon","lat")])
                # write.csv(precip2.pre1980.proc,"prec_test2.csv")
                precip2.pre1980.proc$GC_Month <- paste(precip2.pre1980.proc$GridID, str_pad(precip2.pre1980.proc$month, 2, pad = "0"), sep='-') #creates the year month field
                precip2.pre1980.proc <- precip2.pre1980.proc[!is.na(precip2.pre1980.proc$precip),]
                
                monthy.mean.precip2 <- aggregate(precip2.pre1980.proc$precip, list(precip2.pre1980.proc$GC_Month), FUN=mean)
                names(monthy.mean.precip2)[names(monthy.mean.precip2) == 'x'] <- 'prec_pre1980monthlyMean'
                names(monthy.mean.precip2)[names(monthy.mean.precip2) == 'Group.1'] <- 'GC_Month'
                write.csv(monthy.mean.precip2,"EnvironmentalData\\precip2_monthlymean.csv",row.names=F)
        }else{
                prec.monthy.mean <- read.csv("EnvironmentalData\\precip2_monthlymean.csv")
                precip2.pre1980 <- NULL
        }
        
        #HERE LEFT OFF
        precip2.post1980$month <- NULL
        precip2.post1980$Year <- NULL
        
        precip2.post1980.proc <- append.gridcell.values(point.df = precip2.post1980,gridcells.spdf = grid.cells, weirdness = T)
        
        # write.large.csv(df = precip2.post1980.proc, out.name = "precip2_post1980_proc.csv")
        
        # all.post1980.proc<-read.csv("EnvironmentalData\\sm_nasa_post1980_proc.csv",stringsAsFactors = F)
        
        precip2.post1980.proc <- precip2.post1980.proc[!is.na(precip2.post1980.proc$precip),]
        # precip2.post1980.proc$month <- colsplit(precip2.post1980.proc$time,"-",c("year","month","day"))[,2]        
        
        #fixing the gymID field for months <10
        precip2.post1980.proc$temp <- paste(precip2.post1980.proc$Year, str_pad(precip2.post1980.proc$month, 2, pad = "0"), sep='') #creates the year month field
        precip2.post1980.proc$gymID <- paste(precip2.post1980.proc$GridID, precip2.post1980.proc$temp, sep='-') #creates the year month field
        precip2.post1980.proc$temp <- NULL
        
        prec.anom.table <- createStatsTable(in.table= precip2.post1980.proc,summary.variable= "precip",aggregate.variable="gymID")
        prec.anom.table$GridID <- lapply(strsplit(as.character(prec.anom.table$gymID), "-"), "[", 1)
        prec.anom.table$GC_Month <- paste(prec.anom.table$GridID, str_sub(prec.anom.table$gymID, -2, -1), sep='-') #creates the year month field
        
        
        prec.anom.table$GridID <- NULL
        
        prec2.with.baseline <-merge(prec.anom.table, prec.monthy.mean,by="GC_Month", all.x=T)
        prec2.with.baseline$prec2_meanA <-  prec2.with.baseline$precip_mean - prec2.with.baseline$prec_pre1980monthlyMean
        prec2.with.baseline$prec2_minA <-  prec2.with.baseline$precip_min - prec2.with.baseline$prec_pre1980monthlyMean
        prec2.with.baseline$prec2_maxA <- prec2.with.baseline$precip_max - prec2.with.baseline$prec_pre1980monthlyMean
        prec2.with.baseline <- prec2.with.baseline[,c("gymID", "precip_mean","precip_min","precip_max","prec2_meanA", "prec2_minA","prec2_maxA","precip_n" )]
        names(prec2.with.baseline)<-               c("gymID", "prec2_mean","prec2_min","prec2_max","prec2_meanA", "prec2_minA","prec2_maxA","prec2_n" )
        write.csv(prec2.with.baseline,"EnvironmentalData\\precip2_anomaly_table.csv", row.names = F)
}else{
        prec2.with.baseline<- read.csv("EnvironmentalData\\precip2_anomaly_table.csv", stringsAsFactors = FALSE)
}
#2 adding the gridcell to the csvs

if (create.env.stats.tables){
        if (!file.exists("EnvironmentalData\\precip_table.csv")){
                precip.data <- read.csv("precip.csv", stringsAsFactors = T)
                
                precip.data.processed <- append.gridcell.values(point.df = precip.data,gridcells.spdf = grid.cells)
                precip.table <- createStatsTable(in.table= precip.data.processed,summary.variable= "precip",aggregate.variable="gymID")
                write.csv(precip.table,"EnvironmentalData\\precip_table.csv", row.names = F)
                
        } else{
                precip.table <- read.csv("EnvironmentalData\\precip_table.csv")
        }
        
        if (!file.exists("EnvironmentalData\\temp_table.csv")){
                temp.data <- read.csv("EnvironmentalData\\temp.csv", stringsAsFactors = T)
                
                temp.data.processed <- append.gridcell.values(point.df = temp.data,gridcells.spdf = grid.cells)
                temp.table <- createStatsTable(in.table= temp.data.processed,summary.variable= "temp",aggregate.variable="gymID")
                write.csv(temp.table,"EnvironmentalData\\temp_table.csv", row.names = F)
        }else{
                temp.table <- read.csv("EnvironmentalData\\temp_table.csv")
        }
        
        if (!file.exists("EnvironmentalData\\pdsi_table.csv")){
                pdsi.data <- read.csv("EnvironmentalData\\pdsi.csv", stringsAsFactors = T)
                
                pdsi.data.processed <- append.gridcell.values(point.df = pdsi.data,gridcells.spdf = grid.cells)
                pdsi.table <- createStatsTable(in.table= pdsi.data.processed,summary.variable= "pdsi",aggregate.variable="gymID")
                write.csv(pdsi.table,"EnvironmentalData\\pdsi_table.csv", row.names = F)
        }else{
                pdsi.table <- read.csv("EnvironmentalData\\pdsi_table.csv")
        }
        if (!file.exists("EnvironmentalData\\sm_ECI_table.csv")){
                sm.data <- read.csv("EnvironmentalData\\sm_ECI.csv", stringsAsFactors = T)
                
                sm.data.processed <- append.gridcell.values(point.df = sm.data,gridcells.spdf = grid.cells)
                
                first <- T
                for (i in  seq(1, nrow(sm.data.processed),1000000)){
                        higher <- (i + 999999)
                        if (higher > nrow(sm.data.processed)){
                                higher <- nrow(sm.data.processed)
                        }
                        print(paste(i,as.integer(higher)))
                        if (first){
                                write.table(sm.data.processed[i:higher,],"sm_ECI_procc.csv",row.names = F, append = F,sep=",")
                                first <- F
                        }else{
                                write.table(sm.data.processed[i:higher,],"sm_ECI_procc.csv",row.names = F, col.names = F, append = T,sep=",")
                        }
                }
                remove(i, higher, first)
                sm.data.processed <- read.csv("EnvironmentalData\\sm_ECI_procc.csv", stringsAsFactors = T)
                
                
                sm.table <- createStatsTable(in.table= sm.data.processed,summary.variable= "sm",aggregate.variable="gymID")
                write.csv(sm.table,"EnvironmentalData\\sm_ECI_table.csv", row.names = F)
        }else{
                sm.table <- read.csv("EnvironmentalData\\sm_ECI_table.csv")
        } 
}


#3 Aggrigating, merging and duplicating on gwno
if (aggrigate.environmental.data){
        table.gwno <- grid.cells@data
        table.gwno <- table.gwno[complete.cases(table.gwno),]
        # write.csv(table.gwno,"table_GWNO.CSV")
        table.gwno <- table.gwno[,c("Id","All_GWNO")]
        names(table.gwno) <- c("GridID", "All_GWNO")
        
        
        
        
        master.table <- merge(precip.table,pdsi.table,by="gymID", all=T)
        master.table <- merge(master.table,temp.table,by="gymID", all=T)
        master.table <- merge(master.table,sm.with.baseline,by="gymID", all=T)
        master.table <- merge(master.table,prec2.with.baseline, by="gymID", all=T)
        
        master.table$GridID <- colsplit(master.table$gymID,"-",c("GridID","YearMonth"))[,1]
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
        write.csv(master.table02, "EnvironmentalData\\EnvironmentalVariables_NoDuplication.csv", row.names = FALSE)
        env.nodup <- master.table02
        
        
        
}else{
        env.nodup <- read.csv("EnvironmentalData\\EnvironmentalVariables_NoDuplication.csv", stringsAsFactors =  FALSE)
        # env.withdup <-read.csv("EnvironmentalVariables_WithDuplication.csv", stringsAsFactors =  FALSE)
        
}

#Greating single country grids filter
grid.cell.details <- grid.cells@data
#grid.cell.details$sing_filter <- str_length(grid.cell.details$All_GWNO)
single.country.grids <- grid.cell.details[ which(str_length(grid.cell.details$All_GWNO)==3),]
single.country.grids$single_filer <- 1
single.country.grids <- single.country.grids[,c(1:3,5)]
names(single.country.grids)[names(single.country.grids) == 'Id']    <- 'GridID'
rownames(single.country.grids) <- c()
# merged.with.filter <- merge(all.data.old,single.country.grids,by="gridID", all.x = T)
# merged.with.filter$single_filer[is.na(merged.with.filter$single_filer)] <- 0


#Processing Polity data
polity4 <- read.csv("DataTables\\p4v2014.csv")
polity4 <- polity4[ which(polity4$year>=1980), c(2,5,8:11)] #drops the unwanted vatiables
rownames(polity4) <- c()


polity4$cc_year <- paste(polity4$ccode, polity4$year, sep='-')
polity4 <- polity4[ , c(3:7)]
rownames(polity4) <- c()

#Protest Data: the actually processing can be found in the file Protest_in_R.R
protest <- read.csv("DataTables\\Protest_Data_By_gym.csv",stringsAsFactors = FALSE)
protest$X <- NULL
protest <- protest[ which(protest$gymID != "-201211"), ]
protest <- protest[ which(substr(protest$gymID,1,2) != "NA"), ]


#Conflict Data
conflicts <- read.csv("DataTables\\georeferenced conflict data.csv",stringsAsFactors = F)
conflicts <- conflicts[,c("gwno","lat","lon","date_start","type_of_violence")]
conflicts.with.gridcell <- append.gridcell.values(point.df = conflicts, gridcells.spdf = grid.cells, time.field = "date_start", date.string =  "%m/%d/%Y")
#Fixing erronous gymID
conflicts.with.gridcell$temp <- paste(conflicts.with.gridcell$Year, str_pad(conflicts.with.gridcell$month, 2, pad = "0"), sep='') #creates the year month field
conflicts.with.gridcell$gymID <- paste(conflicts.with.gridcell$GridID, conflicts.with.gridcell$temp, sep='-') #creates the year month field
conflicts.with.gridcell$temp <- NULL


conflict.types <- data.frame ( table ( conflicts.with.gridcell$gymID, conflicts.with.gridcell$type_of_violence ) [,] )
names(conflict.types) <- c("cnfts_1","cnfts_2","cnfts_3")
conflict.types$gymID <- row.names(conflict.types)
row.names(conflict.types) <- NULL
conflict.types <- conflict.types[,c(4,1:3)]
conflict.types$cnfts_t <- conflict.types$cnfts_1 + conflict.types$cnfts_2 + conflict.types$cnfts_3

# conflics.gym.with.IDs <- aggregate(gwno~gymID,paste,collapse=" ",data=conflicts.with.gridcell)


conflics.gym.with.IDs <- by(conflicts.with.gridcell$gwno,conflicts.with.gridcell$gymID,function(x)paste(unique(x),collapse=" "))
conflics.gym.with.IDs <- data.frame(id=c(conflics.gym.with.IDs))
conflics.gym.with.IDs$gymID <- row.names(conflics.gym.with.IDs)
row.names(conflics.gym.with.IDs) <- NULL
names(conflics.gym.with.IDs) <- c("conflict_gwmo","gymID")
conflict.types <- merge(conflict.types, conflics.gym.with.IDs, by="gymID", all.x = T)

remove(conflicts, conflicts.with.gridcell,conflics.gym.with.IDs)

#Soil Data
soil.variables <- read.dbf("DataTables\\all_Soilvariables.dbf")
soil.variables <- soil.variables[,c("GridID","CLYPPT1_Me","SLTPPT1_Me","SNDPPT1_Me")]

#Capacity and Sensitivity
iso.table2 <- read.csv("DataTables\\ISO_GWNO_02.csv",stringsAsFactors=FALSE)
names(iso.table2)[names(iso.table2) == 'STATEABB'] <- 'ISO3'

adaptation.vars <- read.csv("DataTables\\AdaptationVariables.csv",stringsAsFactors=FALSE)
adaptation.vars$cc_year <- paste(adaptation.vars$CC, adaptation.vars$Year, sep='-')

adaptation.vars <- adaptation.vars[,c("cc_year","EXPOSURE","VULNERABIL")]
adaptation.vars[adaptation.vars == 0] <- NA

capacity.raw <- read.csv("DataTables\\capacity_processed.csv")
capacity.raw$merg <- paste(capacity.raw$ISO3,capacity.raw$Year)
sensitvity.raw <- read.csv("DataTables\\sensitivity_processed.csv")
sensitvity.raw$merg <- paste(sensitvity.raw$ISO3,sensitvity.raw$Year)
sensitvity.raw$ISO3 <- NULL
sensitvity.raw$Year <- NULL
capacity.raw <- merge(capacity.raw, sensitvity.raw, by="merg")

capacity.raw <- merge(capacity.raw,iso.table2, by= "ISO3")
capacity.raw$cc_year <- paste(capacity.raw$CCODE, capacity.raw$Year, sep='-') #creates the GridYear field

capacity.raw <- capacity.raw[,c("cc_year", "capacity","sensitivity")]

adaptation.vars <-merge(adaptation.vars,capacity.raw,by="cc_year",all=T)
adaptation.vars <- adaptation.vars[!with(adaptation.vars,is.na(adaptation.vars$EXPOSURE)& is.na(adaptation.vars$VULNERABIL)& is.na(adaptation.vars$capacity)),]
remove(iso.table2,capacity.raw)
#Old Other Variables: Merged after duplication o
demographic.data <- read.csv("DataTables\\climateAndConflictDemographicData.csv", stringsAsFactors = F)
demographic.data <- unique(demographic.data)
# write.csv(demographic.data, "climateAndConflictDemographicData.csv", row.names = F)
demographic.data$cc_year <- paste(demographic.data$ccode, demographic.data$year, sep='-')

demographic.data <- demographic.data[which(demographic.data$year>=1980),]

demographic.data <- demographic.data[,c("ccode","cc_year","degdppc","demortunder5","deruralpoppct")]


#aridity zones
aridity.zones <- read.csv("DataTables\\AridityZones.csv", stringsAsFactors = F)

#MERGING!
remove(grid.cell.details, sm.with.baseline)
#BaseData: ENV.NoDup
final.nodup <- env.nodup
#single.country.grids: GridID
final.nodup <-merge(final.nodup,single.country.grids,by="GridID", all = T)
final.nodup$single_filer <- ifelse(is.na(final.nodup$single_filer), 0, final.nodup$single_filer)
#soil.variables: GridID
final.nodup <-merge(final.nodup,soil.variables,by="GridID", all = T)
final.nodup<- final.nodup[which(!is.na(final.nodup$gymID)),] #Tossing a few places that have soil measurements but nothing else

#Aridity Zones: GridID
final.nodup <-merge(final.nodup,aridity.zones,by="GridID", all = T)

#Conflict Types: gymID
final.nodup <-merge(final.nodup,conflict.types,by="gymID", all = T)


#protest: gymID:
final.nodup <-merge(final.nodup,protest,by="gymID", all = T)

final.nodup<- final.nodup[which(!is.na(final.nodup$GridID)),] #Tossing a few places that have conflicts or protests but no environmental data

#Dedup
y<-strsplit(as.character( final.nodup$All_GWNO)  , " ", fixed=TRUE)
final.duped <- data.frame(GWNO_sing= unlist(y), final.nodup[ rep(1:nrow(final.nodup), sapply(y, length)) ,  ] )
#build year field for cc_year
final.duped$ym <- colsplit(final.duped$gymID,"-",c("gridID","ym"))[,2]
final.duped$Year <- substr(final.duped$ym,1,4)
#cc_year
final.duped$cc_year <- paste(final.duped$GWNO_sing,final.duped$Year,sep="-")

# retry <- unique(final.duped[,2:25])

#polity4: ccYear: Needs to happen after duplication
final.duped <-merge(final.duped,polity4,by="cc_year", all.x = T) #all.x throws out non African countries

#demographic.data: ccYear: Needs to happen after duplication
final.duped$GWNO_sing <- as.character(final.duped$GWNO_sing)


final.duped <-merge(final.duped,demographic.data,by="cc_year", all.x = T) #all.x throws out non African countries
#Adaptation Variables using ccYEar: Needs to happen after duplication

final.duped <-merge(final.duped,adaptation.vars,by="cc_year", all.x = T) #all.x throws out non African countries
final.duped$cnfts_t[which(is.na(final.duped$cnfts_t))]<-0
final.duped$cnfts_1[which(is.na(final.duped$cnfts_1))]<-0
final.duped$cnfts_2[which(is.na(final.duped$cnfts_2))]<-0
final.duped$cnfts_3[which(is.na(final.duped$cnfts_3))]<-0
final.duped$month <- final.duped$ym - as.integer(final.duped$Year)*100 
final.duped$ym <- NULL
write.csv(final.duped,"2016-07-09_TotalRebuild.csv", row.names = FALSE)
write.dta(final.duped, "2016-07-09_TotalRebuild.dta")



if(extended.gridcells){
  grid.cells <- readOGR(dsn= ".", layer = "Grid_Cells_2d30s")
  grid.cells.2x <- readOGR(dsn=".",layer = "Grid_Cells_2d30m_2d30mBuffer")
  grid.cells.1.5x <- readOGR(dsn=".",layer = "Grid_Cells_2d30s_1d15mBuffer")
  conflicts.spdf <- readOGR(dsn=".", layer = "ConflictData")
  
  
  
  contains.2x <-  conflicts.custom.gridcells(grid.cells.active = grid.cells.2x, intro.string = "_2x")
  
  contain.1.5x <- conflicts.custom.gridcells(grid.cells.active = grid.cells.1.5x, intro.string = "_1.5x")
  contains.both <- merge(contains.2x,contain.1.5x,by="gymID",all=T)
  write.csv(contains.both,"2017-07-18_gymIDExtendedConflicts.csv",row.names=F)
  write.dta(contains.both, "2017-07-18_gymIDExtendedConflicts.dta")
  
}
