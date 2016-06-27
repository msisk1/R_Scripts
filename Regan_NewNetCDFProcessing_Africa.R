rm(list=ls(all=TRUE)) # clear memory

packages<- c("ncdf4","chron","rgdal","sp","reshape2","cmsaf","stringr", "foreign") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd("/mnt/smb/Research/OTool_Distances")
setwd("E:\\GISWork_2\\Regan_Conflict\\2016-06-15_TotalRebuild")

# cc_year GridYear gymID gridID id_all Year Month lat lon  ccode gwnos_conflict 
#  clyppt1_mean sltppt1_mean sndppt1_mean  



#Functions
open.netcdf.return.df<-function(file.name, outfield.name = "nothing", cut.year = 1980, drop.na = FALSE,lat.range=c(-40,55),lon.range = c(-30,70),write.netcdf = FALSE, funky.override = FALSE){
        if (FALSE){
                
                
                
                file.name = paste(netcdf.folder,"ESA_sm2_all_monthly.nc",sep = "")
                
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





convert.necdfs <- FALSE
process.ECI.soilMoisture <- FALSE
aggrigate.environmental.data <- FALSE
create.env.stats.tables <- FALSE

#1a). Processing the ESI Soil Moisture daily data
netcdf.folder <- "E:\\GISWork_2\\Regan_Conflict\\NetCDF\\" 

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
        write.csv(precip.data,"precip.csv",row.names = F)
        
        temp.data <- open.netcdf.return.df(file.name = paste(netcdf.folder,"air.2x2.1200.mon.anom.land.nc",sep = ""), outfield.name = "temp", cut.year = 1980, drop.na = T,write.netcdf = TRUE)
        write.csv(temp.data,"temp.csv",row.names = F)
        
        pdsi.data <- open.netcdf.return.df(file.name = paste(netcdf.folder,"pdsi.mon.mean.selfcalibrated.nc",sep = ""), outfield.name = "pdsi", cut.year = 1980, drop.na = T,write.netcdf = TRUE)
        write.csv(pdsi.data,"pdsi.csv",row.names = F)
        
        
        sm2.data <- open.netcdf.return.df(file.name = paste(netcdf.folder,"ESA_sm2_all_monthly.nc",sep = ""), outfield.name = "sm", cut.year = 1980, drop.na = T,write.netcdf = TRUE, funky.override = TRUE)
        #write.csv(sm2.data,"sm.csv",row.names = F)
        first <- T
        for (i in  seq(1, nrow(sm2.data),1000000)){
                higher <- (i + 999999)
                if (higher > nrow(sm2.data)){
                        higher <- nrow(sm2.data)
                }
                print(paste(i,as.integer(higher)))
                if (first){
                        write.table(sm2.data[i:higher,],"sm_ECI.csv",row.names = F, append = F,sep=",")
                        first <- F
                }else{
                        write.table(sm2.data[i:higher,],"sm_ECI.csv",row.names = F, col.names = F, append = T,sep=",")
                }
        }
        remove(i, higher, first)
}


#2 adding the gridcell to the csvs
append.gridcell.values<-function(point.df,gridcells.spdf,x.coord = "lon", y.coord = "lat", time.field = "time", throw.out.nulls = TRUE, date.string = "%Y/%m/%d"){
        if(FALSE){
                rm(list=ls(all=TRUE)) # clear memory
                
                packages<- c("ncdf4","chron","rgdal","sp","reshape2","cmsaf","stringr") # list the packages that you'll need
                lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
                
                #setwd("/mnt/smb/Research/OTool_Distances")
                setwd("E:\\GISWork_2\\Regan_Conflict\\2016-06-15_TotalRebuild")
                conflicts.all <- read.csv("DataTables\\georeferenced conflict data.csv",stringsAsFactors = F)
                conflicts <- conflicts.all[,c("gwno","lat","lon","date_start","type_of_violence")]
                grid.cells <- readOGR("Shapefiles", layer = "Grid_Cells_2d30s")
                
                x.coord = "lon"
                y.coord = "lat"
                
                point.df = conflicts
                gridcells.spdf = grid.cells
                time.field = "date_start"
                date.string =  "%m/%d/%Y"
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
        out.data$dater <- as.Date(out.data[,time.field], date.string)
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

grid.cells <- readOGR("Shapefiles", layer = "Grid_Cells_2d30s")
if (create.env.stats.tables){
        if (!file.exists("precip_table.csv")){
                precip.data <- read.csv("precip.csv", stringsAsFactors = T)
                
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
        if (!file.exists("sm_ECI_table.csv")){
                sm.data <- read.csv("sm_ECI.csv", stringsAsFactors = T)
                
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
                sm.data.processed <- read.csv("sm_ECI_procc.csv", stringsAsFactors = T)
                
                
                sm.table <- createStatsTable(in.table= sm.data.processed,summary.variable= "sm",aggregate.variable="gymID")
                write.csv(sm.table,"sm_ECI_table.csv", row.names = F)
        }else{
                sm.table <- read.csv("sm_ECI_table.csv")
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
        master.table <- merge(master.table,sm.table,by="gymID", all=T)
        
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
        write.csv(master.table02, "EnvironmentalVariables_NoDuplication.csv", row.names = FALSE)
        env.nodup <- master.table02
        
        #Splitting off into the 
        y<-strsplit(as.character( master.table02$All_GWNO)  , " ", fixed=TRUE)
        final.data <- data.frame(GWNO_sing= unlist(y), master.table02[ rep(1:nrow(master.table02), sapply(y, length)) , -1 ] )
        end.withdup <- final.data
        
        write.csv(final.data, "EnvironmentalVariables_WithDuplication.csv", row.names = FALSE)
        
}else{
        env.nodup <- read.csv("EnvironmentalVariables_NoDuplication.csv", stringsAsFactors =  FALSE)
        end.withdup <-read.csv("EnvironmentalVariables_WithDuplication.csv", stringsAsFactors =  FALSE)
        
}

#Greating single country grids filter
grid.cell.details <- grid.cells@data
#grid.cell.details$sing_filter <- str_length(grid.cell.details$All_GWNO)
single.country.grids <- grid.cell.details[ which(str_length(grid.cell.details$All_GWNO)==3),]
single.country.grids$single_filer <- 1
single.country.grids <- single.country.grids[,c(1,5)]
names(single.country.grids)[names(single.country.grids) == 'Id']    <- 'gridID'
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
protest <- read.csv("DataTables\\Protest_Data_By_gym.csv")

#Conflict Data
conflicts <- read.csv("DataTables\\georeferenced conflict data.csv",stringsAsFactors = F)
conflicts <- conflicts[,c("gwno","lat","lon","date_start","type_of_violence")]
conflicts.with.gridcell <- append.gridcell.values(point.df = conflicts, gridcells.spdf = grid.cells, time.field = "date_start", date.string =  "%m/%d/%Y")
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

#OR
# conflict.types <- read.csv("DataTables\\2d30s_Monthly_conflictClasses.csv")


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
capacity.raw <- merge(capacity.raw,iso.table2, by= "ISO3")
capacity.raw$cc_year <- paste(capacity.raw$CCODE, capacity.raw$Year, sep='-') #creates the GridYear field

capacity.raw <- capacity.raw[,c("cc_year", "capacity")]

adaptation.vars <-merge(adaptation.vars,capacity.raw,by="cc_year",all=T)
adaptation.vars <- adaptation.vars[!with(adaptation.vars,is.na(adaptation.vars$EXPOSURE)& is.na(adaptation.vars$VULNERABIL)& is.na(adaptation.vars$capacity)),]
remove(iso.table2,capacity.raw)
