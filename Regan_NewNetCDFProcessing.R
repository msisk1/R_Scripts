rm(list=ls(all=TRUE)) # clear memory

packages<- c("ncdf4","chron") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

#setwd("/mnt/smb/Research/OTool_Distances")
setwd("E:\\GISWork_2\\Regan_Conflict\\2016-04-21_worldClimate")

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
        time.binary <- (time.readable > paste(cut.year,"-1-1",sep=""))
        time.binary
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

#1). converting netcdfs to csv
precip.data <- open.netcdf.return.df(file.name = "NetCDFs\\precip.mon.mean.nc", outfield.name = "precip", cut.year = 1980)
write.csv(precip.data,"precipitation.csv",row.names = F)

temp.data <- open.netcdf.return.df(file.name = "NetCDFs\\air.2x2.1200.mon.anom.land.nc", outfield.name = "temp", cut.year = 1980)
write.csv(temp.data,"temp.csv",row.names = F)

pdsi.data <- open.netcdf.return.df(file.name = "NetCDFs\\pdsi.mon.mean.selfcalibrated.nc", outfield.name = "pdsi", cut.year = 1980)
write.csv(pdsi.data,"pdsi.csv",row.names = F)

smOLD.data <- open.netcdf.return.df(file.name = "NetCDFs\\soilw.mon.mean.v2.nc", outfield.name = "smOLD", cut.year = 1980, drop.na = T)
#write.csv(smOLD.data,"soilmoisture_old.csv",row.names = F)
for (i in  seq(1, nrow(smOLD.data),1000000)){
        write.table(smOLD.data[i:(i + 999999),],"soilmoisture_old.csv",row.names = F, append = T,sep=",")
}

#2 adding the gridcell to the csvs

precip.data <- open.csv("precipitation.csv")

