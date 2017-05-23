# install.packages(("gdistance"))
rm(list=ls(all=TRUE)) # clear memory



packages<- c("rgdal","sp","ggplot2","plyr","foreign","rgeos") # list the packages that you'll need

lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first

setwd("E:/GISWork_2/Ruiz_MexicoDistances") #Linux machine

# graph.data.raw <- read.dbf("170510_MexicanMunicipalitiesGraph.dbf", as.is = T)
graph.data.raw <- read.csv("170510_MexicanMunicipalitiesGraph3.csv", stringsAsFactors = F,fileEncoding = "UTF-8")
unique(graph.data.raw$state)


graph.data <- graph.data.raw[,c(1,2,6,7,8,9,10,11)]
names(graph.data) <- c("id_mun","year","state","Include","Reform","HomRate","OgCrimHR","Capital")


graph.data$Reform <- factor(graph.data$Reform)
levels(graph.data$Reform )[levels(graph.data$Reform )=="0"] <- "No"
levels(graph.data$Reform )[levels(graph.data$Reform )=="1"] <- "Yes"

list.of.years <- unique(graph.data$year)



graph.nodups <- unique(graph.data[,c("id_mun","state","Include","Capital")])
graph.data <- graph.data[,c(1,2,5,6,7)]

graph.nodups <- merge(graph.nodups,reshape(graph.data,idvar = "id_mun",timevar = "year", direction="wide"), by="id_mun")

if (file.exists("mexico_mun_simp.shp")){
        mexico.munc <- readOGR(dsn=".",layer="mexico_mun_simp",stringsAsFactors = F)
}else{
        mexico.munc.raw <- readOGR(dsn="702825217341_s/conjunto_de_datos",layer="areas_geoestadisticas_municipales",stringsAsFactors = F)
        mexico.munc <- ms_simplify(mexico.munc.raw)
        remove(mexico.munc.raw)
        mexico.munc@data$id_mun <- as.integer(paste(mexico.munc@data$CVE_ENT,mexico.munc@data$CVE_MUN,sep=""))
        writeOGR(obj = mexico.munc, dsn= ".", layer = "mexico_mun_simp", driver="ESRI Shapefile",overwrite_layer=T) #writes the spatial points data frame as a shapefile
}
mexico.munc <-merge(mexico.munc,graph.nodups,by="id_mun", all=T)

state.list <- c("Chihuahua","Morelos", "Estado de México","Oaxaca")


#Fixing the Include Variable
on.in.2014 <- c("Aguascalientes","Coahuila","Nayarit","Tamaulipas","Tlaxcala")
# qdap <- mexico.munc@data



pdf("ReformMaps.pdf")
# name <- state.list[1]
for (name in state.list){
        print(name)
        mexico.munc.subset <- mexico.munc[mexico.munc$state %in% c(name),]
        # plot(mexico.munc.subset)
        caps<- (list(mexico.munc.subset[which(mexico.munc.subset$Capital == 1),], fill=NA,lwd=2,col="red", first=FALSE))
        a <- spplot(mexico.munc.subset,c("Reform.2008","Reform.2009","Reform.2006","Reform.2007"),col.regions=c("white","grey"),sp.layout = c(caps),
               main = paste("Reform: ",name,sep=""),
               names.attr = c("2008","2009","2006","2007"))
        print(a)
        b <- spplot(mexico.munc.subset,c("Reform.2012","Reform.2013","Reform.2010","Reform.2011"),col.regions=c("white","grey"),sp.layout = c(caps),
                    main = paste("Reform: ",name,sep=""),
                    names.attr = c("2012","2013","2010","2011"))
        print(b)
        c <- spplot(mexico.munc.subset,c("Reform.2014","Reform.2015"),col.regions=c("white","grey"),sp.layout = c(caps),
                    main = paste("Reform: ",name,sep=""),
                    names.attr = c("2014","2015"))
        print(c)
}
dev.off()
years <- 2006:2015
HR.splits <- c(-1,0,20,40,60,80,100,900)
HR.labels <- c("0","0 - 20","20 - 40","40 - 60","60 - 80","80 - 100","> 100")

pdf("HomicideMaps.pdf")
for (year in years){
        print(year)
        active.field <- paste("Reform.",year,sep="")
        yes<- (list(mexico.munc[which(mexico.munc@data[,active.field] == "Yes"),], fill=NA,lwd=.5,col="red", first=FALSE))
        if (year>=2014){
                not.incl<- (list(mexico.munc[which(mexico.munc@data$Include == 0),], fill=NA,lwd=.5,col="green", first=FALSE))                
        }else{
                not.incl<- (list(mexico.munc[which(mexico.munc@data$state == "Nuevo León"),], fill=NA,lwd=.5,col="green", first=FALSE))        
        }
        

        
        #Reclassing to create categorical variable for plotting
        mexico.munc$gen.hr <- cut(mexico.munc@data[,paste("HomRate.",year,sep="")], HR.splits, labels = HR.labels)
        
        mexico.munc$gen.og <- cut(mexico.munc@data[,paste("OgCrimHR.",year,sep="")], HR.splits, labels = HR.labels)
        d <- spplot(mexico.munc,c("gen.og", "gen.hr"),sp.layout = list(yes,not.incl),
                    main = paste(year),
                    # legend("bottomright", legend=c("a","b","c","d"), pch=18, col=c("yellow","green","blue","red")),
                    names.attr = c("Homicide Rate","Organized Crime Homicide Rate"))         
        print(d)
        #  ORIGINAL
        # d <- spplot(mexico.munc,c(paste("OgCrimHR.",year,sep=""), paste("HomRate.",year,sep="") ),sp.layout = list(yes,not.incl),
        #             main = paste(year),
        #             at = c(0,20,40,60,80,100,900),
        #             # cuts = c(0,20,40,60,80,100),
        #             # zlim=c(0,100),
        #             names.attr = c("Homicide Rate","Organized Crime Homicide Rate"))
        # print(d)
}
dev.off()
