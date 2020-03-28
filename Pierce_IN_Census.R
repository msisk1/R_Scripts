#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("tidyverse","sf") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("E:\\GISWork_2\\Pierce_IN_Census") 


csv <- list.files(path = "nhgis0039_csv", pattern = "*.csv$")
shp <- list.files(path = "nhgis0039_shape", pattern = "*.shp$")
var.names <- read_csv("Annual_Fields.csv")
process.all <- FALSE

lookup_rename <- function(df, column_lookup, drop.other = F) {
  if (drop.other){
    # df<- df %>% select(-one_of(drop.cols))
    df <- df %>% select(c("DECADE","NHGISNAM","GISJOIN",attr(df,"sf_column"),column_lookup$variable))
  }
  
   for (n in 1:nrow(column_lookup)){
    if (column_lookup[n,"variable"] %in% names(df)){
      names(df)[names(df) == paste(column_lookup[n,"variable"])] <- paste(column_lookup[n,"name"])  
    }
   }

  return(df)
}

if (process.all){
  years <- seq(1820,2000, 10)
  for (each.year in years){
    csv.name <- csv[str_detect(csv, as.character(each.year))]
    shp.name <-  shp[str_detect(shp, as.character(each.year))]
    
    each.csv <- read_csv(paste0("nhgis0039_csv\\", csv.name))
    each.csv <- each.csv %>%
      filter(STATE == "Indiana")
    if (nrow(each.csv)>0){
      write_csv(each.csv, paste0("Indiana\\",csv.name))
    }
    each.shp <- st_read(paste0("nhgis0039_shape\\",shp.name), stringsAsFactors = F)
    each.shp <- each.shp %>%
      filter(STATENAM == "Indiana")
    if (nrow(each.shp)>0){
      st_write(each.shp, paste0("Indiana\\",shp.name), delete_layer = T)
    }
    # each.shp <- st_read(paste0("Indiana\\", shp[str_detect(shp, as.character(year))]), stringsAsFactors = F)
    each.joing <- left_join(each.shp, each.csv, by = "GISJOIN")
    # each.joing <- lookup_rename(each.joing,   var.names%>%filter(year == each.year))
    st_write(each.joing, paste0("Joins\\Census_",each.year,".shp"), delete_layer = T)
  }

  vars <- c("totalPop" = "P003001", 
            "white" =  "P003002",
            "black" = "P003003")
  c2010 <- get_decennial(geography = "county",variables = vars,year = 2010, state = "IN", geometry = T, output = "wide")
  st_write(c2010, "Joins\\Census_2010.shp", delete_layer = T)
}


c1820 <- st_read(paste0("Joins\\Census_",1820,".shp"), stringsAsFactors = F)
c1820 <- lookup_rename(c1820, var.names %>% filter(year == 1820), drop.other = T)


c1830 <- st_read(paste0("Joins\\Census_",1830,".shp"), stringsAsFactors = F)
c1830 <- lookup_rename(c1830, var.names %>% filter(year == 1830), drop.other = T)

c1840 <- st_read(paste0("Joins\\Census_",1840,".shp"), stringsAsFactors = F)
c1840 <- lookup_rename(c1840, var.names %>% filter(year == 1840), drop.other = T)

c1850 <- st_read(paste0("Joins\\Census_",1850,".shp"), stringsAsFactors = F)
c1850 <- lookup_rename(c1850, var.names %>% filter(year == 1850), drop.other = T)

c1860 <- st_read(paste0("Joins\\Census_",1860,".shp"), stringsAsFactors = F)
c1860 <- lookup_rename(c1860, var.names %>% filter(year == 1860), drop.other = T)

c1870 <- st_read(paste0("Joins\\Census_",1870,".shp"), stringsAsFactors = F)
c1870 <- lookup_rename(c1870, var.names %>% filter(year == 1870), drop.other = T)

c1880 <- st_read(paste0("Joins\\Census_",1880,".shp"), stringsAsFactors = F)
c1880 <- lookup_rename(c1880, var.names %>% filter(year == 1880), drop.other = T)

c1890 <- st_read(paste0("Joins\\Census_",1890,".shp"), stringsAsFactors = F)
c1890 <- lookup_rename(c1890, var.names %>% filter(year == 1890), drop.other = T)

c1900 <- st_read(paste0("Joins\\Census_",1900,".shp"), stringsAsFactors = F)
c1900 <- lookup_rename(c1900, var.names %>% filter(year == 1900), drop.other = T)

c1910 <- st_read(paste0("Joins\\Census_",1910,".shp"), stringsAsFactors = F)
c1910 <- lookup_rename(c1910, var.names %>% filter(year == 1910), drop.other = T)

c1920 <- st_read(paste0("Joins\\Census_",1920,".shp"), stringsAsFactors = F)
c1920 <- lookup_rename(c1920, var.names %>% filter(year == 1920), drop.other = T)

c1930 <- st_read(paste0("Joins\\Census_",1930,".shp"), stringsAsFactors = F)
c1930 <- lookup_rename(c1930, var.names %>% filter(year == 1930), drop.other = T)

c1940 <- st_read(paste0("Joins\\Census_",1940,".shp"), stringsAsFactors = F)
c1940 <- lookup_rename(c1940, var.names %>% filter(year == 1940), drop.other = T)

c1950 <- st_read(paste0("Joins\\Census_",1950,".shp"), stringsAsFactors = F)
c1950 <- lookup_rename(c1950, var.names %>% filter(year == 1950), drop.other = T)

c1960 <- st_read(paste0("Joins\\Census_",1960,".shp"), stringsAsFactors = F)
c1960 <- lookup_rename(c1960, var.names %>% filter(year == 1960), drop.other = T)

c1970 <- st_read(paste0("Joins\\Census_",1970,".shp"), stringsAsFactors = F)
c1970 <- lookup_rename(c1970, var.names %>% filter(year == 1970), drop.other = T)

c1980 <- st_read(paste0("Joins\\Census_",1980,".shp"), stringsAsFactors = F)
c1980 <- lookup_rename(c1980, var.names %>% filter(year == 1980), drop.other = T)

c1990 <- st_read(paste0("Joins\\Census_",1990,".shp"), stringsAsFactors = F)
c1990 <- lookup_rename(c1990, var.names %>% filter(year == 1990), drop.other = T)

c2000 <- st_read(paste0("Joins\\Census_",2000,".shp"), stringsAsFactors = F)
c2000 <- lookup_rename(c2000, var.names %>% filter(year == 2000), drop.other = T)

c2010 <- st_read("Joins\\Census_2010.shp", stringsAsFactors = F)



c1820$stBlack <- c1820$`Slaves and free colored`
c1830$stBlack <- c1830$`Non-white`
c1840$stBlack <- c1840$`Nonwhite: Free` + c1840$`Nonwhite: Slave`
c1850$stBlack <- c1850$`Nonwhite: Free` + c1850$`Nonwhite: Slave`
c1860$stBlack <- c1860$`Free colored`
c1870$stBlack <- c1870$Colored
c1880$stBlack <- c1880$Colored
c1890$stBlack <- c1890$`Colored >> Male` + c1890$`Colored >> Female`
c1900$stBlack <- c1900$`Negro >> Male` + c1900$`Negro >> Female`
c1910$stBlack <- c1910$`Negro >> Male` + c1910$`Negro >> Female`
c1920$stBlack <- c1920$`Negro >> Male` + c1920$`Negro >> Female`
c1930$stBlack <- c1930$`Non-white`
c1940$stBlack <- c1940$Negro
c1950$stBlack <- c1950$`Non-white`
c1960$stBlack <- c1960$`Non-white`
c1970$stBlack <- c1970$Negro
c1980$stBlack <- c1980$Black
c1990$stBlack <- c1990$Black
c2000$stBlack <- c2000$`Black or African American alone`
c2010$stBlack <- c2010$black

field.names <- c("c1820" = "Slaves and free colored",
                 "c1830" = "Non-white",
                 "c1840" = "Nonwhite: Free + Nonwhite: Slave",
                 "c1850" = "Nonwhite: Free + Nonwhite: Slave",
                 "c1860" = "Free colored + Slave + Half breed",
                 "c1870" = "Colored",
                 "c1880" = "Colored",
                 "c1890" = "Colored >> Male  + Colored >> Female",
                 "c1900" = "Negro >> Male + Negro >> Female",
                 "c1910" = "Negro >> Male + Negro >> Female",
                 "c1920" = "Negro >> Male + Negro >> Female",
                 "c1930" = "Non-white",
                 "c1940" = "Negro",
                 "c1950" = "Non-white",
                 "c1960" = "Non-white",
                 "c1970" = "Negro",
                 "c1980" = "Black",
                 "c1990" = "Black",
                 "c2000" = "Black or African American alone",
                 "c2010" = "Black")

addPopup<- function(df){
  df$popup <- paste("<b>",df$NHGISNAM,"</b><br>","Black Population = ",df$stBlack,sep = "")
  df[is.na(df$stBlack),"stBlack"] <- 0
  return(df)
}

c1820 <- addPopup(st_transform(c1820, crs = 4326))
c1830 <- addPopup(st_transform(c1830, crs = 4326))
c1840 <- addPopup(st_transform(c1840, crs = 4326))
c1850 <- addPopup(st_transform(c1850, crs = 4326))
c1860 <- addPopup(st_transform(c1860, crs = 4326))
c1870 <- addPopup(st_transform(c1870, crs = 4326))
c1880 <- addPopup(st_transform(c1880, crs = 4326))
c1890 <- addPopup(st_transform(c1890, crs = 4326))
c1900 <- addPopup(st_transform(c1900, crs = 4326))
c1910 <- addPopup(st_transform(c1910, crs = 4326))
c1920 <- addPopup(st_transform(c1920, crs = 4326))
c1930 <- addPopup(st_transform(c1930, crs = 4326))
c1940 <- addPopup(st_transform(c1940, crs = 4326))
c1950 <- addPopup(st_transform(c1950, crs = 4326))
c1960 <- addPopup(st_transform(c1960, crs = 4326))
c1970 <- addPopup(st_transform(c1970, crs = 4326))
c1980 <- addPopup(st_transform(c1980, crs = 4326))
c1990 <- addPopup(st_transform(c1990, crs = 4326))
c2000 <- addPopup(st_transform(c2000, crs = 4326))
c2010 <- addPopup(st_transform(c2010, crs = 4326))




  
  

save(c1820,c1830,
     c1840,c1850, 
     c1860,c1870, 
     c1880, c1890, 
     c1900, c1910, 
     c1920, c1930, 
     c1940, c1950, 
     c1960, c1970, 
     c1980, c1990, 
     c2000, c2010,
     field.names, 
     file = "IN_CensusViewer\\data.RData")

# write_csv(st_set_geometry(c2010, NULL), "Outputs\\Census_2010.csv")
# write_csv(st_set_geometry(c2000, NULL), "Outputs\\Census_2000.csv")
# write_csv(st_set_geometry(c1990, NULL), "Outputs\\Census_1990.csv")
# write_csv(st_set_geometry(c1980, NULL), "Outputs\\Census_1980.csv")
# write_csv(st_set_geometry(c1970, NULL), "Outputs\\Census_1970.csv")
# write_csv(st_set_geometry(c1960, NULL), "Outputs\\Census_1960.csv")
# write_csv(st_set_geometry(c1950, NULL), "Outputs\\Census_1950.csv")
# write_csv(st_set_geometry(c1940, NULL), "Outputs\\Census_1940.csv")
# write_csv(st_set_geometry(c1930, NULL), "Outputs\\Census_1930.csv")
# write_csv(st_set_geometry(c1920, NULL), "Outputs\\Census_1920.csv")
# write_csv(st_set_geometry(c1910, NULL), "Outputs\\Census_1910.csv")
# write_csv(st_set_geometry(c1900, NULL), "Outputs\\Census_1900.csv")
# write_csv(st_set_geometry(c1890, NULL), "Outputs\\Census_1890.csv")
# write_csv(st_set_geometry(c1880, NULL), "Outputs\\Census_1880.csv")
# write_csv(st_set_geometry(c1870, NULL), "Outputs\\Census_1870.csv")
# write_csv(st_set_geometry(c1860, NULL), "Outputs\\Census_1860.csv")
# write_csv(st_set_geometry(c1850, NULL), "Outputs\\Census_1850.csv")
# write_csv(st_set_geometry(c1840, NULL), "Outputs\\Census_1840.csv")
# write_csv(st_set_geometry(c1830, NULL), "Outputs\\Census_1830.csv")
# write_csv(st_set_geometry(c1820, NULL), "Outputs\\Census_1820.csv")

