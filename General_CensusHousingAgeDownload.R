library(tidycensus)
library(sf)
library(tidyverse)
v18 <- load_variables(2018, dataset = "acs5")
year.vars <- c("Total_People" = "B00001_001",
               "Total_Structures" = "B25034_001",
               "Built2014orlater" = "B25034_002",
               "Built2010to2013" = "B25034_003" ,
               "Built2000to2009" = "B25034_004",
               "Built1990to1999" = "B25034_005" ,
               "Built1980to1989" = "B25034_006" ,
               "Built1970to1979" = "B25034_007",
               "Built1960to1969" ="B25034_008" ,
               "Built1950to1959" ="B25034_009" ,
               "Built1940to1949" = "B25034_010",
               "Built1939orearlier" = "B25034_011",
               "MedianYearStructureBuilt" = "B25035_001")
counties <- get_acs(geography = "county",variables = year.vars,year = 2018,state = "IN", output= "wide",geometry = T)
state <- get_acs(geography = "state",variables =year.vars,year = 2018,state = "IN",output = "wide", geometry = F)
tracts <- get_acs(geography = "tract",variables =year.vars,year = 2018,state = "IN",output = "wide", geometry = T)



counties%>%
  filter(GEOID == "18141")%>%
  mutate(pre1950  = Built1940to1949E + Built1939orearlierE,
         pre1980 = Built1970to1979E+Built1950to1959E + Built1960to1969E,
         post1980 = Built2014orlaterE+Built2010to2013E + Built2000to2009E+ Built1990to1999E + Built1980to1989E,
         all = Built2014orlaterE+Built2010to2013E + Built2000to2009E+ Built1990to1999E + Built1980to1989E +  Built1970to1979E+Built1950to1959E + Built1960to1969E + Built1940to1949E + Built1939orearlierE) %>%
  select(NAME,pre1950,pre1980, post1980, Total_StructuresE, all)



a <- st_read("E:\\GISWork_2\\Beidinger_Lead\\Structure_YearBuilt.shp")
b <- a[a$YearBuiltN >0& !is.na(a$YearBuiltN),]
sum(b$YearBuiltN < 1950)
sum(b$YearBuiltN >= 1950 & b$YearBuiltN <=1979)
sum(b$YearBuiltN >1979)
