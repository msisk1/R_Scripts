# rm(list=ls(all=TRUE)) # clear memory

library(rgdal)
library(leaflet)
library(shiny)
latlong <- "+init=epsg:4326" #This is the espg code for the WGS 1984 geographic projection
#loading Data
process.rawfiles <-F


if (process.rawfiles){
  setwd("/home/matthew/GIT/R_Scripts/ShinyApps/HUI_ChinaPathsApp_Original_Rebuild") #Linux machine
  
  library(raster)
  china.outline<-getData("GADM",country="China",level=0)
  china.outline<- gSimplify(china.outline,.1, topologyPreserve=FALSE)
  locations  <- read.csv("data/All_Locations_2016-10-11.csv")
  all.points.sdf <- locations
  coordinates(all.points.sdf) <- ~E + N
  proj4string(all.points.sdf) <- CRS(latlong)  
  
  all.lines.df <- readOGR(dsn = "data", layer = "All_ROutes_Merged")
  all.lines.df<- spTransform(all.lines.df, CRS((latlong)))
  

  list.dynasties <- unique(locations$Desc)
 
  save(list.dynasties,all.points.sdf,all.lines.df, china.outline, file="allData.Rda")
}else{
  load("allData.Rda")
}#end ifelse process.rawfiles


pal <- colorFactor(c("navy", "red","orange","brown","yellow","blue","black","coral"), domain = unique(all.points.sdf$Name))

#Building Shiny Interface
ui <- fluidPage(
    # Give the page a title
    titlePanel("China GIS Original Rebuild"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("time", "Route:", 
                    choices=c("All",as.character(list.dynasties))),
        hr(),
        helpText("Select a time period to view")
      ),
      
      # Create a spot for the barplot
      mainPanel(
        leafletOutput("mymap", height = 750)
        # textOutput("result")
        
                )
    )# end main Panel
  ) #end fluidpage


server <- function(input, output, session) {
  points.layer <- eventReactive(c(input$time), {
    if (input$time == "All"){
      temp.pt <- all.points.sdf
    }else{
      temp.pt <- all.points.sdf[all.points.sdf@data$Desc == input$time,]
      print(paste(input$time,nrow(temp.pt)))
    }
    return(temp.pt)
    
    
    }, ignoreNULL = FALSE)#end points.orig
  
  
  lines.layer <- eventReactive(c(input$time), {
    if (input$time == "All"){
      temp.ln <- all.lines.df
    }else{
      temp.ln <- all.lines.df[all.lines.df@data$Name == input$time,]
      
    }
    return(temp.ln)
    
    
  }, ignoreNULL = FALSE)#end points.orig
  
  filter.text <- eventReactive(c(input$time),{return(input$time)}, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      # fitBounds(-129,24.2,-65.58,50.54)%>%
      # addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(minZoom = 1, maxZoom = 13, noWrap = F)) %>%
      addProviderTiles(providers$Stamen.TerrainBackground, options = providerTileOptions(minZoom = 1, maxZoom = 13, noWrap = F)) %>%
      addPolygons(data = china.outline, stroke = TRUE, color = "#03F", weight = 5, opacity = 0.5, fill = F)%>%
      addCircleMarkers(data = points.layer(), popup = ~Location,  color = ~pal(Desc), radius = 5)%>%
      addPolylines(data = lines.layer(), popup = ~Name, group = "lines", color = ~pal(Name),opacity = 1)%>%
      addLegend("bottomright", pal = pal, values = all.lines.df$Name,
                opacity = 1)

  })

}

shinyApp(ui, server,options = list(height = 1080))

