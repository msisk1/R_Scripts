# rm(list=ls(all=TRUE)) # clear memory

library(rgdal)
library(leaflet)
library(shiny)
# setwd("/home/matthew/GIT/R_Scripts/ShinyApps/HUI_ChinaPathsApp_v2") #Linux machine
latlong <- "+init=epsg:4326" #This is the espg code for the WGS 1984 geographic projection
#loading Data
process.rawfiles <-F


if (process.rawfiles){
  china.outline<-getData("GADM",country="China",level=0)
  china.outline<- gSimplify(china.outline,.1, topologyPreserve=FALSE)
  
  master.file <- read.csv("data/Han Data - Data Prepared for Matt.csv", stringsAsFactors = F)
  master.file <- master.file[!is.na(master.file$N),]
  master.file <- master.file[!is.na(master.file$N.1),]
  
  load("data/Han_2019_04_30.rData")
  all.lines.df <- all
  all.lines.df <-  spTransform(all.lines.df, CRS(latlong))
  
  origin.points<-master.file
  coordinates(origin.points) <- ~E + N             #Define the coordinates to convert it to a spatial points data frame
  proj4string(origin.points) <- CRS(latlong)

  dest.points<-master.file
  coordinates(dest.points) <- ~E.1 + N.1             #Define the coordinates to convert it to a spatial points data frame
  proj4string(dest.points) <- CRS(latlong)
  
  list.battles <- unique(master.file$Battle.Name)
    
  save(dest.points,origin.points,all.lines.df, china.outline, list.battles, file="allData.Rda")
}else{
  load("allData.Rda")
}#end ifelse process.rawfiles



#Building Shiny Interface
ui <- fluidPage(
    # Give the page a title
    titlePanel("Han Dynasty Trial"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("time", "Battles:", 
                    choices=list.battles),
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
    index <- match(input$time,list.battles)
    return(origin.points[origin.points$Battle.Name == input$time,])
    
    
    }, ignoreNULL = FALSE)#end points.orig
  
  
  lines.layer <- eventReactive(c(input$time), {
    ooo <- all.lines.df[all.lines.df@data$Battle.Name ==input$time,]
    print(nrow(ooo))
    return(ooo)
    
    
  }, ignoreNULL = FALSE)#end points.orig
  
  filter.text <- eventReactive(c(input$time),{return(input$time)}, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      # fitBounds(-129,24.2,-65.58,50.54)%>%
      # addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(minZoom = 1, maxZoom = 13, noWrap = F)) %>%
      addProviderTiles(providers$Stamen.TerrainBackground, options = providerTileOptions(minZoom = 1, maxZoom = 13, noWrap = F)) %>%
      addPolygons(data = china.outline, stroke = TRUE, color = "#03F", weight = 5, opacity = 0.5, fill = F) 
    

  })
  observe({
    pal <- colorFactor(c("navy", "red","orange","cyan","yellow","blue","black"), domain = unique(points.layer()$Battle.Name))
    
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      clearGroup("lines")%>%
      clearControls() %>%
      # addCircleMarkers(data = points.layer(), popup = ~Location, color = ~pal(Battle.Name), radius = 5)%>%
      addPolylines(data = lines.layer(),  group = "lines",opacity = 1, color = "red")
      # addLegend("bottomright", pal = pal, values = lines.layer()$Name,
      #           title = filter.text(),
      #           opacity = 1
      # )
    # 
  })
}

shinyApp(ui, server,options = list(height = 1080))

# library(rsconnect)
# deployApp()
