# rm(list=ls(all=TRUE)) # clear memory

library(rgdal)
library(leaflet)
library(shiny)
# setwd("/home/matthew/GIT/R_Scripts/ShinyApps/HUI_ChinaPathsApp") #Linux machine
latlong <- "+init=epsg:4326" #This is the espg code for the WGS 1984 geographic projection
#loading Data
process.rawfiles <-F


if (process.rawfiles){
  library(raster)
  china.outline<-getData("GADM",country="China",level=0)
  china.outline<- gSimplify(china.outline,.1, topologyPreserve=FALSE)
  list.files <- list.files(path = "data", pattern = ".csv$", full.names=TRUE)
  list.dynasties <- lapply(list.files,FUN = tools::file_path_sans_ext)
  list.dynasties <- lapply(list.dynasties, FUN = basename)
  all.points.sdf <-lapply(list.files, function(x){
    y<- read.csv(x)
    coordinates(y) <- ~E + N
    proj4string(y) <- CRS(latlong)  
    return(y)
  })#end lapply all.points.sdf
  
  all.lines.df <- lapply(list.dynasties, function(x){
    y <- readOGR(dsn="data",layer=paste(x, "_AllPaths",sep=""))
    y<- spTransform(y, CRS((latlong)))
    return(y)
  })#end lapply all.lines.df
  save(list.dynasties,all.points.sdf,all.lines.df, china.outline, file="allData.Rda")
}else{
  load("allData.Rda")
}#end ifelse process.rawfiles



#Building Shiny Interface
ui <- fluidPage(
    # Give the page a title
    titlePanel("China GIS Trial"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("time", "Dynasty:", 
                    choices=list.dynasties),
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
    index <- match(input$time,list.dynasties)
    return(all.points.sdf[[index]])
    
    
    }, ignoreNULL = FALSE)#end points.orig
  
  
  lines.layer <- eventReactive(c(input$time), {
    print(paste("lines: ", input$time,list.dynasties))
    index <- match(input$time,list.dynasties)
    print(paste("lines: ", input$time,list.dynasties, index))
    
        return(all.lines.df[[index]])
    
    
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
    pal <- colorFactor(c("navy", "red","orange","cyan","yellow","blue","black"), domain = unique(points.layer()$Name))
    
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      clearGroup("lines")%>%
      clearControls() %>%
      addCircleMarkers(data = points.layer(), popup = ~Location, color = ~pal(Desc), radius = 5)%>%
      addPolylines(data = lines.layer(), popup = ~Name, group = "lines", color = ~pal(Name),opacity = 1)%>%
      addLegend("bottomright", pal = pal, values = lines.layer()$Name,
                title = filter.text(),
                opacity = 1
      )
    # 
  })
}

shinyApp(ui, server,options = list(height = 1080))

# library(rsconnect)
# deployApp()
