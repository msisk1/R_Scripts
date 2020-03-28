# rm(list=ls(all=TRUE)) # clear memory

library(rgdal)
library(leaflet)
library(shiny)
# setwd("E:\\GIT_Checkouts\\R_Scripts\\ShinyApps\\HUI_ChinaPathsApp_v3_Many") #Linux machine
latlong <- "+init=epsg:4326" #This is the espg code for the WGS 1984 geographic projection
#loading Data
process.rawfiles <-F


if (process.rawfiles){
  setwd("E:\\GIT_Checkouts\\R_Scripts\\ShinyApps\\HUI_ChinaPathsApp_v3_Many")
  library(raster)
  library(rgeos)
  china.outline<-getData("GADM",country="China",level=0)
  china.outline<- gSimplify(china.outline,.1, topologyPreserve=FALSE)
  
  # master.file <- read.csv("data/Han Data - Data Prepared for Matt.csv", stringsAsFactors = F)
  # master.file <- master.file[!is.na(master.file$N),]
  # master.file <- master.file[!is.na(master.file$N.1),]
  
  # load("data/MING_points.rData")
  
  load("data/MING ROUTES_2019-11-19.rData")
  names(all)[names(all) == "Battle.name"] <- "Campaign.name"
  ming <- all
  ming$dynasty <- "Ming"
  ming$X <- NULL
  ming$Campagin.Begin.Year <- 0
  ming$Campagin.End.Year   <- 0
ming <- ming[,c('Beginning.Year','End.Year','Campagin.Begin.Year',"Campagin.End.Year" ,"Initiator","Target"  , "Campaign.name",
                "Battle.Number"  ,                           
                "Starting.point.current.name..add.old.name.",
                "E",
                "N" ,                                        
                
                "Ending.point.current.name..old.name."  ,    
                "E.1"  ,  "N.1",
                                                   
                "Main.campaign..yes.1..no.0.",
                "Multiple.routes"    ,                       
                "Enemies..yes.1..no.0..",
                "Tri..topographic.ruggedness.index."    ,    
                "Distance",
                "Interior..yes..1..no.0."  ,                 
                "Notes.comments",
                "Army.size.initiator" ,                      
                "Army.size.target",
                "Army.Casualties.Initiator"  ,               
                "Army.Casualties.Target",
                "Result..win.1..lose.0."  ,                  
                "dynasty" 
)]
  
  load("data/Sui Routes_Fin_2019-11-19.rData")
  sui <- all
  sui$dynasty <- "Sui"
  
  load("data/Tang routes_Final_2019-11-19.rData")
  names(all)[names(all) == "Battle.name"] <- "Campaign.name"
  tang <- all
  tang$dynasty <- "Tang"
  
  load("data/Song-Yuan War Data_2019-11-19.rData")
  song <- all
  song$dynasty <- "Song-Yuan"
  song$X <- NULL
  song$X.1 <- NULL
  song <- song[,c("War.Beginning.Year", "War.End.Year", "Campaign.Beginning.Year", "Campaign.End.Year", "Initiator", "Target","Campaign.name","Identification.Number","Starting.point.current.name..add.old.name.","E","N","Ending.point.current.name..old.name.", "E.1", "N.1","Main.campaign..yes.1..no.0.","Multiple.routes","Enemies", "War.Name","Distance","Interior..yes..1..no.0.","Notes.comments", "Initiator.s.Army.Size","Target.s.Army.Size","Initiator.s.Battle.Deaths","Target.s.Battle.Deaths","Result..win.1..lose.0.","dynasty")]
  
  
  names(ming) <- names(sui)
  names(tang) <-names(sui)
  names(song) <- names(sui)
  
  all <- rbind(ming, sui)
  all <- rbind(all, tang)
  all <- rbind(all, song)
  
    
  all.lines.df <- all
  all.lines.df <-  spTransform(all.lines.df, CRS(latlong))
  
  origin.points<-all@data
  coordinates(origin.points) <- ~E + N             #Define the coordinates to convert it to a spatial points data frame
  proj4string(origin.points) <- CRS(latlong)
  
  dest.points<-all@data
  coordinates(dest.points) <- ~E.1 + N.1             #Define the coordinates to convert it to a spatial points data frame
  proj4string(dest.points) <- CRS(latlong)
  
  list.battles <- unique(all$Campaign.name)
  list.dynasties <- unique(all$dynasty)
  save(dest.points,origin.points,all.lines.df, china.outline, list.battles, list.dynasties, file="allData.Rda")
}else{
  load("allData.Rda")
}#end ifelse process.rawfiles


origin.points$popupw <- paste(sep ="", "<b>",origin.points$Campaign.name,"</b><br/>",
                              "Starting: ", origin.points$Sui.Starting.point.current.name..add.old.name.,"<br/>",
                              "Target:   ", origin.points$Sui.Ending.point.current.name..old.name.)


#Building Shiny Interface
ui <- fluidPage(
  # Give the page a title
  titlePanel("Multiple Dynasty Trial"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("dynastyChosen", "Dynasty:",
                  choices = list.dynasties),
      selectInput("campainChosen", "Campaigns:", 
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
  points.layer <- eventReactive(c(input$campainChosen, input$dynastyChosen), {
    temp.points <- origin.points[origin.points$Campaign.name == input$campainChosen,]
    temp.points <- temp.points[temp.points$dynasty == input$dynastyChosen,]
    return(temp.points)
    
    
  }, ignoreNULL = FALSE)#end points.orig
  
  
  lines.layer <- eventReactive(c(input$campainChosen, input$dynastyChosen), {
    ooo <- all.lines.df[all.lines.df@data$Campaign.name ==input$campainChosen,]
    ooo <- ooo[ooo$dynasty == input$dynastyChosen]
    # print(nrow(ooo))
    return(ooo)
    
    
  }, ignoreNULL = FALSE)#end points.orig
  
  list.choices <- eventReactive(c(input$dynastyChosen),{
    list.campaign <- unique(all.lines.df@data[all.lines.df@data$dynasty == input$dynastyChosen,"Campaign.name"])
    return(list.campaign)
  }, ignoreNULL = FALSE)  
  
  
  filter.text <- eventReactive(c(input$campainChosen),{return(input$campainChosen)}, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      # fitBounds(-129,24.2,-65.58,50.54)%>%
      # addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(minZoom = 1, maxZoom = 13, noWrap = F)) %>%
      addProviderTiles(providers$Stamen.TerrainBackground, options = providerTileOptions(minZoom = 1, maxZoom = 13, noWrap = F)) %>%
      addPolygons(data = china.outline, stroke = TRUE, color = "#03F", weight = 5, opacity = 0.5, fill = F) 
    
    
  })
  observeEvent(
    input$dynastyChosen,
    updateSelectInput(session, "campainChosen",
                      choices = list.choices())
  )
  observe({
    pal <- colorFactor(c("navy", "red","orange","cyan","yellow","blue","black"), domain = unique(points.layer()$Campaign.name))
    
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      clearGroup("lines")%>%
      clearControls() %>%
      addCircleMarkers(data = points.layer(), popup = ~popupw, color = ~pal(Campaign.name), radius = 5)%>%
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
