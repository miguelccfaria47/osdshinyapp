library(shiny)
library(leaflet)
library(magrittr)
library(ggplot2)
library(reshape2)
library(data.table)
library(dplyr)
library(leaflet.minicharts)
library(shinyjs)
library(RColorBrewer)
library(colorRamps)

#setwd("C:/Users/Miguel/Desktop/shiny/map/new")


envData <- read.csv("NoDepth.table.csv", header=TRUE)
species <- read.csv("Species.csv",header=TRUE)


colnames(envData)[1] <- "SampleName"


envData <- envData[,-2]

maptypes <- c("MapQuestOpen.Aerial",
              "Stamen.TerrainBackground",
              "Esri.WorldImagery", 
              "OpenStreetMap",
              "Stamen.Watercolor",
              "HERE.hybridDay")



speciesNames <- colnames(species)[-1]


allData <- merge(species,envData,by="SampleName")

sampleNames <- allData[,1]
pop <- as.character(envData[,1])


ui <- navbarPage("Ocean Sampling Day",id="nav",

  tabPanel("Interactive map",
           div(class="outer",
            tags$head(
              includeCSS("styles.css")
            ),
                 
  leafletOutput("map", width="100%", height="100%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 415, height = "auto",
                
                h2("Options"),
                
                selectizeInput("species", "Select up to five taxa", choices = speciesNames, multiple = TRUE, options = list(maxItems = 5)),
                tags$br(),
                tags$br(),
                actionButton(inputId = "Clicks", label = "Simultaneous taxa prevalence sampling sites", style='padding:7px; width:380px; display:block; border:1.5px solid blue'),
                tags$br(),
                actionButton(inputId = "plot", label = "Pie charts of taxa prevalence sampling sites", style='padding:7px; width:380px; display:block; border:1.5px solid blue'),
                tags$br(),
                actionButton(inputId = "plotExists", label= "Pie charts of simultaneous taxa prevalence sampling sites", style='padding:7px; width:380px; display:block; border:1.5px solid blue'),
                tags$br(),
                actionButton(inputId = "reset", label = "Reset to all sampling sites", style='padding:7px; width:380px; display:block; border:1.5px solid blue'),
                tags$br(),
                tags$br(),
                selectizeInput("samples", "Select up to five sampling sites", choices = sampleNames),
                actionButton(inputId = "osd", "Pie charts of all taxa prevalent", style='padding:7px; width:380px; display:block; border:1.5px solid blue')
                
                
                
      )
    )    
  )
)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet(data=allData) %>% 
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude, popup = pop) %>% 
      #setView(lng = 33.31, lat = 44.36, zoom = 2.2) %>% 
      setMaxBounds(lng1 = 180.00, lat1 = 90.00, lng2 = -180.00, lat2 = -90.00)
  })
  

  
  observeEvent(input$Clicks, {
    
    if(is.null(input$species)){
      leafletProxy("map", data=allData) %>% 
        clearMarkers() %>%
        clearMinicharts() %>% 
        addMarkers(lng = ~Longitude, lat = ~Latitude, popup = pop)
     
    } else {
    
    newData <- allData %>% filter_at(vars(input$species), all_vars(.>0)) %>% dplyr::select(input$species,Longitude,Latitude)

    
    leafletProxy("map", data=newData) %>% 
      clearMarkers() %>%
      clearMinicharts() %>% 
      addMarkers(lng = ~Longitude, lat = ~Latitude, popup = pop) 
      
    }
  }
  )
  
  observeEvent(input$reset, {
    leafletProxy("map", data=allData) %>% 
      clearMarkers() %>%
      clearMinicharts() %>% 
      addMarkers(lng = ~Longitude, lat = ~Latitude, popup = pop)
    
  })
  
  observeEvent(input$plot, {
    
    if(is.null(input$species)){
      leafletProxy("map", data=allData) %>% 
        clearMarkers() %>%
        clearMinicharts() %>% 
        addMarkers(lng = ~Longitude, lat = ~Latitude, popup = pop)
      
    } else {
    
    newData <- allData[,input$species] 
    if(length(input$species) == 1){
    leafletProxy("map", data=newData) %>% 
      clearMarkers() %>%
      clearMinicharts() %>% 
      addMinicharts(
        allData$Longitude,allData$Latitude,
        type="pie",
        chartdata = newData,
        width = 15 * (1+ 2*newData/max(newData)),
        legendPosition = "topleft"
      )
    } else {
      leafletProxy("map", data=newData) %>% 
        clearMarkers() %>%
        clearMinicharts() %>% 
        addMinicharts(
          allData$Longitude,allData$Latitude,
          type="pie",
          chartdata = newData,
          legendPosition = "topleft"
      
      )}
    }  
  })
  
  observeEvent(input$plotExists, {
    
    if(is.null(input$species)){
      leafletProxy("map", data=allData) %>% 
        clearMarkers() %>%
        clearMinicharts() %>% 
        addMarkers(lng = ~Longitude, lat = ~Latitude, popup = pop)
      
    } else {
    
    
    plotData <- allData %>% filter_at(vars(input$species), all_vars(.>0)) %>% dplyr::select(input$species)
    coorData <- allData %>% filter_at(vars(input$species), all_vars(.>0)) %>% dplyr::select(Latitude, Longitude)
    numberRows <- nrow(plotData)

        if(numberRows > 0){
    leafletProxy("map", data=plotData) %>% 
      clearMarkers() %>%
      clearMinicharts() %>%
      addMinicharts(
        coorData$Longitude,coorData$Latitude,
        type="pie",
        chartdata = plotData,
        legendPosition = "topleft"
      )} else {
        print(numberRows)
        leafletProxy("map", data=plotData) %>% 
          clearMarkers() %>%
          clearMinicharts()
          
      }  
    }
  }
  
  )
 
  observeEvent(input$osd, {
    

    if(is.null(input$samples)){
      print(input$sample)
      leafletProxy("map", data=allData) %>% 
        clearMarkers() %>%
        clearMinicharts() %>% 
        addMarkers(lng = ~Longitude, lat = ~Latitude, popup = pop)
      
    } else {
      
      result <- species[-1]
      row.names(result) <- species$SampleName

      cr <- allData[-1]
      row.names(cr) <- allData$SampleName
      
      w <- result[input$samples,]
      nsm <- c()
      for(i in 1:ncol(w)){
        if(w[1,i] > 0){
          print(i)
          nsm <- append(nsm,i)
        }
      }
      print(nsm)
      
      final <- w[,nsm]
      lat <- cr[input$samples,"Latitude"]
      lng <- cr[input$samples,"Longitude"]


      
        leafletProxy("map", data=final) %>% 
          clearMarkers() %>%
          clearMinicharts() %>%
          addMinicharts(
            lat = lat, lng = lng,
            type="pie",
            chartdata = final,
            width = 50, height = 50,
            colorPalette = primary.colors(),
            legendPosition = "topleft"
          )
            
            
    }
  }
  
  )
  
  }
  

shinyApp(ui = ui, server = server)

