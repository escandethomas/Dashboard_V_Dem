
# Used packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(tidyr)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)
library(ggfortify)
library(dplyr)
library(FNN)
library(rsconnect)
library(DT)
library(maps)
library(mapproj)
library(sf)
library(rworldmap)
library(ggmap)
library(rgeos)
library(rgdal) 
library(RColorBrewer) 

library(leaflet) 


#helper function for choropleth animation 
setShapeStyle <- function( map, data = getMapData(map), layerId, 
                           stroke = NULL, color = NULL, 
                           weight = NULL, opacity = NULL, 
                           fill = NULL, fillColor = NULL, 
                           fillOpacity = NULL, dashArray = NULL, 
                           smoothFactor = NULL, noClip = NULL, label = NULL, 
                           options = NULL){ 
  
  options <- c(list(layerId = layerId), 
               options, 
               filterNULL(list(stroke = stroke, color = color, 
                               weight = weight, opacity = opacity, 
                               fill = fill, fillColor = fillColor, 
                               fillOpacity = fillOpacity, dashArray = dashArray, 
                               smoothFactor = smoothFactor, noClip = noClip, label = label 
               ))) 
  
  options <- evalFormula(options, data = data) 
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE))) 
  
  layerId <- options[[1]] 
  style <- options[-1] 
  if("label" %in% colnames(style)){ 
    labelData = style[,"label", FALSE] 
    style = style[,-which(colnames(style)=="label"), FALSE] 
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label) 
  } 
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style); 
} 

#helper function in JS for choropleth animation 
leafletjs <-  tags$head( 
  tags$script(HTML(' 
                   
                   window.LeafletWidget.methods.setStyle = function(category, layerId, style){ 
                   var map = this; 
                   if (!layerId){ 
                   return; 
                   } else if (!(typeof(layerId) === "object" && layerId.length)){ 
                   layerId = [layerId]; 
                   } 
                   style = HTMLWidgets.dataframeToD3(style); 
                   layerId.forEach(function(d,i){ 
                   var layer = map.layerManager.getLayer(category, d); 
                   if (layer){ 
                   layer.setStyle(style[i]); 
                   } 
                   }); 
                   }; 
                   window.LeafletWidget.methods.setLabel = function(category, layerId, label){ 
                   var map = this; 
                   if (!layerId){ 
                   return; 
                   } else if (!(typeof(layerId) === "object" && layerId.length)){ 
                   layerId = [layerId]; 
                   } 
                   layerId.forEach(function(d,i){ 
                   var layer = map.layerManager.getLayer(category, d); 
                   if (layer){ 
                   layer.unbindTooltip(); 
                   layer.bindTooltip(label[i]) 
                   } 
                   }); 
                   }; 
                   ' 
  )) 
) 


##### Define UI #####
ui <- navbarPage("Horizontal Dashboard", theme = shinytheme("flatly"),
                    
                    #### Map Advanced Tab #### 
                    tabPanel("Detailed Map",                   
                             leafletjs, 
                             titlePanel("Liberal Democracies in the World"), 
                             
                             sidebarLayout( 
                               sidebarPanel(width = 2, 
                                            helpText("Create world map with  
                                                     information on different democratic indices."), 
                                            
                                            radioButtons(inputId = "mapType", 
                                                         label = "Select Map Type", 
                                                         choices = c("Markers", "Choropleth"), 
                                                         selected = "Markers", 
                                                         inline = TRUE), 
                                            
                                            selectInput("var_map_advanced",  
                                                        label = "Choose a variable to display", 
                                                        choices = c("Electoral Democracy", "Liberal Democracy", 
                                                                    "Participatory Democracy", "Deliberative Democracy"), 
                                                        selected = "Electoral Democracy"), 
                                            
                                            sliderInput("year_map_advanced",  
                                                        label = "Year of interest:", 
                                                        min = 1970, max = 2019, value = c(2000), sep = "", step = 1, animate=animationOptions(interval=500, loop=FALSE)), 
                                            
                                            p("Click on the play button to see the evolution over the years") 
                                            
                                            ), 
                               
                               mainPanel(width = 10, 
                                         
                                         leafletOutput("Map_advanced", width = "90%", height = "750px") 
                                         
                               ) 
                             ) 
)                    

)






# Define server logic ----
server <- function(input, output) {
  
  
  
  #Data to be used
  democracies <-readRDS("data/data_dashboard.rds")
  
  #Correcting the name of the United States
  democracies$country_name[democracies$country_name == "United States of America"] <- "United States"   
  
  worldmap <- getMap(resolution = "low")
  
  worldmap@data <-  worldmap@data %>% 
    dplyr::select(ADMIN,REGION,continent, POP_EST)
  
  worldmap <- st_as_sf(worldmap)
  
  worldmap <- worldmap %>%
    mutate(country_name = as.character(ADMIN))
  
  worldmap <- worldmap %>%  
    filter(REGION != "Antarctica")
  
  #load spatial data 
  world_spdf <- readOGR(  
    dsn = getwd() ,  
    layer = "TM_WORLD_BORDERS_SIMPL-0.3", 
    verbose = FALSE 
  ) 
#### Code for Map Advanced ####   

#filter data depending on selected date 
filteredData <- reactive({ 
  req(input$year_map_advanced) 
  democracies[democracies$year == input$year_map_advanced, ] 
}) 

# yl contains the y variable or column name of the iris dataset selected by the user 
yl <-  reactive({ switch(input$var_map_advanced,   
                         "v2x_polyarchy" = "Electoral Democracy Index",  
                         "v2x_libdem" = "Liberal Democracy Index",  
                         "v2x_partipdem" = "Participatory Democracy Index",  
                         "v2x_delibdem" = "Deliberative Democracy Index")  
  
}) 



# yc contains the different colors applied to the choice of variable
yc <-  reactive({ switch(input$var_map_advanced,   
                         "v2x_polyarchy" = "YlOrBr",  
                         "v2x_libdem" = "YlGnBu",  
                         "v2x_partipdem" = "YlGn",  
                         "v2x_delibdem" = "YlOrRd") 
  
}) 

#create the base leaflet map 
output$Map_advanced <- renderLeaflet({ 
  
  
  #define colorpalette for chart legend 
  paletteBins <- c(0, 20, 40, 60, 80, 100) 
  colorPalette <- colorBin(palette = yc(), domain = input$var_map_advanced, na.color = "transparent", bins = paletteBins)     
  
  leaflet(world_spdf) %>%  
    addTiles()  %>%  
    setView(lat = 0, lng = 0, zoom = 2) %>% 
    
    addPolygons(  
      layerId = ~ISO2, 
      fillColor = "lightgray",  
      stroke = TRUE,  
      fillOpacity = 1,  
      color = "white",  
      weight = 1 
    ) %>% 
    
    #need to specify the leaflet::addLegend function here to avoid ambiguity with the xts::addLegend function 
    leaflet::addLegend(pal = colorPalette, values = input$var_map_advanced, opacity = 0.9, title = yl(), position = "bottomleft") 
  
}) 

observe({
  
  if(input$var_map_advanced == "v2x_delibdem"){     
    world_spdf$v2x_delibdem <- filteredData()$v2x_delibdem[match(world_spdf$NAME, filteredData()$country_name)] 
    
    world_spdf@data$LabelText <- paste0( 
      "<b>Country:</b> ", world_spdf@data$NAME,"<br>",  
      "<b>", yl(), ":</b> ", format(world_spdf@data$v2x_delibdem, nsmall=0, big.mark=",")) 
    
    #define colorpalette for chart legend 
    paletteBins <- c(0, 20, 40, 60, 80, 100) 
    colorPalette <- colorBin(palette = yc(), domain = input$var_map_advanced, na.color = "transparent", bins = paletteBins) 
    
    
    if(input$mapType == "Markers"){ 
      
      leafletProxy("Map_advanced", data = world_spdf) %>% 
        clearMarkers() %>% 
        setShapeStyle(layerId = ~ISO2, fillColor = "lightgray") %>% 
        addCircleMarkers(lng = ~LON, 
                         lat = ~LAT, 
                         radius = ~log(v2x_delibdem) * 4, 
                         weight = 1, 
                         opacity = 1, 
                         color = ~ifelse(v2x_delibdem > 0, "black", "transparent"), 
                         fillColor = ~ifelse(v2x_delibdem > 0, colorPalette(v2x_delibdem), "transparent"), 
                         fillOpacity = 0.8, 
                         label = ~lapply(LabelText, htmltools::HTML)) 
      
    }else if(input$mapType == "Choropleth"){ 
      
      leafletProxy("Map_advanced", data = world_spdf) %>% 
        clearMarkers() %>% 
        setShapeStyle(layerId = ~ISO2, fillColor = ~ifelse(v2x_delibdem > 0, colorPalette(v2x_delibdem), "lightgray"), label = world_spdf$LabelText) 
      
    } 
  }
  
  
  if(input$var_map_advanced == "v2x_polyarchy"){     
    world_spdf$v2x_polyarchy <- filteredData()$v2x_polyarchy[match(world_spdf$NAME, filteredData()$country_name)] 
    
    world_spdf@data$LabelText <- paste0( 
      "<b>Country:</b> ", world_spdf@data$NAME,"<br>",  
      "<b>", yl(), ":</b> ", format(world_spdf@data$v2x_polyarchy, nsmall=0, big.mark=",")) 
    
    #define colorpalette for chart legend 
    paletteBins <- c(0, 20, 40, 60, 80, 100) 
    colorPalette <- colorBin(palette = yc(), domain = input$var_map_advanced, na.color = "transparent", bins = paletteBins) 
    
    
    if(input$mapType == "Markers"){ 
      
      leafletProxy("Map_advanced", data = world_spdf) %>% 
        clearMarkers() %>% 
        setShapeStyle(layerId = ~ISO2, fillColor = "lightgray") %>% 
        addCircleMarkers(lng = ~LON, 
                         lat = ~LAT, 
                         radius = ~log(v2x_polyarchy) * 4, 
                         weight = 1, 
                         opacity = 1, 
                         color = ~ifelse(v2x_polyarchy > 0, "black", "transparent"), 
                         fillColor = ~ifelse(v2x_polyarchy > 0, colorPalette(v2x_polyarchy), "transparent"), 
                         fillOpacity = 0.8, 
                         label = ~lapply(LabelText, htmltools::HTML)) 
      
    }else if(input$mapType == "Choropleth"){ 
      
      leafletProxy("Map_advanced", data = world_spdf) %>% 
        clearMarkers() %>% 
        setShapeStyle(layerId = ~ISO2, fillColor = ~ifelse(v2x_polyarchy > 0, colorPalette(v2x_polyarchy), "lightgray"), label = world_spdf$LabelText) 
      
    } 
  }
  if(input$var_map_advanced == "v2x_libdem"){     
    world_spdf$v2x_libdem <- filteredData()$v2x_libdem[match(world_spdf$NAME, filteredData()$country_name)] 
    
    world_spdf@data$LabelText <- paste0( 
      "<b>Country:</b> ", world_spdf@data$NAME,"<br>",  
      "<b>", yl(), ":</b> ", format(world_spdf@data$v2x_libdem, nsmall=0, big.mark=",")) 
    
    #define colorpalette for chart legend 
    paletteBins <- c(0, 20, 40, 60, 80, 100) 
    colorPalette <- colorBin(palette = yc(), domain = input$var_map_advanced, na.color = "transparent", bins = paletteBins) 
    
    
    if(input$mapType == "Markers"){ 
      
      leafletProxy("Map_advanced", data = world_spdf) %>% 
        clearMarkers() %>% 
        setShapeStyle(layerId = ~ISO2, fillColor = "lightgray") %>% 
        addCircleMarkers(lng = ~LON, 
                         lat = ~LAT, 
                         radius = ~log(v2x_libdem) * 4, 
                         weight = 1, 
                         opacity = 1, 
                         color = ~ifelse(v2x_libdem > 0, "black", "transparent"), 
                         fillColor = ~ifelse(v2x_libdem > 0, colorPalette(v2x_libdem), "transparent"), 
                         fillOpacity = 0.8, 
                         label = ~lapply(LabelText, htmltools::HTML)) 
      
    }else if(input$mapType == "Choropleth"){ 
      
      leafletProxy("Map_advanced", data = world_spdf) %>% 
        clearMarkers() %>% 
        setShapeStyle(layerId = ~ISO2, fillColor = ~ifelse(v2x_libdem > 0, colorPalette(v2x_libdem), "lightgray"), label = world_spdf$LabelText) 
      
    } 
  }
  
  if(input$var_map_advanced == "v2x_partipdem"){     
    world_spdf$v2x_partipdem <- filteredData()$v2x_partipdem[match(world_spdf$NAME, filteredData()$country_name)] 
    
    world_spdf@data$LabelText <- paste0( 
      "<b>Country:</b> ", world_spdf@data$NAME,"<br>",  
      "<b>", yl(), ":</b> ", format(world_spdf@data$v2x_partipdem, nsmall=0, big.mark=",")) 
    
    #define colorpalette for chart legend 
    paletteBins <- c(0, 20, 40, 60, 80, 100) 
    colorPalette <- colorBin(palette = yc(), domain = input$var_map_advanced, na.color = "transparent", bins = paletteBins) 
    
    
    if(input$mapType == "Markers"){ 
      
      leafletProxy("Map_advanced", data = world_spdf) %>% 
        clearMarkers() %>% 
        setShapeStyle(layerId = ~ISO2, fillColor = "lightgray") %>% 
        addCircleMarkers(lng = ~LON, 
                         lat = ~LAT, 
                         radius = ~log(v2x_partipdem) * 4, 
                         weight = 1, 
                         opacity = 1, 
                         color = ~ifelse(v2x_partipdem > 0, "black", "transparent"), 
                         fillColor = ~ifelse(v2x_partipdem > 0, colorPalette(v2x_partipdem), "transparent"), 
                         fillOpacity = 0.8, 
                         label = ~lapply(LabelText, htmltools::HTML)) 
      
    }else if(input$mapType == "Choropleth"){ 
      
      leafletProxy("Map_advanced", data = world_spdf) %>% 
        clearMarkers() %>% 
        setShapeStyle(layerId = ~ISO2, fillColor = ~ifelse(v2x_partipdem > 0, colorPalette(v2x_partipdem), "lightgray"), label = world_spdf$LabelText) 
      
    } 
  }
  
}) 


}

# Run app ---- 
shinyApp(ui, server)