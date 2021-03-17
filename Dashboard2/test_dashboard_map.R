library(maps) 
library(mapproj) 
library(tidyverse) 
library(sf) 
library(rworldmap) 
library(ggmap) 
library(rgeos) 
library(leaflet) 
library(rgdal) 
library(RColorBrewer) 

#https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1 


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



#load spatial data 
world_spdf <- readOGR(  
  dsn = getwd() ,  
  layer = "TM_WORLD_BORDERS_SIMPL-0.3", 
  verbose = FALSE 
) 

democracies <- readRDS("data/data_dashboard.rds") 
democracies$country_name[democracies$country_name == "United States of America"] <- "United States" 



# User interface ---- 
ui <- fluidPage( 
  
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
                 
                 selectInput("var",  
                             label = "Choose a variable to display", 
                             choices = c("Electoral Democracy", "Liberal Democracy", 
                                         "Participatory Democracy", "Deliberative Democracy"), 
                             selected = "Electoral Democracy"), 
                 
                 sliderInput("year",  
                             label = "Year of interest:", 
                             min = 1970, max = 2019, value = c(2000), sep = "", step = 1, animate=animationOptions(interval=500, loop=FALSE)), 
                 
                 p("Click on the play button to see the evolution over the years") 
                 
                 ), 
    
    mainPanel(width = 10, 
              
              leafletOutput("map", width = "90%", height = "750px") 
              
    ) 
  ) 
  ) 

# Server logic ---- 
server <- function(input, output) { 
  
  
  #filter data depending on selected date 
  filteredData <- reactive({ 
    req(input$year) 
    democracies[democracies$year == input$year, ] 
  }) 
  
  # yl contains the y variable or column name of the iris dataset selected by the user 
  yl <-  reactive({ switch(input$var_y,   
                           "v2x_polyarchy" = "Index Electoral Democracy",  
                           "v2x_libdem" = "Index Liberal Democracy",  
                           "v2x_partipdem" = "Index Participatory Democracy",  
                           "v2x_delibdem" = "Index Deliberative Democracy")  
  }) 
  
  #create the base leaflet map 
  output$map <- renderLeaflet({ 
    
    world_spdf@data$LabelText <- paste0( 
      "<b>Country:</b> ", world_spdf@data$NAME,"<br>",  
      "<b>v2x_delibdem:</b> ", format(world_spdf@data$v2x_delibdem, nsmall=0, big.mark=",")) 
    
    #define colorpalette for chart legend 
    paletteBins <- c(0, 20, 40, 60, 80, 100) 
    colorPalette <- colorBin(palette = "YlOrBr", domain = filteredData()$v2x_delibdem, na.color = "transparent", bins = paletteBins)     
    
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
      leaflet::addLegend(pal = colorPalette, values = democracies$v2x_delibdem, opacity = 0.9, title = "v2x_delibdem", position = "bottomleft") 
    
  }) 
  
  observe({ 
    
    world_spdf$v2x_delibdem <- filteredData()$v2x_delibdem[match(world_spdf$NAME, filteredData()$country_name)] 
    
    
    world_spdf@data$LabelText <- paste0( 
      "<b>Country:</b> ", world_spdf@data$NAME,"<br>",  
      "<b>v2x_delibdem:</b> ", format(world_spdf@data$v2x_delibdem, nsmall=0, big.mark=",")) 
    
    #define colorpalette for chart legend 
    paletteBins <- c(0, 20, 40, 60, 80, 100) 
    colorPalette <- colorBin(palette = "YlOrBr", domain = filteredData()$v2x_delibdem, na.color = "transparent", bins = paletteBins) 
    
    
    if(input$mapType == "Markers"){ 
      
      leafletProxy("map", data = world_spdf) %>% 
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
      
      leafletProxy("map", data = world_spdf) %>% 
        clearMarkers() %>% 
        setShapeStyle(layerId = ~ISO2, fillColor = ~ifelse(v2x_delibdem > 0, colorPalette(v2x_delibdem), "lightgray"), label = world_spdf$LabelText) 
      
    } 
  }) 
  
  
} 


# Run app ---- 
shinyApp(ui, server)