
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



# Define server logic ----
shinyServer( function(input, output) {
  
  ############### #helper function for choropleth animation ############## 
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
  
  
  #Data to be used
  democracies <-readRDS("data/data_dashboard.rds")
  
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
  
  #### Code for Table and Download of the dataset ####    
  
  output$table <- DT::renderDataTable(DT::datatable({
    data  <- subset(democracies, year >= input$year_table[1] & year <= input$year_table[2] ) 
    data  <- subset(data       , country_name == input$country_table) 
    data <- data %>% 
      select(year,country_name,v2x_polyarchy,v2x_libdem,v2x_partipdem,v2x_delibdem) 
    data
  }, colnames=c("Country Name", "Year", "Electoral Democracy", "Liberal Democracy", "Participatory Democracy","Deliberative Democracy")
  ))
  
  output$down_data <- downloadHandler(
    filename = function() {
      paste("V_dem", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(democracies, file, row.names = FALSE)
    }
  )
  
  
  #### Code for the graph output and download ####    
  # Data contains the dataset we want to show. It is a reactive function
  
  data  <- reactive({  
    subset(democracies, year >= input$year[1] & year <= input$year[2] & country_name == input$country) 
  })
  
  # yl contains the y variable or column name of the iris dataset selected by the user
  yl <-  reactive({ switch(input$var_y,  
                           "v2x_polyarchy" = "Index Electoral Democracy", 
                           "v2x_libdem" = "Index Liberal Democracy", 
                           "v2x_partipdem" = "Index Participatory Democracy", 
                           "v2x_delibdem" = "Index Deliberative Democracy") 
  })
  
  
  #Creating plot function to be used twice
  myPlot <- function(){
    ggplot(data(), aes_string(x = "year", y = input$var_y, colour = "country_name" )) + 
      geom_line() +
      labs(x = "Year" , y = yl()) +
      theme_classic() +
      scale_color_discrete(name = "") +
      theme(
        legend.position = "bottom")
  }    
  
  output$index_evolution <- renderPlotly({ 
    print(
      ggplotly(myPlot()) %>%   
        layout(legend=list(orientation="h",x=0.4,y=-0.2))
    )
  })
  
  
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$down <- downloadHandler(
    filename =  function() {
      paste("Graph_Democracies_Index", input$var3, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$var3 == "png") {
        png(file) # open the png device
      }
      else {
        pdf(file) # open the pdf device
      }
      #Generating the plot again to be downloaded
      print(myPlot())
      dev.off()  # turn the device off
      
    } 
  ) 
  
  #### Code for Map ####   
  
  democracies_year  <- reactive({  
    subset(democracies, year == input$year_map)
  })
  
  worldmap_data  <- reactive({  
    worldmap %>%  
      left_join( democracies_year() )
  })
  
  
  output$map <- renderPlot({
    data <- switch(input$var_map, 
                   "Electoral Democracy"     = worldmap_data()$v2x_polyarchy,
                   "Liberal Democracy"       = worldmap_data()$v2x_libdem,
                   "Participatory Democracy" = worldmap_data()$v2x_partipdem,
                   "Deliberative Democracy"  = worldmap_data()$v2x_delibdem)
    
    color1 <- switch(input$var_map, 
                     "Electoral Democracy" = "darkgreen",
                     "Liberal Democracy" = "black",
                     "Participatory Democracy" = "darkorange",
                     "Deliberative Democracy" = "darkviolet")
    
    color2 <- switch(input$var_map, 
                     "Electoral Democracy" = "lightgreen",
                     "Liberal Democracy" = "white",
                     "Participatory Democracy" = "yellow",
                     "Deliberative Democracy" = "violet")
    
    legend <- switch(input$var_map, 
                     "Electoral Democracy" = "Index Electoral Democracy",
                     "Liberal Democracy"   = "Index Liberal Democracy",
                     "Participatory Democracy" = "Index Participatory Democracy",
                     "Deliberative Democracy" = "Index Deliberative Democracy")
    
    
    
    ggplot(worldmap_data()) + 
      geom_sf(aes(fill=data)) +
      labs(fill = legend) +
      scale_fill_gradient(low = color2, high = color1) +
      theme_void() + 
      theme(legend.position = "top")    
    
  })
  
  #### Code for Map Advanced ####   
  democracies$country_name[democracies$country_name == "United States of America"] <- "United States" 

  #filter data depending on selected date 
  filteredData <- reactive({ 
    req(input$year_map_advanced) 
    democracies[democracies$year == input$year_map_advanced, ] 
  }) 
  
  # yl contains the y variable or column name of the iris dataset selected by the user 
  yl <-  reactive({ switch(input$var_map_advanced,   
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
      
})