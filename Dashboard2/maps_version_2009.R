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

#load spatial data
world_spdf <- readOGR( 
  dsn = getwd() , 
  layer = "TM_WORLD_BORDERS_SIMPL-0.3",
  verbose = FALSE
)

democracies <- readRDS("data/data_dashboard.rds")

democracies$country_name[democracies$country_name == "United States of America"] <- "United States"

democracies_2009  <- subset(democracies, year == 2009)

#match polyarchy and spatial data via country name
world_spdf$v2x_polyarchy <- democracies_2009$v2x_polyarchy[match(world_spdf$NAME, democracies_2009$country_name)]


##### Maps with the Circles
#create label texts
world_spdf@data$LabelText <- paste0(
  "<b>Country:</b> ", world_spdf@data$NAME,"<br>", 
  "<b>v2x_polyarchy:</b> ", format(world_spdf@data$v2x_polyarchy, nsmall=0, big.mark=","))

#define colorpalette for chart legend
paletteBins <- c(0, 20, 40, 60, 80, 100)
colorPalette <- colorBin(palette = "YlOrBr", domain = democracies$v2x_polyarchy, na.color = "transparent", bins = paletteBins)



leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView(lat = 0, lng = 0, zoom=2) %>%
  addCircleMarkers(lng = ~LON,
                   lat = ~LAT,
                   radius = ~log(v2x_polyarchy)*4,
                   weight = 1,
                   opacity = 1,
                   color = ~ifelse(v2x_polyarchy > 0, "black", "transparent"),
                   fillColor = ~ifelse(v2x_polyarchy > 0, colorPalette(v2x_polyarchy), "transparent"),
                   fillOpacity = 0.8,
                   label = ~lapply(LabelText, htmltools::HTML)) %>%
  
  addLegend(pal = colorPalette, values = democracies$v2x_polyarchy, opacity=0.9, title = "polyarchy", position = "bottomleft")


##### Maps with the countries and color 
#create choropleth map
leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView(lat = 0, lng = 0, zoom=2) %>%
  
  addPolygons( 
    layerId = ~ISO2,
    fillColor = ~colorPalette(v2x_polyarchy),
    stroke = TRUE, 
    fillOpacity = 1, 
    color = "white", 
    weight = 1,
    label = ~lapply(LabelText, htmltools::HTML)) %>%
  
  addLegend(pal = colorPalette, values = democracies$v2x_polyarchy, opacity=0.9, title = "polyarchy", position = "bottomleft")





