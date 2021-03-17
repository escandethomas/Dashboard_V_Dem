library(leaflet)
library(rgdal)

#loading the world borders spatial polygons data frame
world_spdf <- readOGR( 
  dsn= "C:/Users/wb464819/Desktop/Github/Dashboard_V_Dem/Dashboard2" , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

democracies <- readRDS("C:/Users/wb464819/Desktop/Github/Dashboard_V_Dem/Dashboard2/data/data_dashboard.rds")
democracies_final <-  subset(democracies, year == 2010) 

worldmap_data  <- world_spdf %>%  
    left_join( democracies_final )

world_spdf$dem <- democracies_final$v2x_libdem[match(world_spdf$NAME, democracies_final$country_name)]
