library(maps)
library(mapproj)
library(tidyverse)
library(sf)
library(rworldmap)
library(ggmap)
library(rgeos)
democracies <- readRDS("data/data_dashboard.rds")
democracies_2015 <- subset(democracies,year==2015) 

worldmap <- getMap(resolution = "low")

worldmap@data <-  worldmap@data %>% 
  dplyr::select(ADMIN,REGION,continent, POP_EST)

worldmap <- st_as_sf(worldmap)

worldmap <- worldmap %>%
  mutate(country_name = as.character(ADMIN))

worldmap <- worldmap %>% 
  left_join(democracies_2015)

worldmap <- worldmap %>%  
  filter(REGION != "Antarctica")
  
# User interface ----
ui <- fluidPage(
  titlePanel("Liberal Democracies in the World"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create world map with 
        information on the Liberal Democracy Index."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Electoral Democracy", "Liberal Democracy",
                              "Participatory Democracy", "Deliberative Democracy"),
                  selected = "Electoral Democracy"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  
  
  output$map <- renderPlot({
    data <- switch(input$var, 
                   "Electoral Democracy"     = worldmap$v2x_polyarchy,
                   "Liberal Democracy"       = worldmap$v2x_libdem,
                   "Participatory Democracy" = worldmap$v2x_partipdem,
                   "Deliberative Democracy"  = worldmap$v2x_delibdem)
    
    color1 <- switch(input$var, 
                    "Electoral Democracy" = "darkgreen",
                    "Liberal Democracy" = "black",
                    "Participatory Democracy" = "darkorange",
                    "Deliberative Democracy" = "darkviolet")
    
    color2 <- switch(input$var, 
                     "Electoral Democracy" = "lightgreen",
                     "Liberal Democracy" = "white",
                     "Participatory Democracy" = "yellow",
                     "Deliberative Democracy" = "violet")
    
    legend <- switch(input$var, 
                     "Electoral Democracy" = "Index Electoral Democracy",
                     "Liberal Democracy" = "Index Liberal Democracy",
                     "Participatory Democracy" = "Index Participatory Democracy",
                     "Deliberative Democracy" = "Index Deliberative Democracy")

    
    
    ggplot(worldmap ) + 
      geom_sf(aes(fill=data)) +
      labs(fill = legend) +
      scale_fill_gradient(low = color2, high = color1) +
      theme_void() + 
      theme(legend.position = "top")
    
   # percent_map(data, color, legend, input$range[1], input$range[2])
  })
}

# Run app ----
shinyApp(ui, server)