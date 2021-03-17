
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

  
  ##### Define UI #####
  shinyUI( navbarPage("Horizontal Dashboard", theme = shinytheme("flatly"),
                   
                   #### Presentation Tab ####                
                   tabPanel("Presentation", theme = shinytheme("flatly"), 
                            img(src="Hzontal-logo.png", height = 70, width = 200),  
                            titlePanel("Dashboard Example"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                p("This dashboard aims at showing the kind of presentation that can be made out of your data and how they 
                                  can be used to present results in new ways and giving flexibility to observers", style = "font-size: 1.1em", align = "justify"),
                                br(),
                                p("We present here a table, a graph and a map. All these elements can be adapted to your needs and the specific data you are using. The dataset and graphs can be downloaded.", style = "font-size: 1.1em", align = "justify"),
                                br(),
                                p("You can find more information on the Horizontal's products, projects, and history on the following ",
                                  a("website.", href = "https://wearehorizontal.org/"), style = "font-size: 1.1em", align = "justify")
                                ),
                              
                              mainPanel(
                                h1("Democracies in the World"),
                                p("We present some of the results taken from the V-Dem dataset. Varieties of Democracy (V-Dem) is a new approach to conceptualizing and measuring democracy. They provide a multidimensional
                                  and disaggregated dataset that reflects the complexity of the concept of democracy as a system of rule that goes beyond the simple
                                  presence of elections. The V-Dem project distinguishes between
                                  seven high-level principles of democracy: electoral, liberal, participatory, deliberative, egalitarian, majoritarian and consensual, and
                                  collects data to measure these principles.
                                  ", align = "justify"),
                                p("More information can be found here: ", a("V-Dem", href = "https://www.v-dem.net/en/")),
                                br(),
                                p(em("Reference"),": Coppedge, Michael, John Gerring, Carl Henrik Knutsen, Staffan I. Lindberg, Jan Teorell, David Altman, Michael Bernhard, M. Steven Fish, Adam Glynn, Allen Hicken, Anna Luhrmann, 
                                  Kyle L. Marquardt, Kelly McMann, Pamela Paxton, Daniel Pemstein, Brigitte Seim, Rachel Sigman, Svend-Erik Skaaning, Jeffrey Staton, Steven Wilson, Agnes Cornell, Nazifa Alizada, Lisa Gastaldi, 
                                  Haakon Gjerl?w, Garry Hindle, Nina Ilchenko, Laura Maxwell, Valeriya Mechkova, Juraj Medzihorsky, Johannes von R?mer, Aksel Sundstr?m, Eitan Tzelgov, Yi-ting Wang, Tore Wig, and Daniel Ziblatt. 2020. 
                                  Varieties of Democracy (V-Dem) Project.")
                                )
                                )
                                ),
                   
                   
                   #### Table Tab #### 
                   tabPanel("Table",   
                            titlePanel("Summary Statistics"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Create a table presenting summary statistics"),
                                
                                checkboxGroupInput(inputId = "country_table", 
                                                   label = "Choose a country to display",
                                                   choices = c("Mexico", "Ghana",  "South Africa"), selected = "Mexico"),   
                                
                                sliderInput(inputId ="year_table", label ="Year Range",
                                            min = 1970, max = 2019, value = c(1970, 2019), sep = ""), 
                                downloadButton(outputId = "down_data", label = "Download the dataset")
                              ),
                              
                              # Create the table.
                              mainPanel(DT::dataTableOutput("table"))
                            )
                   ),
                   
                   
                   #### Graph Tab #### 
                   tabPanel("Graph", theme = shinytheme("flatly"),
                            br(),
                            br(),
                            
                            titlePanel("Democracies in the World"),
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Visualization of the evolution of democratic indices over time."),
                                
                                selectInput(inputId = "var_y", 
                                            label = "Choose a variable to display",
                                            choices = c("Electoral Democracy Index" = "v2x_polyarchy", 
                                                        "Liberal Democracy Index" = "v2x_libdem", 
                                                        "Participatory Democracy Index" = "v2x_partipdem", 
                                                        "Deliberative Democracy Index" = "v2x_delibdem"), selected = "Electoral Democracy"),
                                
                                checkboxGroupInput(inputId = "country", 
                                                   label = "Choose a country to display",
                                                   choices = c("France", "Italy",  "Spain", "Mexico", "Ghana",  "South Africa",
                                                               "Afghanistan","Sweden"), selected = "France"),   
                                
                                sliderInput(inputId ="year", label ="Year Range",
                                            min = 1970, max = 2019, value = c(1970, 2019), sep = ""), 
                                
                                radioButtons(inputId = "var3", label = "Select the file type for the download", choices = list("png", "pdf"))
                                
                              ),
                              
                              mainPanel(
                                plotlyOutput("index_evolution"),
                                br(),
                                br(),
                                downloadButton(outputId = "down", label = "Download the plot")
                              )
                            )
                   ),
                   
                   
                   #### Map Tab #### 
                   tabPanel("Map",
                            titlePanel("Liberal Democracies in the World"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Create world map with 
        information on different democratic indices."),
                                
                                selectInput("var_map", 
                                            label = "Choose a variable to display",
                                            choices = c("Electoral Democracy", "Liberal Democracy",
                                                        "Participatory Democracy", "Deliberative Democracy"),
                                            selected = "Electoral Democracy"),
                                
                                sliderInput("year_map", 
                                            label = "Year of interest:",
                                            min = 1970, max = 2019, value = c(2000), sep = "", step = 1, animate=animationOptions(interval=1000, loop=FALSE)),
                                
                                p("Click on the play button to see the evolution over the years")
                                
                              ),
                              
                              mainPanel(plotOutput("map"))
                            )),
                   
                   #### Map Advanced Tab #### 
                   tabPanel("Map_advanced",                   
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
  
)
