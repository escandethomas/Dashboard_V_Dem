

# Used packages
pacotes = c("shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders","tidyverse", "magrittr", "ggthemes", "tidyr", "ggplot2",
            "scales", "knitr", "kableExtra", "ggfortify","dplyr","FNN", "rsconnect","DT","maps","mapproj","sf","rworldmap","ggmap","rgeos")

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

democracies <-readRDS("data/data_dashboard.rds")

worldmap <- getMap(resolution = "low")

worldmap@data <-  worldmap@data %>% 
  dplyr::select(ADMIN,REGION,continent, POP_EST)

worldmap <- st_as_sf(worldmap)

worldmap <- worldmap %>%
  mutate(country_name = as.character(ADMIN))

worldmap <- worldmap %>%  
  filter(REGION != "Antarctica")

# Define UI ----
ui <- navbarPage("Horizontal Dashboard", theme = shinytheme("flatly"),
                 
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
                                                 choices = c("France", "Italy",  "Spain"), selected = "France"),   
                              
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
))
)

# Define server logic ----
server <- function(input, output) {
  
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
      geom_point() +
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
   
  
}

# Run the app ----
shinyApp(ui = ui, server = server)