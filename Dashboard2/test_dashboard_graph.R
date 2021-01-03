library(ggplot2)
democracies <- readRDS("C:/Users/escan/OneDrive/Bureau/Github/Dashboard_V_Dem/Dashboard2/data/data_dashboard.rds")


# User interface ----
ui <- fluidPage(
  titlePanel("Democracies in the World"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualization of the evolution of democratic indices over time."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Electoral Democracy", "Liberal Democracy",
                              "Participatory Democracy", "Deliberative Democracy"),
                  selected = "Electoral Democracy"),
      
    selectInput("country", 
                label = "Choose a country to display",
                choices = c("France", "Italy",
                            "Spain"),
                selected = "France"),      
    
    dateRangeInput("dates", label = h3("Date range"), format = "yyyy")
    ),
    
    mainPanel(plotOutput("index_evolution"))
  )
)

# Server logic ----
server <- function(input, output) {

    data  <- reactive({
      subset(democracies, year >= 1970 & year <= 2015 & country_name == input$country) 
  
       })
    

    
  output$index_evolution <- renderPlot({    

    
    
    legend <- switch(input$var, 
                     "Electoral Democracy" = "Index Electoral Democracy",
                     "Liberal Democracy" = "Index Liberal Democracy",
                     "Participatory Democracy" = "Index Participatory Democracy",
                     "Deliberative Democracy" = "Index Deliberative Democracy")
    

    ggplot(data(), aes(year,v2x_polyarchy)) + 
      geom_point() +
      labs(x = "Year" , y = legend) +
      theme_minimal()
  })
}

# Run app ----
shinyApp(ui, server)