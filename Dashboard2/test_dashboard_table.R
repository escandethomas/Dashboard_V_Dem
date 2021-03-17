library(DT)
# Used packages
pacotes = c("shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders","tidyverse", "magrittr", "ggthemes", "tidyr", "ggplot2",
            "scales", "knitr", "kableExtra", "ggfortify","dplyr","FNN", "rsconnect")

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

democracies <-readRDS("data/data_dashboard.rds")


# User interface ----
ui <- fluidPage(
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
)

# Server logic ----
server <- function(input, output) {
  
  # Data contains the dataset we want to show. It is a reactive function
  
  
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
    
}

# Run app ----
shinyApp(ui, server)



