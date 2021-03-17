library(ggplot2)
democracies <-readRDS("data/data_dashboard.rds")



# ui.R
ui <- fluidPage(
  titlePanel("Investigating Seedling Traits across Elevation"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "var_y", 
                  label = 'Trait', 
                  choices = c("Height (cm)" = "v2x_polyarchy", 
                              "Number of leaves" = "v2x_libdem"), 
                  selected = NULL)),
    mainPanel(plotOutput("traitplot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$traitplot <- renderPlot(
    ggplot(data = democracies, 
           aes_string(x = year, y = input$var_y)) +
      geom_point() + theme_classic()
  )
}

# Run the application 
shinyApp(ui = ui, server = server)