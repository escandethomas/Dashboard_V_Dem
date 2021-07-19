library(ggplot2)
library(plotly)
library(shinythemes)
library(shiny)
library(tidyr)
library(tidyverse)
library(ggthemes)
library(magrittr)
library(dplyr)
library(palmerpenguins)
library(hrbrthemes)

democracies <-readRDS("data/data_dashboard.rds")


# User interface ----
ui <- fluidPage(theme = shinytheme("flatly"),
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
)



# Server logic ----
server <- function(input, output) {

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
      ggplot(data(), aes(x = year,
                         group = country_name,
                         colour = country_name,
                         text =  paste0("<b>Year: </b>", year,
                                        "<b><br>Country: </b>", country_name,
                                        "<br>Liberal Democracy: ", v2x_libdem ,
                                        "<br>Participatory Democracy: ", v2x_partipdem ,
                                        "<br>Electoral Democracy: ", v2x_polyarchy ,
                                        "<br>Deliberative Democracy: ", v2x_delibdem ) )) + 
        geom_line(aes_string(y = input$var_y)) +
        labs(x = "Year" , y = yl()) +
        theme_classic() +
        scale_color_discrete(name = "") +
        theme(
          legend.position = "bottom")
}
      

    output$index_evolution <- renderPlotly({ 
        print(
          ggplotly(myPlot(), tooltip = "text") %>%   
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
  
}

# Run app ----
shinyApp(ui, server)