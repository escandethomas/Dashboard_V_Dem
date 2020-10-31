library(shiny)

# Define UI ----
ui <- fluidPage(
      img(src="Hzontal-logo.png", height = 70, width = 200),  
  titlePanel("Dashboard Example"),
  
  sidebarLayout(
    sidebarPanel(
      p("This dashboard aims at showing the kind of presentation that can be made out of your data and how they 
      can be used to present results in new ways and giving flexibility to observers"),
      p("You can find more information on the Horizontal's products, projects, and history on their ",
      a("website.", href = "https://wearehorizontal.org/"))
    ),
    
    mainPanel(
  h1("Democracies in the World"),
  p("We present some of the results taken from the V-Dem dataset. Varieties of Democracy (V-Dem) is a new approach to conceptualizing and measuring democracy. They provide a multidimensional
and disaggregated dataset that reflects the complexity of the concept of democracy as a system of rule that goes beyond the simple
presence of elections. The V-Dem project distinguishes between
seven high-level principles of democracy: electoral, liberal, participatory, deliberative, egalitarian, majoritarian and consensual, and
collects data to measure these principles.
"),
  p("More information can be found here: ", a("V-Dem", href = "https://www.v-dem.net/en/")),
  br(),
  p(em("Reference"),": Coppedge, Michael, John Gerring, Carl Henrik Knutsen, Staffan I. Lindberg, Jan Teorell, David Altman, Michael Bernhard, M. Steven Fish, Adam Glynn, Allen Hicken, Anna Luhrmann, Kyle L. Marquardt, Kelly McMann, Pamela Paxton, Daniel Pemstein, Brigitte Seim, Rachel Sigman, Svend-Erik Skaaning, Jeffrey Staton, Steven Wilson, Agnes Cornell, Nazifa Alizada, Lisa Gastaldi, Haakon Gjerl?w, Garry Hindle, Nina Ilchenko, Laura Maxwell, Valeriya Mechkova, Juraj Medzihorsky, Johannes von R?mer, Aksel Sundstr?m, Eitan Tzelgov, Yi-ting Wang, Tore Wig, and Daniel Ziblatt. 2020. Varieties of Democracy (V-Dem) Project."),
    )
  )
)
# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)