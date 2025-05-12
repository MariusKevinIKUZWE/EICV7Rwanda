library(shiny)
library(readxl)
library(plotly)
library(dplyr)
library(tidyr)



ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  h2("Education Dashboard"),
  

  
  ,
  
  
)

server <- function(input, output, session) {
  

}

shinyApp(ui, server)
