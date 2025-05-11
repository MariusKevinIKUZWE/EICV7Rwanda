# ui.R - EICV7 Dashboard UI Structure
# Rwanda National Institute of Statistics EICV7 Survey Data Dashboard


library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)
library(dplyr)
library(readxl)

ui <- fluidPage(
  # Include custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Open+Sans:400,600,700")
  ),
  
  # NISR logo and title bar
  fluidRow(
    column(2, 
           tags$img(src = "www/nisr_logo.png", height = "80px", alt = "NISR Logo"),
           style = "padding: 15px;"
    ),
    column(10,
           tags$div(
             tags$h1("EICV7 National Survey Dashboard", 
                     style = "color: var(--nisr-blue); margin-top: 20px;"),
             tags$p("Rwanda National Institute of Statistics (NISR)",
                    style = "color: var(--nisr-grey);")
           )
    ),
    style = "background-color: white; margin-bottom: 15px; border-bottom: 5px solid var(--nisr-gold);"
  ),
  
  # Main Navigation
  navbarPage(
    title = "",
    id = "mainNav",
    theme = shinythemes::shinytheme("flatly"),
    
    # Home Tab
    tabPanel(
      "Home",
      fluidRow(
        column(12,
               tags$div(
                 class = "panel",
                 tags$h2("Rwanda EICV7 Survey Dashboard"),
                 tags$p("Welcome to the dashboard for exploring the 7th Integrated Household Living Conditions Survey (EICV7) data."),
                 tags$p("Use the tabs above to navigate through different sections of the dashboard."),
                 tags$h3("About EICV7"),
                 tags$p("The Integrated Household Living Conditions Survey (Enquête Intégrale sur les Conditions de Vie des ménages, EICV) 
                        is Rwanda's main living standards survey, conducted approximately every 3 years."),
                 # YOU: add UI content here later
               )
        )
      )
    ),
    
    # Demographics Tab
    tabPanel(
      "Demographics",
      fluidRow(
        column(12, 
               tags$h2("Population Demographics", style = "color: var(--nisr-blue);"),
               tags$div(
                 class = "panel",
                 tags$h3("Population Distribution"),
                 # YOU: add UI content here later
                 h3("Coming soon...")
               )
        )
      )
    ),
    
    # Migration Tab
    tabPanel(
      "Migration",
      fluidRow(
        column(12, 
               tags$h2("Population Migration", style = "color: var(--nisr-blue);"),
               tags$div(
                 class = "panel",
                 tags$h3("Migration Patterns"),
                 # YOU: add UI content here later
                 h3("Coming soon...")
               )
        )
      )
    ),
    
    # Health Tab
    tabPanel(
      "Health",
      fluidRow(
        column(12, 
               tags$h2("Health Statistics", style = "color: var(--nisr-blue);"),
               tags$div(
                 class = "panel",
                 tags$h3("Health Indicators"),
                 # YOU: add UI content here later
                 h3("Coming soon...")
               )
        )
      )
    ),
    
    # Education Tab
    tabPanel(
      "Education",
      fluidRow(
        column(12, 
               tags$h2("Education Statistics", style = "color: var(--nisr-blue);"),
               tags$div(
                 class = "panel",
                 tags$h3("Education Indicators"),
                 # YOU: add UI content here later
                 h3("Coming soon...")
               )
        )
      )
    ),
    
    # Employment Tab
    tabPanel(
      "Employment",
      fluidRow(
        column(12, 
               tags$h2("Employment Statistics", style = "color: var(--nisr-blue);"),
               tags$div(
                 class = "panel",
                 tags$h3("Employment Indicators"),
                 # YOU: add UI content here later
                 h3("Coming soon...")
               )
        )
      )
    ),
    
    # Housing Tab
    tabPanel(
      "Housing",
      fluidRow(
        column(12, 
               tags$h2("Housing Conditions", style = "color: var(--nisr-blue);"),
               tags$div(
                 class = "panel",
                 tags$h3("Housing Indicators"),
                 # YOU: add UI content here later
                 h3("Coming soon...")
               )
        )
      )
    ),
    
    # Agriculture Tab
    tabPanel(
      "Agriculture",
      fluidRow(
        column(12, 
               tags$h2("Agricultural Statistics", style = "color: var(--nisr-blue);"),
               tags$div(
                 class = "panel",
                 tags$h3("Agricultural Indicators"),
                 # YOU: add UI content here later
                 h3("Coming soon...")
               )
        )
      )
    ),
    
    # Poverty Tab
    tabPanel(
      "Poverty",
      fluidRow(
        column(
          width = 12,
          # Title
          tags$h2("Poverty Analysis", style = "color: var(--nisr-blue); margin-bottom: 10px;"),
          
          # Region filter (dynamic)
          selectInput("poverty_region_filter", "Select Region:",
                      choices = NULL,
                      width = "300px"),  # Better visual size
          br(),  # spacing between filter and charts
        )
      ),
      
      # New row for the charts
      fluidRow(
        column(4,  # First chart, takes up 4/12 of the row
               tags$div(
                 class = "panel",
                 style = "background-color: white; padding: 20px; border-radius: 10px; margin-bottom: 30px;
                 box-shadow: 0 4px 10px rgba(0,0,0,0.05);",
                 tags$h3("Poverty Rate vs Extreme Poverty Rate", style = "color: var(--nisr-blue);"),
                 plotlyOutput("poverty_rates_plot")
               )
        ),
        column(4,  # Second chart, also takes up 4/12 of the row
               tags$div(
                 class = "panel",
                 style = "background-color: white; padding: 20px; border-radius: 10px; margin-bottom: 30px;
                 box-shadow: 0 4px 10px rgba(0,0,0,0.05);",
                 tags$h3("Modeled vs Actual Poverty (2017 vs 2024)", style = "color: var(--nisr-blue);"),
                 plotlyOutput("modeled_vs_actual_plot")
               )
        ),
        column(4,  # Third chart, takes up the remaining 4/12 of the row
               tags$div(
                 class = "panel",
                 style = "background-color: white; padding: 20px; border-radius: 10px;
                 box-shadow: 0 4px 10px rgba(0,0,0,0.05);",
                 tags$h3("Multidimensional Poverty Index (MPI)", style = "color: var(--nisr-blue);"),
                 plotlyOutput("mpi_plot")
               )
        )
      )
    ),
    
    
    
    # Map View Tab
    tabPanel(
      "Map View",
      fluidRow(
        column(3,
               tags$div(
                 class = "panel",
                 tags$h3("Map Controls"),
                 selectInput("mapVariable", "Select Variable:", 
                             choices = c("Poverty Rate", "Population Density", "Education Level")),
                 selectInput("mapLevel", "Geographic Level:", 
                             choices = c("Province", "District", "Sector")),
                 # YOU: add UI content here later
               )
        ),
        column(9,
               tags$div(
                 class = "panel",
                 leafletOutput("rwandaMap", height = "600px")
                 # YOU: add UI content here later
               )
        )
      )
    ),
    
    # NST1 / SDG Tracker Tab
    tabPanel(
      "NST1 / SDG Tracker",
      fluidRow(
        column(12, 
               tags$h2("National Strategy for Transformation & SDG Progress", 
                       style = "color: var(--nisr-blue);"),
               tags$div(
                 class = "panel",
                 tags$h3("Key Performance Indicators"),
                 # YOU: add UI content here later
                 h3("Coming soon...")
               )
        )
      )
    )
  ),
  
  # Footer
  tags$footer(
    fluidRow(
      column(12,
             tags$p("© 2025 National Institute of Statistics of Rwanda (NISR). All rights reserved."),
             tags$p("Data from the 7th Integrated Household Living Conditions Survey (EICV7).")
      )
    ),
    style = "background-color: var(--nisr-blue); color: white; padding: 20px; margin-top: 30px; text-align: center;"
  )
)