# server.R - EICV7 Dashboard Server Logic
# Rwanda National Institute of Statistics EICV7 Survey Data Dashboard

library(tidyr)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(DT)

# Load poverty data from the first sheet of the Excel file
poverty_data <- read_excel("EICV7Rwanda.xlsx", sheet = 1)


#employment data

# ---- Load datasets from Excel (Sheet 2, different ranges) ----
employment_by_province_df <- read_excel("EICV7Rwanda.xlsx", sheet = 2, range = "A1:D6")        # Province | Male | Female
employment_by_residence_df <- read_excel("EICV7Rwanda.xlsx", sheet = 2, range = "A9:D11")       # Area of residence | Male | Female
employment_by_activity_df <- read_excel("EICV7Rwanda.xlsx", sheet = 2, range = "K1:N22")        # Economic activity | Male | Female
employment_by_age_df <- read_excel("EICV7Rwanda.xlsx", sheet = 2, range = "F1:I12")             # Age group | Male | Female

# Load education data
education_levels <- read_excel("EICV7Rwanda.xlsx", sheet = "education", range = "B2:H44")
literacy_numeracy <- read_excel("EICV7Rwanda.xlsx", sheet = "education", range = "L2:P42")
computer_literacy <- read_excel("EICV7Rwanda.xlsx", sheet = "education", range = "L46:O101")
attendance_rates <- read_excel("EICV7Rwanda.xlsx", sheet = "education", range = "R2:X38")
promotion_repetition <- read_excel("EICV7Rwanda.xlsx", sheet = "education", range = "AA2:AG72")

server <- function(input, output, session) {
  
  # Load data (placeholder - replace with actual data loading)
  # YOU: add appropriate data loading logic here
  
  # Reactive data for filtered datasets (placeholder)
  filtered_data <- reactive({
    # YOU: implement data filtering based on inputs
    return(NULL)  # replace with actual filtered data
  })
  
  #========================================
  # Home Tab
  #========================================
  output$summaryStats <- renderUI({
    # YOU: insert logic for Home tab here
    tags$div(
      h3("Dashboard Overview"),
      p("This dashboard provides interactive visualizations of the EICV7 survey data.")
    )
  })
  
  #========================================
  # Demographics Tab
  #========================================
  # Population Pyramid
  output$populationPyramid <- renderPlotly({
    # YOU: insert logic for Demographics tab here
    plot_ly() %>%
      layout(title = "Population by Age and Gender - Coming Soon")
  })
  
  # Demographic Table
  output$demographicTable <- renderDT({
    # YOU: insert logic for demographic table here
    data.frame(
      Category = c("Total Population", "Urban Population", "Rural Population"),
      Value = c("N/A", "N/A", "N/A")
    )
  })
  
  #========================================
  # Migration Tab
  #========================================
  output$migrationPlot <- renderPlotly({
    # YOU: insert logic for Migration tab here
    plot_ly() %>%
      layout(title = "Migration Patterns - Coming Soon")
  })
  
  #========================================
  # Health Tab
  #========================================
  output$healthStats <- renderPlotly({
    # YOU: insert logic for Health tab here
    plot_ly() %>%
      layout(title = "Health Indicators - Coming Soon")
  })
  
  #========================================
  # Education Tab
  #========================================
  # === Filters ===
  output$Education_secondary_filter <- renderUI({
    req(input$Education_mainfilter)
    df <- education_levels %>% filter(Mainfilter == input$Education_mainfilter)
    selectInput("Education_secondary", "Category:", choices = unique(df$Secondaryfilter))
  })
  
  output$Education_ln_secondary_filter <- renderUI({
    req(input$Education_ln_main)
    selectInput("Education_ln_secondary", "Category:", choices = unique(literacy_numeracy$Secondfilter[literacy_numeracy$Mainfilter == input$Education_ln_main]))
  })
  
  output$Education_ln_third_filter <- renderUI({
    req(input$Education_ln_main, input$Education_ln_secondary)
    df <- literacy_numeracy %>%
      filter(Mainfilter == input$Education_ln_main, Secondfilter == input$Education_ln_secondary)
    selectInput("Education_ln_third", "Type:", choices = unique(df$Thirdfilter))
  })
  
  output$Education_cl_secondary_filter <- renderUI({
    req(input$Education_cl_main)
    df <- computer_literacy %>% filter(Mainfilter == input$Education_cl_main)
    selectInput("Education_cl_secondary", "Category:", choices = unique(df$Secondaryfilter))
  })
  
  output$Education_att_third_filter <- renderUI({
    req(input$Education_att_secondary)
    df <- attendance_rates %>% filter(Secondaryfilter == input$Education_att_secondary)
    selectInput("Education_att_third", "Field:", choices = unique(df$Thirdfilter))
  })
  
  output$Education_att_school_filter <- renderUI({
    req(input$Education_att_secondary, input$Education_att_third)
    df <- attendance_rates %>% filter(Secondaryfilter == input$Education_att_secondary, Thirdfilter == input$Education_att_third)
    selectInput("Education_att_school", "Type of School:", choices = unique(df$`Type of school`))
  })
  
  output$Education_pr_third_filter <- renderUI({
    req(input$Education_pr_secondary)
    df <- promotion_repetition %>% filter(Secondaryfilter == input$Education_pr_secondary)
    selectInput("Education_pr_third", "Field:", choices = unique(df$Thirdfilter))
  })
  
  output$Education_pr_indicator_filter <- renderUI({
    req(input$Education_pr_secondary, input$Education_pr_third)
    df <- promotion_repetition %>% filter(Secondaryfilter == input$Education_pr_secondary, Thirdfilter == input$Education_pr_third)
    selectInput("Education_pr_indicator", "Type:", choices = unique(df$`Type of indicator`))
  })
  
  output$Education_pr_school_filter <- renderUI({
    req(input$Education_pr_secondary, input$Education_pr_third, input$Education_pr_indicator)
    df <- promotion_repetition %>%
      filter(Secondaryfilter == input$Education_pr_secondary,
             Thirdfilter == input$Education_pr_third,
             `Type of indicator` == input$Education_pr_indicator)
    selectInput("Education_pr_school", "Type of School:", choices = unique(df$`Type of school`))
  })
  
  # === Charts ===
  
  output$Education_levels_chart <- renderPlotly({
    req(input$Education_mainfilter, input$Education_secondary)
    df <- education_levels %>%
      filter(Mainfilter == input$Education_mainfilter, Secondaryfilter == input$Education_secondary) %>%
      pivot_longer(cols = c("No education", "Primary", "Secondary", "University"), names_to = "Level", values_to = "Percentage")
    
    plot_ly(df, x = ~Level, y = ~Percentage, color = ~Gender, type = "bar", barmode = "group",
            colors = c("Male" = "blue", "Female" = "deeppink")) %>%
      layout(title = paste("Education Level Distribution by", input$Education_mainfilter),
             yaxis = list(title = "Percentage"))
  })
  
  output$Education_literacy_numeracy_chart <- renderPlotly({
    req(input$Education_ln_main, input$Education_ln_secondary, input$Education_ln_third)
    df <- literacy_numeracy %>%
      filter(Mainfilter == input$Education_ln_main,
             Secondfilter == input$Education_ln_secondary,
             Thirdfilter == input$Education_ln_third)
    
    plot_ly(df, x = ~Thirdfilter, y = ~Male, name = "Male", type = "bar", marker = list(color = "blue")) %>%
      add_trace(y = ~Female, name = "Female", marker = list(color = "deeppink")) %>%
      layout(barmode = "group", title = paste(input$Education_ln_main, "Literacy Rate by Gender"),
             yaxis = list(title = "Percentage"))
  })
  
  output$Education_computer_literacy_chart <- renderPlotly({
    req(input$Education_cl_main, input$Education_cl_secondary)
    df <- computer_literacy %>%
      filter(Mainfilter == input$Education_cl_main, Secondaryfilter == input$Education_cl_secondary)
    
    plot_ly(df, x = ~Age, y = ~Computerliteracy, type = "bar",
            marker = list(color = "mediumseagreen")) %>%
      layout(title = paste("Computer Literacy by Age Group -", input$Education_cl_main),
             yaxis = list(title = "Percentage"))
  })
  
  output$Education_attendance_chart <- renderPlotly({
    req(input$Education_att_secondary, input$Education_att_third, input$Education_att_school)
    df <- attendance_rates %>%
      filter(Secondaryfilter == input$Education_att_secondary,
             Thirdfilter == input$Education_att_third,
             `Type of school` == input$Education_att_school)
    
    plot_ly(df, x = ~Thirdfilter, y = ~Total, name = "Total", type = "bar", marker = list(color = "gray")) %>%
      add_trace(y = ~Male, name = "Male", marker = list(color = "blue")) %>%
      add_trace(y = ~Female, name = "Female", marker = list(color = "deeppink")) %>%
      layout(barmode = "group", title = "Attendance Rates by School Type", yaxis = list(title = "Percentage"))
  })
  
  output$Education_promotion_chart <- renderPlotly({
    req(input$Education_pr_secondary, input$Education_pr_third, input$Education_pr_indicator, input$Education_pr_school)
    df <- promotion_repetition %>%
      filter(Secondaryfilter == input$Education_pr_secondary,
             Thirdfilter == input$Education_pr_third,
             `Type of indicator` == input$Education_pr_indicator,
             `Type of school` == input$Education_pr_school)
    
    plot_ly(df, x = ~Thirdfilter, y = ~Male, name = "Male", type = "bar", marker = list(color = "blue")) %>%
      add_trace(y = ~Female, name = "Female", marker = list(color = "deeppink")) %>%
      add_trace(y = ~Total, name = "Total", marker = list(color = "gray")) %>%
      layout(barmode = "group", title = paste(input$Education_pr_indicator, "by Gender"),
             yaxis = list(title = "Percentage"))
  })
  
  #========================================
  # Employment Tab
  #========================================
  # ---- Dynamic filter population ----
  observe({
    updateSelectInput(session, "employment_province_filter",
                      choices = unique(employment_by_province_df$Province),
                      selected = unique(employment_by_province_df$Province)[1])
    
    updateSelectInput(session, "employment_residence_filter",
                      choices = unique(employment_by_residence_df$`Area of residence`),
                      selected = unique(employment_by_residence_df$`Area of residence`)[1])
    
    updateSelectInput(session, "employment_activity_filter",
                      choices = unique(employment_by_activity_df$`Economic activity`),
                      selected = unique(employment_by_activity_df$`Economic activity`)[1])
    
    updateSelectInput(session, "employment_age_filter",
                      choices = unique(employment_by_age_df$`Age group`),
                      selected = unique(employment_by_age_df$`Age group`)[1])
  })
  
  # ---- Chart 1: Employment Gender Pie by Province ----
  output$employment_gender_pie <- renderPlotly({
    df <- employment_by_province_df %>%
      filter(Province == input$employment_province_filter)
    
    values <- c(df$Male, df$Female)
    labels <- c("Male", "Female")
    
    plot_ly(
      labels = labels,
      values = values,
      type = 'pie',
      textinfo = 'label+percent',
      marker = list(colors = c('#003b71', '#d4af37'))
    ) %>%
      layout(title = list(
        text = paste("Gender Distribution -", input$employment_province_filter),
        font = list(size = 18)
      ))
  })
  
  # ---- Chart 2: Employment Gender by Residence Pie ----
  output$employment_urban_rural_pie <- renderPlotly({
    df <- employment_by_residence_df %>%
      filter(`Area of residence` == input$employment_residence_filter)
    
    values <- c(df$Male, df$Female)
    labels <- c("Male", "Female")
    
    plot_ly(
      labels = labels,
      values = values,
      type = 'pie',
      textinfo = 'label+percent',
      marker = list(colors = c('#2176ae', '#d4af37'))
    ) %>%
      layout(title = list(
        text = paste("Gender by Residence -", input$employment_residence_filter),
        font = list(size = 18)
      ))
  })
  
  # ---- Chart 3: Employment by Activity and Gender (Bar) ----
  output$employment_gender_by_activity_bar <- renderPlotly({
    df <- employment_by_activity_df %>%
      filter(`Economic activity` == input$employment_activity_filter)
    
    df_long <- df %>%
      select(Male, Female) %>%
      pivot_longer(cols = everything(), names_to = "Gender", values_to = "Count")
    
    plot_ly(df_long, x = ~Gender, y = ~Count, type = 'bar', color = ~Gender) %>%
      layout(title = list(
        text = paste("Gender in", input$employment_activity_filter),
        font = list(size = 18)
      ))
  })
  
  # ---- Chart 4: Employment by Age Group and Gender (Bar with Age Filter) ----
  output$employment_age_gender_bar <- renderPlotly({
    df <- employment_by_age_df %>%
      filter(`Age group` == input$employment_age_filter)
    
    df_long <- df %>%
      select(Male, Female) %>%
      pivot_longer(cols = everything(), names_to = "Gender", values_to = "Count")
    
    plot_ly(df_long, x = ~Gender, y = ~Count, type = 'bar', color = ~Gender) %>%
      layout(title = list(
        text = paste("Employment by Gender in Age Group:", input$employment_age_filter),
        font = list(size = 18)
      ))
  })
  
  #========================================
  # Housing Tab
  #========================================
  output$housingPlot <- renderPlotly({
    # YOU: insert logic for Housing tab here
    plot_ly() %>%
      layout(title = "Housing Indicators - Coming Soon")
  })
  
  #========================================
  # Agriculture Tab
  #========================================
  output$agriculturePlot <- renderPlotly({
    # YOU: insert logic for Agriculture tab here
    plot_ly() %>%
      layout(title = "Agricultural Statistics - Coming Soon")
  })
  
  #========================================
  # Poverty Tab
  #========================================
  # ---- 1. Update region filter input ----
  observe({
    updateSelectInput(session, "poverty_region_filter",
                      choices = unique(poverty_data$Filter),
                      selected = "Rwanda"
    )
  })
  
  # ---- 2. Reactive data based on selected region ----
  selected_poverty <- reactive({
    req(input$poverty_region_filter)
    poverty_data %>% filter(Filter == input$poverty_region_filter)
  })
  
  # ---- 3. Total vs Extreme Poverty Plot ----
  output$poverty_rates_plot <- renderPlotly({
    poverty_df <- selected_poverty()
    
    total <- as.numeric(poverty_df$`Total Poverty rate (%)`)
    extreme <- as.numeric(poverty_df$`Extreme Poverty rate (%)`)
    
    plot_ly(
      x = c("Total Poverty", "Extreme Poverty"),
      y = c(total, extreme),
      type = 'bar',
      marker = list(color = c('#003b71', '#d4af37')),
      text = sprintf("%.2f", c(total, extreme)),
      textposition = 'outside',
      textfont = list(color = 'black', size = 14)
    ) %>%
      layout(
        yaxis = list(title = "%", fixedrange = TRUE, range = c(0, 50), tickfont = list(size = 16)),
        xaxis = list(title = "", fixedrange = TRUE, tickfont = list(size = 16)),
        barmode = 'group'
      )
  })
  
  # ---- 4. Modeled vs Actual Poverty Plot ----
  output$modeled_vs_actual_plot <- renderPlotly({
    modeled_df <- selected_poverty()
    
    # Use exact column names with the extra space
    actual_2024 <- as.numeric(modeled_df$`% of individuals who are poor in  2024`)
    modeled_2017 <- as.numeric(modeled_df$`% of individuals who were modelled to be poor in  2017`)
    
    plot_ly(
      x = c("Poor in 2024", "Modeled in 2017"),
      y = c(actual_2024, modeled_2017),
      type = 'bar',
      marker = list(color = c('#2176ae', '#58595b')),
      text = sprintf("%.2f", c(actual_2024, modeled_2017)),
      textposition = 'outside',
      textfont = list(color = 'black', size = 14),
      hoverinfo = 'text'
    ) %>%
      layout(
        yaxis = list(title = "%", fixedrange = TRUE, range = c(0, 50), tickfont = list(size = 16)),
        xaxis = list(title = "", fixedrange = TRUE, tickfont = list(size = 16)),
        barmode = 'group'
      )
  })
  
  
  # ---- 5. MPI Breakdown Plot ----
  output$mpi_plot <- renderPlotly({
    mpi_df <- selected_poverty()
    
    incidence <- as.numeric(mpi_df$`Incidence (%)`)
    intensity <- as.numeric(mpi_df$`Intensity (%)`)
    mpi <- as.numeric(mpi_df$MPI)
    
    plot_ly(
      x = c("Incidence", "Intensity", "MPI"),
      y = c(incidence, intensity, mpi),
      type = 'bar',
      marker = list(color = c('#003b71', '#2176ae', '#d4af37')),
      text = sprintf("%.2f", c(incidence, intensity, mpi)),
      textposition = 'outside',
      textfont = list(color = 'black', size = 14)
    ) %>%
      layout(
        yaxis = list(title = "%", fixedrange = TRUE, range = c(0, 50), tickfont = list(size = 16)),
        xaxis = list(title = "", fixedrange = TRUE, tickfont = list(size = 16)),
        barmode = 'group'
      )
  })
  
  
  #========================================
  # Map View Tab
  #========================================
  # Rwanda Map
  output$rwandaMap <- renderLeaflet({
    # YOU: insert logic for Map View tab here
    
    # Placeholder map of Rwanda
    leaflet() %>%
      setView(lng = 30.0587, lat = -1.9403, zoom = 8) %>%
      addTiles() %>%
      addMarkers(lng = 30.0587, lat = -1.9403, popup = "Kigali, Rwanda")
  })
  
  # Update map based on selection
  observeEvent(input$mapVariable, {
    # YOU: insert logic to update map based on variable selection
  })
  
  observeEvent(input$mapLevel, {
    # YOU: insert logic to update map based on geographic level
  })
  
  #========================================
  # NST1 / SDG Tracker Tab
  #========================================
  output$nstSdgPlot <- renderPlotly({
    # YOU: insert logic for NST1/SDG Tracker tab here
    plot_ly() %>%
      layout(title = "NST1/SDG Progress Indicators - Coming Soon")
  })
  
  # Global session logic
  observe({
    # YOU: insert any global reactive logic here
  })
  
}