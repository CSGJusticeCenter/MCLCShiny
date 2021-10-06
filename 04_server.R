#######################################
# Project: MCLCShiny
# File: server.R
# Authors: Mari Roberts
# Date: September 27, 2021
# Description: 
#    Server for R Shiny app
#######################################
server <- function(input, output, session) {
  
  ###############
  # 1) About
  ###############
  
  # no code needed
  
  ###############
  # 2) Dashboard
  ###############
  
  
  ###############
  # 3) View Data
  ###############
  # filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- mclc
    if (input$states != "All") {
      data <- data[data$states == input$states,]
    }
    if (input$metric != "All") {
      data <- data[data$metric == input$metric,]
    }
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    data
  }))
  
  ###############
  # 4) Map
  ###############
  
      #####
      # 4b) leaflet map
      #####
      output$regional_map <- renderLeaflet({
        # leaflet map from regional_map.R
        regional_map})
      
}