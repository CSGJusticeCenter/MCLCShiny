#######################################
# Project: MCLCShiny
# File: ui.R
# Authors: Mari Roberts
# Date: September 27, 2021
# Description: 
#    User interface for R Shiny app
#######################################

# Define UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # titlePanel(HTML("<h1><center><font size=14>More Community, Less Confinement</font></center></h1>")), 
                
                navbarPage(
                  
                  ###############
                  # 1) About
                  ###############
                  "More Community, Less Confinement",
                  tabPanel("About", 
                           
                           h1("More Community, Less Confinement"),
                           h2("A State-by-State Analysis on How Supervision Violations Impacted Prison Populations During the Pandemic"),
                           p("This 50-state revocation dashboard explores how supervision violations impacted prison populations during—and prior to—the pandemic. The project was conducted in partnership with the Correctional Leaders Association with support from Arnold Ventures."),
                           p("View the national report, 50 state reports, and our research methodology.")
                           
                  ),#tabPanel
                  
                  ###############
                  # 2) Dashboard 
                  ###############
                  tabPanel("Dashboard", 
                           
                           
                  ), #tabPanel
                  
                  ###############
                  # 3) View Data 
                  ###############
                  tabPanel("View Data", 
                           
                           titlePanel("View Data"),
                           
                           # Create a new Row in the UI for selectInputs
                           fluidRow(
                             column(4,
                                    selectInput("states",
                                                "states:",
                                                c("All",
                                                  unique(as.character(mclc$states))))),
                             column(4,
                                    selectInput("year",
                                                "year:",
                                                c("All",
                                                  unique(as.character(mclc$year))))),
                             column(4,
                                    selectInput("metric",
                                                "metric:",
                                                c("All",
                                                  unique(as.character(mclc$metric)))))
                           ), #fluidRow
                           # create table
                           DT::dataTableOutput("table")
                           
                  ),#tabPanel
                  
                  ###############
                  # 4) Map  
                  ###############
                  tabPanel("Map", 
                           
                           # Sidebar layout with input and output definitions
                           sidebarLayout(
                             
                             # Sidebar panel for inputs 
                             sidebarPanel(
                               
                               titlePanel("Desired Characteristics"),
                               
                               #####
                               # 4b) choose metric
                               #####
                               selectInput(inputId = "metric",
                                           label = "Data Type",
                                           choices = list("Total Admissions" = "total_admissions", "Total Population" = "total_population")),

                             ), #sidebarPanel
                             
                             # Main panel for displaying outputs
                             mainPanel(
                               
                               # hide errors
                               tags$style(type = "text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"),
                               
                               #####
                               # 4b) leaflet map
                               #####
                               
                               leafletOutput("regional_map")
                               
                             ) #mainPanel
                           ) #sidebarLayout
                  ) #tabPanel
                ) #navbarPage
) #fluidPage


