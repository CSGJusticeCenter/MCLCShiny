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
                           
                           #A. Select state
                           selectInput(inputId = "state", label = "Select State", choices = unique(adm_pop_long$states)),
                           verbatimTextOutput("state_choice"),
                           
                           #B. Select adm or pop for plot
                           selectInput(inputId = "adm_or_pop", label="Admissions or Population", choices = unique(adm_pop_long$adm_or_pop)),
                           
                           #C. select metric for plot
                           selectInput(inputId = "metric", label="Data", unique(adm_pop_long$metric)),
                           
                           #D. "Change from 2018-2020", br(), 
                           plotOutput("barchart")
                          
                  ), #tabPanel
                  
                  ###############
                  # 3) View Data 
                  ###############
                  tabPanel("View Data", 
                           
                           titlePanel("View Data"),
                           
                           # create table
                           DTOutput('table')
                           
                  ),#tabPanel
                  
                  ###############
                  # 4) Map  
                  ###############
                  tabPanel("Map", 
                           
                           # Sidebar layout with input and output definitions
                           sidebarLayout(
                             
                             # Sidebar panel for inputs 
                             sidebarPanel(
                               
                               titlePanel("Select Data"),
                               
                               #####
                               # 4a) Choose state
                               #####
                               selectizeInput(
                                 "stateInput", 'State', choices = "", multiple = FALSE,
                                 options = list(
                                   placeholder = 'Please select a state from below')
                               ),
                               
                               #####
                               # 4b) Choose year
                               #####
                               selectInput("yearInput", label = h3("Year"),
                                           choices = c("2018",
                                                       "2019",
                                                       "2020"))
                               
                             ), #sidebarPanel
                             
                             # Main panel for displaying outputs
                             mainPanel(
                               
                               # # hide errors
                               # tags$style(type = "text/css",
                               #            ".shiny-output-error { visibility: hidden; }",
                               #            ".shiny-output-error:before { visibility: hidden; }"),
                               
                               #####
                               # 4b) leaflet map
                               #####
                               
                               leafletOutput(outputId = "map", height = 800)
                               
                             ) #mainPanel
                           ) #sidebarLayout
                  ) #tabPanel
                ) #navbarPage
) #fluidPage


