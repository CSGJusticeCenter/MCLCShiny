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
                  # 1) About____________________________________________________
                  ###############
                  "More Community, Less Confinement",
                  tabPanel("About", 
                           
                           h1("More Community, Less Confinement"),
                           h2("A State-by-State Analysis on How Supervision Violations Impacted Prison Populations During the Pandemic"),
                           p("This 50-state revocation dashboard explores how supervision violations impacted prison populations during—and prior to—the pandemic. The project was conducted in partnership with the Correctional Leaders Association with support from Arnold Ventures."),
                           p("View the national report, 50 state reports, and our research methodology.")
                           
                  ),#tabPanel
                  
                  ###############
                  # 2) Dashboard________________________________________________ 
                  ###############
                  tabPanel("Dashboard", 
                           
                           sidebarLayout(
                             sidebarPanel(
                               #2A) Select state
                               selectInput(inputId = "state", label = "Select State", choices = unique(adm_pop_long$states)),
                               
                               #2B) Select adm or pop for plot
                               selectInput(inputId = "adm_or_pop", label="Admissions or Population", choices = unique(adm_pop_long$adm_or_pop)),
                               
                               #2C) select metric for plot
                               selectInput(inputId = "metric", label="Data", unique(adm_pop_long$metric))
                             ),
                             mainPanel(

                               #2D) Text output
                               textOutput("selected_state"),
                               tags$head(tags$style("#selected_state{color: #000000;
                                                             font-size: 48px;
                                                             font-style: bold;
                                                    }")
                                         ),
                               
                               #2E) Plot title
                               textOutput("selected_var"),
                               tags$head(tags$style("#selected_var{color: #000000;
                                                             font-size: 18px;
                                                    }")
                               ),
                               
                               #2E) Bar chart 
                               plotOutput("barchart"),
                               
                             ) #mainPanel
                           ) #sidebarLayout

                  ), #tabPanel
                  
                  ###############
                  # 3) View Data________________________________________________ 
                  ###############
                  tabPanel("View Data", 
                           
                           titlePanel("View Data"),
                           
                           # create table
                           DTOutput('table')
                           
                  ),#tabPanel
                  
                  ###############
                  # 4) Map______________________________________________________  
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
                                 "stateInput", 'State', 
                                 choices = "",
                                 multiple = FALSE,
                                 options = list(
                                   placeholder = 'Please select a state from below')
                               ),
                               
                               #####
                               # 4b) Choose year
                               #####
                               selectInput("yearInput", 
                                           label = "Year",
                                           choices = c("2018",
                                                       "2019",
                                                       "2020"))
                               
                             ), #sidebarPanel
                             
                             # Main panel for displaying outputs
                             mainPanel(

                               #####
                               # 4b) leaflet map
                               #####
                               
                               leafletOutput(outputId = "map", height = 600)
                               
                             ) #mainPanel
                           ) #sidebarLayout
                  ), #tabPanel
                  
                  ###############
                  # 5) Dashboard________________________________________________ 
                  ###############
                  tabPanel("Dashboard 2", 
                           
                           sidebarLayout(
                             sidebarPanel(
                               #2A) Select state
                               selectInput(inputId = "state_2", label = "Select State", choices = unique(adm_pop_long$states)),
                               
                               #2B) Select adm or pop for plot
                               selectInput(inputId = "adm_or_pop_2", label="Admissions or Population", choices = unique(adm_pop_long$adm_or_pop))
                               
                             ),
                             mainPanel(
                               
                               #2D) Text output
                               textOutput("selected_state_2"),
                               tags$head(tags$style("#selected_state_2{color: #000000;
                                                             font-size: 48px;
                                                             font-style: bold;
                                                    }")
                               ),
                               
                               #2E) Plot title
                               textOutput("selected_adm_pop_2"),
                               tags$head(tags$style("#selected_adm_pop_2{color: #000000;
                                                             font-size: 32px;
                                                    }")
                               ),
                               
                               #2E) Bar chart 
                               plotOutput("barchart_2"),
                               
                             ) #mainPanel
                           ) #sidebarLayout
                           
                  ), #tabPanel
                  
                  
                ) #navbarPage
) #fluidPage


