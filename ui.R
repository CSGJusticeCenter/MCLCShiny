#######################################
# Project: MCLCShiny
# File: ui.R
# Authors: Mari Roberts
# Date: November 11, 2021
# Description: 
#    User interface
#######################################

# define UI
ui <- fluidPage(
  
  # theme
  # update theme https://medium.com/analytics-vidhya/building-custom-r-shiny-ui-66d446ef4dad
  theme = shinytheme("cosmo"),
  
  navbarPage(
    
    #_________________________________________________________________________________________
    # 1) About
    #_________________________________________________________________________________________

    "More Community, Less Confinement",
    tabPanel("About",

             h1("More Community, Less Confinement"),
             h2("A State-by-State Analysis on How Supervision Violations Impacted Prison Populations During the Pandemic"),
             p("This 50-state revocation dashboard explores how supervision violations impacted prison populations during—and prior to—the pandemic. The project was conducted in partnership with the Correctional Leaders Association with support from Arnold Ventures."),
             p("View the national report, 50 state reports, and our research methodology.")

    ),#tabPanel
    
    #_________________________________________________________________________________________
    # 2) Dashboard
    #_________________________________________________________________________________________
    
    tabPanel("Dashboard",
             
             ########
             # Side bar with drop downs and instructions
             # eventually would like to have the option to hide the panel????
             # https://stackoverflow.com/questions/42159804/how-to-collapse-sidebarpanel-in-shiny-app
             ########
             fluidRow(# select state
                      column(width = 6,
                             wellPanel(
                              # Select state
                                div(style = "font-size:16px;",
                                    selectInput(inputId = "state", label = div(style = "font-size:16px;
                                                                                        color: #FFFFFF;", 
                                                "Select State"), 
                                                choices = unique(adm_pop_long$states)
                                    )
                                ),
                              style = "background: #000000"
                              ) #wellPanel
                      ),#column
                      
                      # select population or admissions
                      column(width = 6, 
                             wellPanel(
                             # Select adm or pop for plot
                               div(style = "font-size:16px;",
                                   selectInput(inputId = "adm_or_pop", label = div(style = "font-size:16px;
                                                                                            color: #FFFFFF;", 
                                              "Admissions or Population"), 
                                               choices = unique(adm_pop_long$adm_or_pop)
                                   )
                               ),                             
                               style = "background: #000000"
                             ) #wellPanel
                      ) #column
             ), #fluidRow
             
             ########
             # State title
             ########
             fluidRow(column(width = 1,
                             ""),
                      align = "center",
                      column(width = 10,
                             #1) Text output
                             textOutput("selected_state"),
                             tags$head(tags$style("#selected_state{color: #000000;
                                                                   font-size: 42px;
                                                                   font-style: bold;
                                                    }")),
                             # add spacing
                             br(),
                             
                             #2) Paragraph about data
                             textOutput("selected_state_adm_pop"),
                             tags$head(tags$style("#selected_state_adm_pop{color: #000000;
                                                                           font-size: 16px;
                                                    }")),
                             
                             # add spacing
                             br(),
                             br()
                             
                             ), #column
                      column(width = 1,
                             "")
             ), #fluidRow
             
             
             # fluidRow(# Title admissions or population
             #          column(width = 6, 
             #                 textOutput("adm_pop_header"),
             #                 tags$head(tags$style("#adm_pop_header{color: #000000;
             #                                                       font-size: 22px;
             #                                                       font-style: bold;
             #                                                       text-align: center;
             #                                        }"))),
             #          
             #          # Title for supervision violations admissions or population
             #          column(width = 6, 
             #                 textOutput("viol_header"),
             #                 tags$head(tags$style("#viol_header{color: #000000;
             #                                                    font-size: 22px;
             #                                                    font-style: bold;
             #                                                    text-align: center;
             #                                        }")))
             # ), #fluidRow
             
             # add spacing
             br(),

             fluidRow(# Title for overall graph
               column(width = 6, 
                      textOutput("adm_pop_title"),
                      tags$head(tags$style("#adm_pop_title{color: #000000;
                                                           font-size: 18px;
                                                           font-style: bold;
                                                           text-align: center;
                                                    }"))),
               
               # Title for supervision violations graph
               column(width = 6, 
                      textOutput("viol_title"),
                      tags$head(tags$style("#viol_title{color: #000000;
                                                        font-size: 18px;
                                                        font-style: bold;
                                                        text-align: center;
                                                    }")))
             ), #fluidRow
             
             ########
             # Plots 
             ########
             fluidRow(column(width = 6, 
                             # Bar chart 
                             div(plotOutput("barchart", height = "100%"), align = "center")
                             ),
                      
                      column(width = 6, 
                             # Area chart 
                             div(plotOutput("barchart_2", height = "100%"), align = "center")
                             )
             ), #fluidRow
             
             # add spacing
             br(),
             br(),
             
             ########
             # Changes from 2018 and 2019
             ########
             
             fluidRow(# change from 2018
                      column(width = 2, 
                             valueBoxOutput("total_change_18")
                      ), #column

                      # change from 2019
                      column(width = 2, 
                             valueBoxOutput("total_change_19")
                      ), #column

                      # sentence about change
                      column(width = 2, 
                             textOutput("total_sentence_change"),
                             tags$head(tags$style("#total_sentence_change{color: #000000;
                                                                          font-size: 14px;
                                                                          text-align: left;
                                                                          vertical-align: middle;}"))
                      ), #column
                      
                      # change from 2018
                      column(width = 2, 
                             valueBoxOutput("viol_change_18")
                      ), #column
                     
                      # change from 2019
                      column(width = 2, 
                             valueBoxOutput("viol_change_19")
                      ), #column
                     
                      # sentence about change
                      column(width = 2, 
                             textOutput("viol_sentence_change"),
                             tags$head(tags$style("#viol_sentence_change{color: #000000;
                                                                         font-size: 14px;
                                                                         text-align: left;
                                                                         vertical-align: middle;}"))
                      ) #column
             ) #fluidRow
    ), #tabPanel
    
    #_________________________________________________________________________________________
    # 3) View Data
    #_________________________________________________________________________________________
    
    tabPanel("View Data",
             
             titlePanel(""),
             
             DT::dataTableOutput("dt"),
             
             downloadButton(outputId = "download_filtered",
                            label = "Download Filtered Data")
             
    ), #tabPanel
    
    #_________________________________________________________________________________________
    # 4) Map
    #_________________________________________________________________________________________
    
    tabPanel("Map",
             
             fluidRow(# title
               column(width = 12,
                      
                      # choose admissions or population
                      selectInput("adm_or_pop_map", 
                                  label = "Admissions or Population",
                                  choices = unique(mclc_change$adm_or_pop))
               ), #column
               
               column(width = 12,
                      
                      # select data
                      selectInput("data_map", 
                                  label = "Select Data",
                                  choices = unique(mclc_change$metric)),
               ), #column
               
               column(width = 12,
                      
                      # select year
                      selectInput("year_map", 
                                  label = "Change from Previous Year",
                                  choices = unique(mclc_change$year)),
               ) #column
             ), #fluidRow
             
             fluidRow(# title
               column(width = 12,
                      
                      "Title of Map"
                      
               ) #column
             ), #fluidRow
             
             fluidRow(# title
               column(width = 12,
                      leafletOutput(outputId = "map", height = 600),
                      tags$style(HTML(".leaflet-container { background: #FFFFFF;}"))
               ) #column
             ) #fluidRow
               
    ) #tabPanel
    
  ) #navbarPage
) #fluidPage
