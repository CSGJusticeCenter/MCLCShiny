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
  
  # change font of entire dashboard
  tags$head(tags$style(HTML('*{generic-family: "sans-serif"};'))),
  
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
             p("This 50-state revocation dashboard explores how supervision violations impacted prison populations during and prior to the pandemic. The project was conducted in partnership with the Correctional Leaders Association with support from Arnold Ventures."),
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
                                                                                        color: #000000;", 
                                                "Select State"), 
                                                choices = unique(adm_pop_long$states)
                                    )
                                ),
                              style = "background: #E7E7E7"
                              ) #wellPanel
                      ),#column
                      
                      # select population or admissions
                      column(width = 6, 
                             wellPanel(
                             # Select adm or pop for plot
                               div(style = "font-size:16px;",
                                   selectInput(inputId = "adm_or_pop", label = div(style = "font-size:16px;
                                                                                            color: #000000;", 
                                              "Admissions or Population"), 
                                               choices = unique(adm_pop_long$adm_or_pop)
                                   )
                               ),                             
                               style = "background: #E7E7E7"
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
                                                                           font-size: 20px;
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

             fluidRow(
               column(width = 1),
               
               # Title for overall graph
               column(width = 4, 
                      textOutput("adm_pop_title"),
                      tags$head(tags$style("#adm_pop_title{color: #FFFFFF;
                                                           font-size: 20px;
                                                           font-style: bold;
                                                           text-align: center;
                                                           background-color:#000000;
                                                           line-height: 32px;
                                                    }"))),
               column(width = 2),
               
               # Title for supervision violations graph
               column(width = 4, 
                      textOutput("viol_title"),
                      tags$head(tags$style("#viol_title{color: #FFFFFF;
                                                        font-size: 20px;
                                                        font-style: bold;
                                                        text-align: center;
                                                        background-color:#000000;
                                                        line-height: 32px;
                                                    }"))),
               column(width = 1)
               
             ), #fluidRow
             
             br(),
             
             ########
             # Plots 
             ########
             fluidRow(column(width = 6, 
                             # Bar chart 
                             div(plotlyOutput("barchart", height = "75%", width = "75%"), align = "center")
                             ),
                      
                      column(width = 6, 
                             # Area chart 
                             div(plotlyOutput("barchart_2", height = "75%", width = "75%"), align = "center")
                             )
             ), #fluidRow
             
             # add spacing
             br(),
             br(),
             
             ########
             # Changes from 2018 and 2019
             ########
             
             tags$style(".small-box.bg-green {background-color: #FFFFFF !important; color: #000000 !important; }"),
             tags$style(".small-box.bg-red   {background-color: #FFFFFF !important; color: #000000 !important; }"),
             tags$style(".small-box          {border: 3px; border-style: solid; border-color: #E7E7E7 !important; 
                                              border-radius: 5px; padding: 0.5em; }"),
             
             fluidRow(# change from 2018
                      column(width = 1),
                      
                      column(width = 1, 
                             valueBoxOutput("total_change_18", width = 25),
                             tags$head(tags$style("#total_change_18{color: #000000;
                                                                    font-size: 16px;
                                                                    text-alight: left;
                             }"))
                      ), #column

                      # change from 2019
                      column(width = 1, 
                             valueBoxOutput("total_change_19", width = 25),
                             tags$head(tags$style("#total_change_19{color: #000000;
                                                                    font-size: 16px;
                                                                    text-alight: left;
                             }"))
                      ), #column

                      # sentence about change
                      column(width = 2, 
                             textOutput("total_sentence_change"),
                             tags$head(tags$style("#total_sentence_change{color: #000000;
                                                                          font-size: 16px;
                                                                          text-align: left;
                                                                          vertical-align: middle;}"))
                      ), #column
                      
                      column(width = 2),
                      
                      # change from 2018
                      column(width = 1, 
                             valueBoxOutput("viol_change_18", width = 25),
                             tags$head(tags$style("#viol_change_18{color: #000000;
                                                                  font-size: 16px;
                                                                  text-alight: left;
                             }"))
                      ), #column
                     
                      # change from 2019
                      column(width = 1, 
                             valueBoxOutput("viol_change_19", width = 25),
                             tags$head(tags$style("#viol_change_19{color: #000000;
                                                                  font-size: 16px;
                                                                  text-alight: left;
                             }"))
                      ), #column
                     
                      # sentence about change
                      column(width = 2, 
                             textOutput("viol_sentence_change"),
                             tags$head(tags$style("#viol_sentence_change{color: #000000;
                                                                         font-size: 16px;
                                                                         text-align: left;
                                                                         vertical-align: middle;}"))
                      ), #column
                      column(width = 1)
             ), #fluidRow
             
             br(),
             br()
             
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
             
             sidebarLayout(
               
               # side bar to select data
               sidebarPanel(
                 
                 # choose admissions or population
                 selectInput("adm_or_pop_map", 
                             label = "Admissions or Population",
                             choices = unique(mclc_change$adm_or_pop)),
                 
                 # select data
                 selectInput("data_map", 
                             label = "Select Data",
                             choices = unique(mclc_change$metric)),
                 
                 # select year
                 selectInput("year_map", 
                             label = "Select Year",
                             choices = unique(mclc_change$year)),
                 
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 
                 textOutput("map_title"),
                 tags$head(tags$style("#map_title{color: #000000;
                                                      font-size: 22px;
                                                      font-style: bold;
                                                      text-align: left;
                                                    }")), 
                 
                 leafletOutput(outputId = "map", height = 600),
                 tags$style(HTML(".leaflet-container { background: #FFFFFF;}"))
               )
             )
             
    ) #tabPanel
    
  ) #navbarPage
) #fluidPage
