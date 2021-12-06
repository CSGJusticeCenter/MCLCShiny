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

  # update theme https://medium.com/analytics-vidhya/building-custom-r-shiny-ui-66d446ef4dad
  theme = shinytheme("cosmo"),
  
  navbarPage(
    
    #_________________________________________________________________________________________
    # 1) About
    #_________________________________________________________________________________________

    "More Community, Less Confinement",
    tabPanel("About",

             fluidRow(# title and text of national page
                      column(width = 2),
                      column(width = 8,
                             br(), br(),
                             div(" More Community, Less Confinement", style = "font-size:48px;
                                                                              font-style: bold;
                                                                              text-align: left;
                                                                              color: #000000;"), br(), 
                             div("A State-by-State Analysis on How Supervision Violations Impacted Prison Populations During the Pandemic",
                                 style = "font-size:32px;
                                          text-align: left;
                                          color: #000000;")
                      ), #column
                      column(width = 2)
             ), #fluidRow
             br()

    ),#tabPanel
    
    #_________________________________________________________________________________________
    # 2) Dashboard
    #_________________________________________________________________________________________
    
    tabPanel("State Reports",
             
             ########
             # Drop downs
             # eventually would like to have the option to hide the panel
             # https://stackoverflow.com/questions/42159804/how-to-collapse-sidebarpanel-in-shiny-app
             ########
             
             fluidRow(column(width = 1),
                      # select state
                      column(width = 5,
                             wellPanel(
                              # Select state
                                div(style = "font-size:16px;",
                                    selectInput(inputId = "state", label = div(style = "font-size:16px;
                                                                                        color: #000000;", 
                                                "Select State"), 
                                                choices = unique(adm_pop_long$states))),
                              style = "background: #E7E7E7"
                              ) #wellPanel
                      ),#column
                      
                      # select population or admissions
                      column(width = 5, 
                             wellPanel(
                             # Select adm or pop for plot
                               div(style = "font-size:16px;",
                                   selectInput(inputId = "adm_or_pop", label = div(style = "font-size:16px;
                                                                                            color: #000000;", 
                                              "Admissions or Population"), 
                                               choices = unique(adm_pop_long$adm_or_pop))),                             
                               style = "background: #E7E7E7"
                             ) #wellPanel
                      ), #column
                      column(width = 1)
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
             br(),
             
             ########
             # 1st Headers
             ########
             
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
             
             fluidRow(column(width = 1),
                      column(width = 3, 
                             # bar chart 
                             div(plotlyOutput("barchart", height = "100%", width = "100%"), align = "center")),
                      column(width = 1, 
                             br(),br(),br(),br(),br(),
                             # donut chart 
                             div(girafeOutput("donutchart", height = "100%", width = "100%"), align = "left")),
                      column(width = 2),
                      column(width = 3, 
                             # barchart
                             div(plotlyOutput("barchart_supviols", height = "100%", width = "100%"), align = "center")
                             ),
                      column(width = 1,
                             br(),br(),br(),br(),br(),
                             # donut chart 
                             div(girafeOutput("donutchart_supviols", height = "100%", width = "100%"), align = "left")),
                      column(width = 1)
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
             br(),
             br(),
             
             ########
             # 2nd Headers
             ########
             
             fluidRow(
               column(width = 1),
               # Title for probation graph
               column(width = 4, 
                      textOutput("prob_title"),
                      tags$head(tags$style("#prob_title{color: #FFFFFF;
                                                        font-size: 20px;
                                                        font-style: bold;
                                                        text-align: center;
                                                        background-color:#000000;
                                                        line-height: 32px;
                                                        }"))),
               column(width = 2),
               # Title for parole graph
               column(width = 4, 
                      textOutput("parole_title"),
                      tags$head(tags$style("#parole_title{color: #FFFFFF;
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
             # Probation and parole plots
             ########
             
             fluidRow(column(width = 1),
                      column(width = 3, 
                             # bar chart 
                             div(plotlyOutput("barchart_prob", height = "100%", width = "100%"), align = "center")),
                      column(width = 1, 
                             br(),br(),br(),br(),br(),
                             div(girafeOutput("donutchart_prob", height = "100%", width = "100%"), align = "left")),
                      column(width = 2),
                      column(width = 3, 
                             div(plotlyOutput("barchart_parole", height = "100%", width = "100%"), align = "center")
                      ),
                      column(width = 1, 
                             br(),br(),br(),br(),br(),
                             div(girafeOutput("donutchart_parole", height = "100%", width = "100%"), align = "left")),
                      column(width = 1)
             ), #fluidRow
             
             # add spacing
             br(),
             br()
             
    ), #tabPanel
    
    #_________________________________________________________________________________________
    # 3) View Data
    #_________________________________________________________________________________________
    
    tabPanel("View Data",
             
             fluidRow(
               column(width = 1),
               column(width = 10,
                      
                      titlePanel(""),
                      
                      DT::dataTableOutput("dt"),
                      
                      downloadButton(outputId = "download_filtered",
                                     label = "Download Filtered Data")
                      
                      ),
               column(width = 1)
             )

    ), #tabPanel
    
    #_________________________________________________________________________________________
    # 4) Map - Counts
    #_________________________________________________________________________________________
    
    tabPanel("Map 2",
             
             fluidRow(column(width = 1),
                      # select state
                      column(width = 5,
                             wellPanel(
                               div(style = "font-size:16px;",
                                   selectInput(inputId = "data_map_counts", label = div(style = "font-size:16px;
                                                                                                 color: #000000;", 
                                                         "Select Data"), 
                                               choices = unique(mclc$metric))),
                               style = "background: #E7E7E7"
                             ) #wellPanel
                      ),#column
                      
                      # select population or admissions
                      column(width = 5, 
                             wellPanel(
                               div(style = "font-size:16px;",
                                   selectInput(inputId = "adm_or_pop_map_counts", label = div(style = "font-size:16px;
                                                                                                       color: #000000;", 
                                                         "Admissions or Population"), 
                                               choices = unique(mclc$adm_or_pop))),                             
                               style = "background: #E7E7E7"
                             ) #wellPanel
                      ), #column
                      column(width = 1)
             ), #fluidRow
             
             fluidRow(column(width = 1),
                      column(width = 5,
                             
                             sliderInput("year_map_counts", "Year",
                                         min = min(mclc$year), max = max(mclc$year),
                                         sep = "", value = min(mclc$year), step = 1),
                             
                      ),#column
                      column(width = 6)
             ), #fluidRow     
             
             fluidRow(column(width = 1),
                      column(width = 10,
                             
                             textOutput("map_title_counts"),
                             tags$head(tags$style("#map_title_counts{color: #000000;
                                                                     font-size: 22px;
                                                                     font-style: bold;
                                                                     text-align: left;
                                                    }")), 
                             br(),
                             br()
                      ),#column
                      column(width = 1)
             ), #fluidRow
             
             fluidRow(column(width = 1),
                      column(width = 10,
                             
                             leafletOutput(outputId = "map_counts", height = 600),
                             tags$style(HTML(".leaflet-container { background: #FFFFFF;}")),
                             tags$style(type = "text/css", "#map_counts {height: calc(100vh - 53px) !important;}"),
                             br(), br(), br()
                             
                      ),#column
                      column(width = 1)
             ) #fluidRow
             
    ), #tabPanel
    
    #_________________________________________________________________________________________
    # 5) Map - Changes
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
    ), #tabPanel
    
    tags$style(type = "text/css", ".container-fluid {padding-left:0px; padding-right:0px;}"),
    tags$style(type = "text/css", ".navbar {margin-bottom: .5px;}"),
    tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}")
    
  ) #navbarPage
) #fluidPage
