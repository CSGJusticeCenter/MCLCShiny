#######################################
# Project: MCLCShiny
# File: ui.R
# Authors: Mari Roberts
# Date: November 11, 2021
# Description: 
#    User interface
#######################################

source("data_libraries.R")
source("functions.R")

#################################################################
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
             
             ########
             # title and text of national page
             ########
             fluidRow(
               column(width = 2),
               column(width = 8,
                      br(), 
                      br(),
                      div(" More Community, Less Confinement", style = "font-size:48px;
                                                                              font-style: bold;
                                                                              text-align: left;
                                                                              color: #000000;"), 
                      br(), 
                      div("A State-by-State Analysis on How Supervision Violations Impacted Prison Populations During the Pandemic",
                          style = "font-size:32px;
                                          text-align: left;
                                          color: #000000;"),
                      br(),
                      tags$div("View the ",
                               tags$a(href="https://csgjusticecenter.org/publications/more-community-less-confinement/", "national report, "),
                               tags$a(href="https://csgjusticecenter.org/projects/course-corrections/cost-calculator/", "Cost Calculator, "),
                               tags$a(href="https://csgjusticecenter.org/publications/more-community-less-confinement/state-reports/", "50 state reports, "), "and our ",
                               tags$a(href="https://csgjusticecenter.org/publications/more-community-less-confinement/methodology/", "research methodology."),
                               style = "font-size:24px;
                                          text-align: left;
                                          color: #000000;"),
                      
                      
               ), #column
               column(width = 2)
             ), #fluidRow
             br(),
             br(),
             br(),
             
             ########
             # title of area charts
             ########
             fluidRow(
               column(width = 3),
               column(width = 2,
                      br(), 
                      br(),
                      div("Prison Admissions", style = "font-size:20px;
                                                        font-style: bold;
                                                        text-align: center;
                                                        color: #FFFFFF;
                                                        background-color:#000000;
                                                        line-height: 32px;"),
                      br(), 
                      br()
               ),
               column(width = 2),
               column(width = 2,
                      br(), 
                      br(),
                      div("Prison Population", style = "font-size:20px;
                                                        font-style: bold;
                                                        text-align: center;
                                                        color: #FFFFFF;
                                                        background-color:#000000;
                                                        line-height: 32px;"),
                      br(), 
                      br()
               ),
               column(width = 3)
             ), #fluidRow 
             
             ########
             # area charts
             ########
             fluidRow(column(width = 2),
                      
                      # admissions plot
                      column(width = 4,
                             
                             div(plotOutput("areachart_adm"), align = "center")
                             
                      ),
                      
                      # population plot
                      column(width = 4,
                             
                             div(plotOutput("areachart_pop"), align = "center")
                             
                      ),
                      column(width = 2)
             ), #fluidRow
             br(),
             br(),
             
             ########
             # Compare BJS
             ########
             fluidRow(
               column(width = 2),
               column(width = 8,
                      br(), 
                      br(),
                      br(), 
                      div("Despite the reductions seen during the pandemic, supervision violations remain a large portion of prison admissions and account for a substantial share of prison populations.",
                          style = "font-size:28px;
                                  text-align: left;
                                  color: #000000;"),
                      br(),
                      br()
               ), #column
               column(width = 2)
             ), #fluidRow
             
             ########
             # Compare BJS
             ########
             fluidRow(
               column(width = 2),
               column(width = 8,
                      br(), 
                      br(),
                      br(), 
                      div("The Bureau of Justice Statistics provides a complementary/different picture of prison admissions and population in the US.",
                          style = "font-size:28px;
                                  text-align: left;
                                  color: #000000;"),
                      br(),
                      br()
               ), #column
               column(width = 2)
             ) #fluidRow
             
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
                      ), #column
                      
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
             
             fluidRow(column(width = 1),
                      column(width = 10,
                             align = "center",
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
             br(),
             
             ########
             # Plots 
             ########
             
             fluidRow(column(width = 1),
                      column(width = 3, 
                             # bar chart 
                             div(plotOutput("barchart"), align = "center")),
                      column(width = 1, 
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             # donut chart 
                             div(girafeOutput("donutchart", height = "100%", width = "100%"), align = "left")),
                      column(width = 2),
                      column(width = 3, 
                             # barchart
                             div(plotOutput("barchart_supviols"), align = "center")
                      ),
                      column(width = 1,
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
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
             br(),
             
             ########
             # Probation and parole plots
             ########
             
             fluidRow(column(width = 1),
                      column(width = 3, 
                             # bar chart 
                             div(plotOutput("barchart_prob"), align = "center")),
                      column(width = 1, 
                             br(),br(),br(),br(),br(),
                             div(girafeOutput("donutchart_prob", height = "100%", width = "100%"), align = "left")),
                      column(width = 2),
                      column(width = 3, 
                             div(plotOutput("barchart_parole"), align = "center")
                      ),
                      column(width = 1, 
                             br(),br(),br(),br(),br(),
                             div(girafeOutput("donutchart_parole", height = "100%", width = "100%"), align = "left")),
                      column(width = 1)
             ), #fluidRow
             
             # add spacing
             br(),
             
             ########
             # Probation and parole value boxes
             ########
             
             tags$style(".small-box.bg-green {background-color: #FFFFFF !important; color: #000000 !important; }"),
             tags$style(".small-box.bg-red   {background-color: #FFFFFF !important; color: #000000 !important; }"),
             tags$style(".small-box          {border: 3px; border-style: solid; border-color: #E7E7E7 !important; 
                                              border-radius: 5px; padding: 0.5em; }"),
             
             fluidRow(# change from 2018
               column(width = 1),
               
               column(width = 1, 
                      valueBoxOutput("prob_change_18", width = 25),
                      tags$head(tags$style("#prob_change_18{color: #000000;
                                                                    font-size: 16px;
                                                                    text-alight: left;
                             }"))
               ), #column
               
               # change from 2019
               column(width = 1, 
                      valueBoxOutput("prob_change_19", width = 25),
                      tags$head(tags$style("#prob_change_19{color: #000000;
                                              font-size: 16px;
                                              text-alight: left;
                             }"))
               ), #column
               
               # sentence about change
               column(width = 2, 
                      textOutput("prob_change_sentence"),
                      tags$head(tags$style("#prob_change_sentence{color: #000000;
                                                    font-size: 16px;
                                                    text-align: left;
                                                    vertical-align: middle;}"))
               ), #column
               
               column(width = 2),
               
               # change from 2018
               column(width = 1, 
                      valueBoxOutput("parole_change_18", width = 25),
                      tags$head(tags$style("#parole_change_18{color: #000000;
                                                              font-size: 16px;
                                                              text-alight: left;
                             }"))
               ), #column
               
               # change from 2019
               column(width = 1, 
                      valueBoxOutput("parole_change_19", width = 25),
                      tags$head(tags$style("#parole_change_19{color: #000000;
                                                              font-size: 16px;
                                                              text-alight: left;
                             }"))
               ), #column
               
               # sentence about change
               column(width = 2, 
                      textOutput("parole_change_sentence"),
                      tags$head(tags$style("#parole_change_sentence{color: #000000;
                                                                   font-size: 16px;
                                                                   text-align: left;
                                                                   vertical-align: middle;}"))
               ), #column
               column(width = 1)
             ), #fluidRow
             br()
             
    ), #tabPanel
    
    #_________________________________________________________________________________________
    # 3) View Data
    #_________________________________________________________________________________________
    
    tabPanel("View Data",
             
             fluidRow(
               column(width = 1),
               column(width = 10,
                      br(), 
                      br(),
                      div("View Data", style = "font-size:32px;
                                                font-style: bold;
                                                text-align: left;
                                                color: #000000;"), 
                      br(), 
                      div("This dataset contains prison admissions and population data by state between 2018 and 2020.",
                          style = "font-size:18px;
                                  text-align: left;
                                  color: #000000;")
               ),
               column(width = 1)
             ),
             
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
    # Map - Counts
    #_________________________________________________________________________________________
    
    tabPanel("Map",
             
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
             
             fluidRow(column(width = 2),
                      column(width = 8,
                             
                             leafletOutput(outputId = "map_counts", height = 300),
                             tags$style(HTML(".leaflet-container { background: #FFFFFF;}")),
                             tags$style(type = "text/css", "#map_counts {height: calc(100vh - 53px) !important;}"),
                             br(), br(), br()
                             
                      ),#column
                      column(width = 2)
             ) #fluidRow
             
    ), #tabPanel
    
    #_________________________________________________________________________________________
    # Map - Changes
    #_________________________________________________________________________________________
    
    tabPanel("Map - Changes",
             
             fluidRow(column(width = 1),
                      # select state
                      column(width = 5,
                             wellPanel(
                               div(style = "font-size:16px;",
                                   selectInput(inputId = "data_map_change", label = div(style = "font-size:16px;
                                                                                                 color: #000000;", 
                                                                                        "Select Data"), 
                                               choices = unique(mclc_change$metric))),
                               style = "background: #E7E7E7"
                             ) #wellPanel
                      ),#column
                      
                      # select population or admissions
                      column(width = 5, 
                             wellPanel(
                               div(style = "font-size:16px;",
                                   selectInput(inputId = "adm_or_pop_map_change", label = div(style = "font-size:16px;
                                                                                                       color: #000000;", 
                                                                                              "Admissions or Population"), 
                                               choices = unique(mclc_change$adm_or_pop))),                             
                               style = "background: #E7E7E7"
                             ) #wellPanel
                      ), #column
                      column(width = 1)
             ), #fluidRow
             
             fluidRow(column(width = 1),
                      column(width = 5,
                             
                             sliderInput("year_map_change", "Year",
                                         min = min(mclc_change$year), max = max(mclc_change$year),
                                         sep = "", value = min(mclc_change$year), step = 1),
                             
                      ),#column
                      column(width = 6)
             ), #fluidRow     
             
             fluidRow(column(width = 1),
                      column(width = 10,
                             
                             textOutput("map_title_change"),
                             tags$head(tags$style("#map_title_change{color: #000000;
                                                                     font-size: 22px;
                                                                     font-style: bold;
                                                                     text-align: left;
                                                    }")), 
                             br(),
                             br()
                      ),#column
                      column(width = 1)
             ), #fluidRow
             
             fluidRow(column(width = 2),
                      column(width = 8,
                             
                             leafletOutput(outputId = "map_change", height = 300),
                             tags$style(HTML(".leaflet-container { background: #FFFFFF;}")),
                             tags$style(type = "text/css", "#map_change {height: calc(100vh - 53px) !important;}"),
                             br(), 
                             br(), 
                             br()
                             
                      ),#column
                      column(width = 2)
             ) #fluidRow
             
    ), #tabPanel
    
    tags$style(type = "text/css", ".container-fluid {padding-left:0px; padding-right:0px;}"),
    tags$style(type = "text/css", ".navbar {margin-bottom: .5px;}"),
    tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}")
    
  ) #navbarPage
) #fluidPage
