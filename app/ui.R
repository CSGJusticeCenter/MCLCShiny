#######################################
# Project: MCLCShiny
# File: ui.R
# Authors: Mari Roberts
# Date last updated: August 3, 2022
# Description:
#    User interface for shiny app
#######################################

source("library.R")
source("colors.R")
source("dataframes.R")
source("functions.R")

ui <- fluidPage(includeCSS("www/theme.css"),

                navbarPage(id = "navbarID",

                           # formats light blue header
                           tags$style(type = "text/css", ".container-fluid {padding-left:0px; padding-right:0px;}"),
                           tags$style(type = "text/css", ".navbar {margin-bottom: .5px;}"),
                           tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}"),

                           title = "",

                           ##############################################################################################################################

                           tabPanel("mapexplorer", id = "mapexplorer",

                                    # div(id = "app-title", titlePanel("Map Explorer")),

                                    div(id = "header",

                                        #######
                                        # Dropdown and download buttons
                                        #######

                                        fluidRow(column(width = 3),

                                                 column(width = 6,

                                                        fluidRow(# Select Data
                                                                 column(width = 3,
                                                                         labeled_input('data-map-btn', "",
                                                                               selectizeInput('data_map', div(style = "font-weight: bold", "Select Data"),
                                                                                              choices = c("Total", "New Offense", "Supervision Violation", "Probation Violation", "Parole Violation", "Technical Violation"),
                                                                                              multiple = FALSE))),
                                                                 # Select Adm or Pop
                                                                 column(width = 3,
                                                                         labeled_input('adm-pop-map-btn', "",
                                                                                       selectizeInput('adm_or_pop_map', div(style = "font-weight: bold", "Select Type"),
                                                                                                      choices = c("Admissions", "Population"),
                                                                                                      multiple = FALSE))),
                                                                 # Select Year Change
                                                                 column(width = 3,
                                                                         labeled_input('year-map-btn', "",
                                                                                       selectizeInput('year_map', div(style = "font-weight: bold", "Select Years"),
                                                                                                      choices = c("2018 - 2019", "2019 - 2020", "2020 - 2021", "All (2018 - 2021)"),
                                                                                                      multiple = FALSE))),

                                                                 # Download Map
                                                                 column(width = 3,
                                                                         labeled_input('save-map-btn', "",
                                                                                       downloadButton(outputId = 'save_map', "Download Map",
                                                                                                      #div(style = "font-weight: bold", "Download Data"), # this causes spacing issues within the button
                                                                                                      class = "download-map")))
                                                        ) # end fluidRow
                                                  ),
                                                  column(width = 3)
                                        ) # end fluidRow

                                    ), # end div header
                                    br(),

                                    div(id = "map-body",

                                        #######
                                        # Hex map
                                        #######

                                        fluidRow(column(width = 1),
                                                 column(width = 10, align = "center", highchartOutput("hex_map", height = 550, width = "100%")),
                                                 column(width = 1)),

                                        br(),
                                        br(),

                                        #######
                                        # Hex map table
                                        #######

                                        fluidRow(column(width = 1),
                                                 column(width = 10, align = "center", div(id = "selected-map-table", textOutput("selected_map_table"))),
                                                 column(width = 1)),

                                        br(),

                                        tags$head(tags$style(HTML("thead{color: #004270; font-size: 16px}"))),
                                        fluidRow(column(width = 1),
                                                 column(width = 10, align = "left",
                                                        div(id = "table-map",
                                                            # dataTableOutput("table_map")
                                                            reactableOutput("table_map")
                                                            )
                                                        ),
                                                 column(width = 1)),

                                        br(),
                                        br()

                                    ) # end div

                           ), # end tabPanel

                           ##############################################################################################################################

                           tabPanel("statereports", id = "statereports",

                                    #######
                                    # Dropdown and download buttons
                                    #######

                                    div(id = "state-header",
                                        fluidRow(column(width = 3),

                                                 column(width = 6,

                                                        fluidRow(column(width = 3),

                                                                 # Select State
                                                                 column(width = 3,
                                                                        labeled_input('state-btn', "",
                                                                                      selectizeInput('state_report', div(style = "font-weight: bold", "Select State"),
                                                                                                     choices = unique(adm_pop_long$state),
                                                                                                     multiple = FALSE))),
                                                                 # Select Adm or Pop
                                                                 column(width = 3,
                                                                        labeled_input('adm-pop-btn', "",
                                                                                      selectizeInput('adm_pop_report', div(style = "font-weight: bold", "Select Type"),
                                                                                                     choices = c("Admissions", "Population"),
                                                                                                     multiple = FALSE))),
                                                                 column(width = 3)
                                                                 )),

                                                 column(width = 3)

                                                 ) # fluidRow
                                    ), # end div header

                                    br(),

                                    div(id = "state-body",

                                        #######
                                        # Value boxes
                                        #######

                                        fluidRow(column(width = 1),
                                                 column(width = 10, div(id = "selected-state", textOutput("selected_state"))),
                                                 column(width = 1)),

                                        br(),

                                        tags$style(".small-box.bg-black  {background-color: #004270 !important; color: #FFFFFF !important; font-family: Graphik !important;}"),
                                        tags$style(".small-box           {border: 1px; border-style: solid; border-color: #FFFFFF !important; border-radius: 1px; padding: 0.75em; }"),

                                        fluidRow(column(width = 2),
                                                 column(width = 2,
                                                        valueBoxOutput("total_change", width = 100)),
                                                 column(width = 2,
                                                        valueBoxOutput("sup_change", width = 100)),
                                                 column(width = 2,
                                                        valueBoxOutput("tech_change", width = 100)),
                                                 column(width = 2,
                                                        valueBoxOutput("new_off_change", width = 100)),
                                                 column(width = 2)),

                                        br(),
                                        br(),

                                        #######
                                        # Panels for Overview, Parole, Probation, and Revocations
                                        #######

                                        fluidRow(column(width = 1),
                                                 column(width = 10,

                                                        tabsetPanel(

                                                          tabPanel(value="1","Overview",

                                                                   br(),

                                                                   fluidRow(column(width = 6, align = "center", highchartOutput("state_area_chart", height = 400, width = 390)),
                                                                            column(width = 6, align = "center", highchartOutput("state_bar_chart", height = 400, width =390))),

                                                                   br(),
                                                                   br(),
                                                                   br(),

                                                                   fluidRow(column(width = 12, align = "center", reactableOutput("state_table"))),

                                                                   br(),

                                                                   fluidRow(column(width = 12, align = "left", div(id = "selected-state-note-title", "State Notes"))),

                                                                   br(),

                                                                   fluidRow(column(width = 12, align = "left", div(id = "selected-state-note", textOutput("selected_state_note")))),

                                                                   br(),

                                                                   fluidRow(column(width = 12, align = "left", div(id = "consistent-state-note", state_note))),

                                                                   br(),
                                                                   br()

                                                          ), # end tabPanel

                                                          tabPanel(value="2","Parole",

                                                                   br(),

                                                                   fluidRow(column(width = 12, align = "center", highchartOutput("parole_bar_chart", height = 400, width = 390))),

                                                                   br(),
                                                                   br(),
                                                                   br(),

                                                                   fluidRow(column(width = 12, align = "center", reactableOutput("parole_table"))),

                                                                   br(),
                                                                   br()

                                                          ), # end tabPanel

                                                          tabPanel(value="3","Probation",

                                                                   br(),

                                                                   fluidRow(column(width = 12, align = "center", highchartOutput("probation_bar_chart", height = 400, width = 390))),

                                                                   br(),
                                                                   br(),
                                                                   br(),

                                                                   fluidRow(column(width = 12, align = "center", reactableOutput("probation_table"))),

                                                                   br(),
                                                                   br()

                                                          ), # end tabPanel

                                                          tabPanel(value="4","Revocations",

                                                                   br(),

                                                                   "Coming soon.",

                                                                   br(),
                                                                   br()

                                                          ) # end tabPanel

                                                        ) # end tabsetPanel
                                                 ), # end column
                                                 column(width = 1)
                                        ), # end fluidRow

                                        br()

                                    ) # end div

                           ), # end tabPanel

                           ##############################################################################################################################

                           tabPanel("downloaddata", id = "downloaddata",

                                    #######
                                    # Dropdowns
                                    #######

                                    div(id = "download-header",

                                        fluidRow(column(width = 3),

                                                 column(width = 6,

                                                        # https://stackoverflow.com/questions/67212995/r-shiny-pickerinput-chiocesopt

                                                        fluidRow(# Select State(s)
                                                                 column(width = 3,
                                                                        labeled_input('download-state-btn', "", #"Select State(s)",
                                                                                      pickerInput(inputId = 'download_state',
                                                                                                  width = "100%",
                                                                                                  choices = NULL,
                                                                                                  selected = NULL,
                                                                                                  multiple = TRUE,
                                                                                                  div(style = "font-weight: bold", "Select State(s)"),
                                                                                                  # choicesOpt = list(style = sprintf('background:%s;', 'green')), # choicesOpt is not working
                                                                                                  options = list(`actions-box` = TRUE,
                                                                                                                 style = "picker-style")))),
                                                                 # Select Metric(s)
                                                                 column(width = 3,
                                                                        labeled_input('download-metric-btn', "", #"Select Metric(s)",
                                                                                      pickerInput(inputId = 'download_metric',
                                                                                                  width = "100%",
                                                                                                  choices = NULL,
                                                                                                  selected = NULL,
                                                                                                  multiple = TRUE,
                                                                                                  div(style = "font-weight: bold", "Select Metric(s)"),
                                                                                                  options = list(`actions-box` = TRUE,
                                                                                                                 style = "picker-style")))),
                                                                 # Select Year(s)
                                                                 column(width = 3,
                                                                        labeled_input('download-year-btn', "", #"Select Year(s)",
                                                                                      pickerInput(inputId = 'download_year',
                                                                                                  width = "100%",
                                                                                                  choices = NULL,
                                                                                                  selected = NULL,
                                                                                                  multiple = TRUE,
                                                                                                  div(style = "font-weight: bold", "Select Year(s)"),

                                                                                                  options = list(`actions-box` = TRUE,
                                                                                                                 style = "picker-style")))),
                                                                 # Download Data
                                                                 column(width = 3,
                                                                        labeled_input('save-data-btn', "",
                                                                                      downloadButton(outputId = 'save_data', "Download Data",
                                                                                                     #div(style = "font-weight: bold", "Download Data"), # this causes spacing issues within the button
                                                                                                     class = "download-data")))
                                                        )),

                                                 column(width = 3)

                                        ) # fluidRow
                                    ), # end div header

                                    br(),

                                    #######
                                    # Download table
                                    #######

                                    div(id = "download-body",

                                        fluidRow(column(width = 2),
                                                 column(width = 8, div(id = "download-title", "Download Data")),
                                                 column(width = 2)),

                                        br(),

                                        fluidRow(column(width = 2),
                                                 column(width = 8, div(id = "download-data-title", "More Community, Less Confinement (2022)")),
                                                 column(width = 2)),

                                        br(),

                                        fluidRow(column(width = 2),
                                                 column(width = 8, div(id = "download-info", "To understand the impact of community supervision (i.e., probation, parole, post-release supervision) on state prison populations, The Council of State Governments (CSG) Justice Center surveyed corrections leaders in all 50 states. This project was supported by Arnold Ventures and produced in partnership with the Correctional Leaders Association (CLA). The resulting data span 4 years—from 2018 to 2021—and demonstrate how the number of people sent to prison for supervision violations changed.")),
                                                 column(width = 2)),
                                        br(),

                                        fluidRow(column(width = 2),
                                                 column(width = 8,
                                                        align = "center",
                                                        # div(id = "selected-download-table", DT::dataTableOutput("selected_download_table"))
                                                        div(id = "selected-download-table", reactableOutput("selected_download_table"))
                                                 ),
                                                 column(width = 2)),

                                        br(),
                                        br()

                                    ) # end div

                           ) # end tabPanel
                ))
