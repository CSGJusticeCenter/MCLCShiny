#######################################
# Project: MCLCShiny
# File: ui.R
# Authors: Mari Roberts
# Date last updated: June 7, 2022
# Description:
#    User interface for shiny app
#######################################

source("library.R")
source("dataframes.R")
source("functions.R")

ui <- fluidPage(includeCSS("www/theme.css"),

                navbarPage(id = "navbarID",

                           tags$style(type = "text/css", ".container-fluid {padding-left:0px; padding-right:0px;}"),
                           tags$style(type = "text/css", ".navbar {margin-bottom: .5px;}"),
                           tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}"),

                           title = "",

                           ##############################################################################################################################

                           tabPanel("Map Explorer", id = "mapexplorer",

                                    # div(id = "app-title", titlePanel("Map Explorer")),

                                    div(id = "header",

                                        #######
                                        # Dropdown and download buttons
                                        #######

                                        fluidRow(

                                          column(width = 3),
                                          column(width = 1,
                                                 labeled_input('data-map-btn', "",
                                                               selectizeInput('data_map', div(style = "font-weight: bold", "Select Data"),
                                                                              choices = c("Total", "New Offense", "Supervision Violation", "Probation Violation", "Parole Violation", "Technical Violation"),
                                                                              multiple = FALSE))
                                          ),
                                          column(width = 1, align = "left",
                                                 labeled_input('adm-pop-map-btn', "",
                                                               selectizeInput('adm_or_pop_map', div(style = "font-weight: bold", "Select Type"),
                                                                              choices = c("Admissions", "Population"),
                                                                              multiple = FALSE))
                                          ),
                                          column(width = 1,
                                                 labeled_input('year-map-btn', "",
                                                               selectizeInput('year_map', div(style = "font-weight: bold", "Select Years"),
                                                                              choices = c("2018 - 2019", "2019 - 2020"),
                                                                              multiple = FALSE))
                                          ),
                                          column(width = 1),
                                          column(width = 1,
                                                 tags$style(type="text/css", "#save_map {background-color:#004270; color: #fff;}"),
                                                 labeled_input('save-map-btn', "",
                                                               downloadButton(outputId = 'save_map', label = "Download Map", class = "download_this"))
                                          ),
                                          column(width = 1,
                                                 tags$style(type="text/css", "#save_map_data {background-color:#004270; color: #fff;}"),
                                                 labeled_input('save-map-data-btn', "",
                                                               downloadButton(outputId = 'save_map_data', label = "Download Data", class = "download_this"))
                                          ),
                                          column(width = 3)

                                        ) # end fluidRow

                                    ), # end div header
                                    br(),

                                    div(id = "mapbody",

                                        #######
                                        # Hex map
                                        #######

                                        # fluidRow(column(width = 1),
                                        #          column(width = 10, align = "left", div(id = "selected-map", textOutput("selected_map"))),
                                        #          column(width = 1)),

                                        fluidRow(column(width = 1),
                                                 column(width = 10, align = "center", highchartOutput("hex_map", height = 550, width = 1000)),
                                                 column(width = 1)),

                                        br(),
                                        br(),

                                        #######
                                        # Hex map table
                                        #######

                                        fluidRow(column(width = 1),
                                                 column(width = 10, align = "left", div(id = "selected-map-table", textOutput("selected_map_table"))),
                                                 column(width = 1)),

                                        br(),

                                        tags$head(tags$style(HTML("thead{color: #004270; font-size: 16px}"))),
                                        fluidRow(column(width = 1),
                                                 column(width = 10, align = "left",
                                                        div(id = "table-map", dataTableOutput("table_map"))),
                                                 column(width = 1)),

                                        br(),
                                        br()

                                    ), # end div

                           ), # end tabPanel

                           ##############################################################################################################################

                           tabPanel("State Reports", id = "statereports",

                                    #######
                                    # Dropdown and download buttons
                                    #######

                                    div(id = "state-header",

                                        fluidRow(
                                          column(width = 5),
                                          column(width = 1,
                                                 labeled_input('state-btn', "",
                                                               selectizeInput('state_report', div(style = "font-weight: bold", "Select State"),
                                                                              choices = unique(adm_pop_long$state),
                                                                              multiple = FALSE))
                                          ),
                                          column(width = 1,
                                                 labeled_input('adm-pop-btn', "",
                                                               selectizeInput('adm_pop_report', div(style = "font-weight: bold", "Select Type"),
                                                                              choices = c("Admissions", "Population"),
                                                                              multiple = FALSE))
                                          ),
                                          column(width = 5)
                                        ) # end fluidRow
                                    ), # end div header

                                    br(),

                                    div(id = "statebody",

                                        #######
                                        # Value boxes
                                        #######

                                        fluidRow(column(width = 2),
                                                 column(width = 8, align = "center", div(id = "selected-state", textOutput("selected_state"))),
                                                 column(width = 2)),

                                        br(),

                                        tags$style(".small-box.bg-black  {background-color: #004270 !important; color: #FFFFFF !important; }"),
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
                                        # Panels for Overview, Parole, and Probation
                                        #######

                                        fluidRow(column(width = 2),
                                                 column(width = 8,

                                                        tabsetPanel(

                                                          tabPanel(value="1","Overview",

                                                                   br(),

                                                                   fluidRow(column(width = 6, highchartOutput("state_area_chart", height = 400, width = 390)),
                                                                            column(width = 6, highchartOutput("state_bar_chart", height = 400, width =390))
                                                                   ),

                                                                   br(),
                                                                   br(),
                                                                   br(),

                                                                   fluidRow(column(width = 12, align = "center", reactableOutput("state_table"))
                                                                   )

                                                          ), # end tabPanel

                                                          tabPanel(value="2","Parole",

                                                                   "Coming soon."

                                                          ), # end tabPanel

                                                          tabPanel(value="3","Probation",

                                                                   "Coming soon."

                                                          ) # end tabPanel

                                                        ) # end tabsetPanel
                                                 ), # end column
                                                 column(width = 2)
                                        ), # end fluidRow

                                        br(),
                                        br(),

                                        fluidRow(column(width = 2),
                                                 column(width = 8, align = "left", div(id = "selected-state-note-title", "State Notes")),
                                                 column(width = 2)),

                                        br(),

                                        fluidRow(column(width = 2),
                                                 column(width = 8, align = "left", div(id = "selected-state-note", textOutput("selected_state_note"))),
                                                 column(width = 2)),

                                        br(),

                                        fluidRow(column(width = 2),
                                                 column(width = 8, align = "left", div(id = "consistent-state-note", state_note)),
                                                 column(width = 2)),

                                    ), # end div

                                    # #######
                                    # # Value boxes
                                    # #######
                                    #
                                    # fluidRow(column(width = 2),
                                    #          column(width = 8, align = "center", div(id = "selected-state", textOutput("selected_state"))),
                                    #          column(width = 2)),
                                    #
                                    # br(),
                                    #
                                    # tags$style(".small-box.bg-black  {background-color: #004270 !important; color: #FFFFFF !important; }"),
                                    # tags$style(".small-box           {border: 1px; border-style: solid; border-color: #FFFFFF !important; border-radius: 1px; padding: 0.75em; }"),
                                    #
                                    # fluidRow(column(width = 2),
                                    #          column(width = 2,
                                    #                 valueBoxOutput("total_change", width = 100)),
                                    #          column(width = 2,
                                    #                 valueBoxOutput("sup_change", width = 100)),
                                    #          column(width = 2,
                                    #                 valueBoxOutput("tech_change", width = 100)),
                                    #          column(width = 2,
                                    #                 valueBoxOutput("new_off_change", width = 100)),
                                    #          column(width = 2)),
                                    #
                                    # br(),
                                    # br(),
                                    #
                                    # #######
                                    # # Panels for Overview, Parole, and Probation
                                    # #######
                                    #
                                    # fluidRow(column(width = 2),
                                    #          column(width = 8,
                                    #
                                    #                 tabsetPanel(
                                    #
                                    #                   tabPanel(value="1","Overview",
                                    #
                                    #                            br(),
                                    #
                                    #                            fluidRow(column(width = 5, highchartOutput("state_area_chart", height = 400, width = 400)),
                                    #                                     column(width = 5, highchartOutput("state_bar_chart", height = 400, width = 400))
                                    #                                     ),
                                    #
                                    #                            br(),
                                    #                            br(),
                                    #                            br(),
                                    #
                                    #                            fluidRow(column(width = 10, align = "center", reactableOutput("state_table"))
                                    #                                     )
                                    #
                                    #                   ), # end tabPanel
                                    #
                                    #                   tabPanel(value="2","Parole",
                                    #
                                    #                            "Coming soon."
                                    #
                                    #                   ), # end tabPanel
                                    #
                                    #                   tabPanel(value="3","Probation",
                                    #
                                    #                            "Coming soon."
                                    #
                                    #                   ) # end tabPanel
                                    #
                                    #                 ) # end tabsetPanel
                                    #          ), # end column
                                    #          column(width = 2)
                                    # ), # end fluidRow
                                    #
                                    # br(),
                                    # br(),
                                    #
                                    # fluidRow(column(width = 2),
                                    #          column(width = 8, align = "left", div(id = "selected-state-note-title", "State Notes")),
                                    #          column(width = 2)),
                                    #
                                    # br(),
                                    #
                                    # fluidRow(column(width = 2),
                                    #          column(width = 8, align = "left", div(id = "selected-state-note", textOutput("selected_state_note"))),
                                    #          column(width = 2)),
                                    #
                                    # br(),
                                    #
                                    # fluidRow(column(width = 2),
                                    #          column(width = 8, align = "left", div(id = "consistent-state-note", state_note)),
                                    #          column(width = 2)),

                           ), # end tabPanel

                           ##############################################################################################################################

                           tabPanel("Download", id = "downloaddata"))
)


# Useful links
# DIRECTING TO WEB PAGE - https://stackoverflow.com/questions/43244468/how-to-direct-to-another-web-page-after-clicking-tabpanel-in-shiny-app
# SEARCH BAR - https://stackoverflow.com/questions/55848517/how-do-i-add-search-widget-to-my-shiny-app
