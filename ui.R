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

                navbarPage(tags$style(type = "text/css", ".container-fluid {padding-left:0px; padding-right:0px;}"),
                           tags$style(type = "text/css", ".navbar {margin-bottom: .5px;}"),
                           tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}"),

                           title = "",

                           ##############################################################################################################################

                           tabPanel("",icon = icon("home", lib = "glyphicon"),value = "home"),

                           ##############################################################################################################################

                           tabPanel("National Report"),

                           ##############################################################################################################################

                           tabPanel("Map Explorer",

                                    # div(id = "app-title", titlePanel("Map Explorer")),

                                    div(id = "header",

                                        #######
                                        # Dropdown and download buttons
                                        #######

                                        fluidRow(
                                          column(width = 1),
                                          column(width = 2,
                                                 labeled_input('data-map-btn', "",
                                                               selectizeInput('data_map', div(style = "font-weight: bold", "Select Data"),
                                                                              choices = c("Total", "New Offense", "Supervision Violation", "Probation Violation", "Parole Violation", "Technical Violation"),
                                                                              multiple = FALSE))
                                          ),
                                          column(width = 2,
                                                 labeled_input('adm-pop-map-btn', "",
                                                               selectizeInput('adm_or_pop_map', div(style = "font-weight: bold", "Select Type"),
                                                                              choices = c("Admissions", "Population"),
                                                                              multiple = FALSE))
                                          ),
                                          column(width = 2,
                                                 labeled_input('year-map-btn', "",
                                                               selectizeInput('year_map', div(style = "font-weight: bold", "Select Years"),
                                                                              choices = c("2018 - 2019", "2019 - 2020"),
                                                                              multiple = FALSE))
                                          ),
                                          column(width = 2,
                                                 tags$style(type="text/css", "#save_map {background-color:#004270; color: #fff;}"),
                                                 labeled_input('save-map-btn', "",
                                                               downloadButton(outputId = 'save_map', label = "Download Map", class = "download_this"))
                                          ),
                                          column(width = 2,
                                                 tags$style(type="text/css", "#save_map_data {background-color:#004270; color: #fff;}"),
                                                 labeled_input('save-map-data-btn', "",
                                                               downloadButton(outputId = 'save_map_data', label = "Download Data", class = "download_this"))
                                          ),
                                          column(width = 1)
                                        ) # end fluidRow
                                    ), # end div header
                                    br(),

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

                           ), # end tabPanel

                           ##############################################################################################################################

                           tabPanel("State Reports",

                                    #######
                                    # Dropdown and download buttons
                                    #######

                                    div(id = "state-header",

                                        fluidRow(
                                          column(width = 4),
                                          column(width = 2,
                                                 labeled_input('state-btn', "",
                                                               selectizeInput('state_report', div(style = "font-weight: bold", "Select State"),
                                                                              #choices = unique(adm_pop_long$state),
                                                                              choices = c("Alabama", "Alaska"),
                                                                              multiple = FALSE))
                                          ),
                                          column(width = 2,
                                                 labeled_input('adm-pop-btn', "",
                                                               selectizeInput('adm_pop_report', div(style = "font-weight: bold", "Select Type"),
                                                                              choices = c("Admissions", "Population"),
                                                                              multiple = FALSE))
                                          ),
                                          column(width = 4)
                                        ) # end fluidRow
                                    ), # end div header

                                    br(),

                                    #######
                                    # Value boxes
                                    #######

                                    fluidRow(column(width = 2),
                                             column(width = 8, align = "left", div(id = "selected-state", textOutput("selected_state"))),
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

                                    fluidRow(column(width = 12,

                                                    tabsetPanel(

                                                      tabPanel(value="1","Overview",

                                                               br(),

                                                               fluidRow(column(width = 6, highchartOutput("state_area_chart", height = 400, width = 600)),
                                                                        column(width = 6, highchartOutput("state_bar_chart", height = 400, width = 600))),

                                                               br(),

                                                               fluidRow(column(width = 12, align = "center", reactableOutput("state_table"))),

                                                      ), # end tabPanel

                                                      tabPanel(value="2","Parole",

                                                              "Text"

                                                      ), # end tabPanel

                                                      tabPanel(value="3","Probation",

                                                               "Text"

                                                      ) # end tabPanel

                                                    ) # end tabsetPanel
                                             ) # end column
                                    ) # end fluidRow



                           ), # end tabPanel

                           ##############################################################################################################################

                           tabPanel("Download"),

                           ##############################################################################################################################

                           tabPanel("Methodology"),

                           ##############################################################################################################################

                           tabPanel("Videos"),

                           ##############################################################################################################################

                           tabPanel("Project Credits"))
)


# Useful links
# DIRECTING TO WEB PAGE - https://stackoverflow.com/questions/43244468/how-to-direct-to-another-web-page-after-clicking-tabpanel-in-shiny-app
# SEARCH BAR - https://stackoverflow.com/questions/55848517/how-do-i-add-search-widget-to-my-shiny-app
