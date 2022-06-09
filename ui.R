#######################################
# Project: MCLCShiny
# File: ui.R
# Authors: Mari Roberts
# Date last updated: June 7, 2022
# Description:
#    User interface for shiny app
#######################################

setwd(L_PATH)
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
                                                 #selectizeInput('data_map', label = "Select Data", choices = c("Total", "New Offense", "Supervision Violation", "Probation Violation", "Parole Violation", "Technical Violation"), multiple = FALSE)
                                                 labeled_input('data-map-btn', "",
                                                               selectizeInput('data_map', div(style = "font-weight: bold", "Select Data"),
                                                                              choices = c("Total", "New Offense", "Supervision Violation", "Probation Violation", "Parole Violation", "Technical Violation"),
                                                                              multiple = FALSE))
                                                 ),
                                          column(width = 2,
                                                 #selectizeInput('adm_or_pop_map', label = "Select Type", choices = c("Admissions", "Population"), multiple = FALSE)
                                                 labeled_input('adm-pop-map-btn', "",
                                                               selectizeInput('adm_or_pop_map', div(style = "font-weight: bold", "Select Type"),
                                                                              choices = c("Admissions", "Population"),
                                                                              multiple = FALSE))
                                                 ),
                                          column(width = 2,
                                                 #selectizeInput('year_map', label = "Select Years", choices = c("2018 - 2019", "2019 - 2020"), multiple = FALSE)
                                                 labeled_input('year-map-btn', "",
                                                               selectizeInput('year_map', div(style = "font-weight: bold", "Select Years"),
                                                                              choices = c("2018 - 2019", "2019 - 2020"),
                                                                              multiple = FALSE))
                                                 ),
                                          column(width = 2,
                                                 #downloadButton(outputId = 'save_map', label = "Download Map", class = "download_this")
                                                 tags$style(type="text/css", "#save_map {background-color:#004270; color: #fff;}"),
                                                 labeled_input('save-map-btn', "",
                                                               downloadButton(outputId = 'save_map', label = "Download Map", class = "download_this"))
                                                 ),
                                          column(width = 2,
                                                 #downloadButton(outputId = 'save_map_data', label = "Download Data", class = "download_this")
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

                                    fluidRow(column(width = 1),
                                             column(width = 10, align = "left", div(id = "selected-map", textOutput("selected_map"))),
                                             column(width = 1)),

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
                                             column(width = 1)
                                             ),

                                    br(),

                                    tags$head(tags$style(HTML("thead{color: #004270; font-size: 16px}"))),
                                    fluidRow(column(width = 1),
                                             column(width = 10, align = "left", div(id = "table-map", formattableOutput("table_map"))),
                                             column(width = 1)),

                                    # fluidRow(column(width = 1),
                                    #          column(width = 10, align = "left", div(id = "table-map", dataTableOutput("table_map"))),
                                    #          column(width = 1)),

                                    br(),
                                    br()

                           ), # end tabPanel

##############################################################################################################################

                           tabPanel("State Reports"),

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
