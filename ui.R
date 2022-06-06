#######################################
# Project: MCLCShiny
# File: ui.R
# Authors: Mari Roberts
# Date: April 27, 2022
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

                           tabPanel("National Report"),

                           tabPanel("Map Explorer",

                                    # div(id = "app-title", titlePanel("Map Explorer")),

                                    div(id = "header",

                                        #######
                                        # Dropdown menus
                                        #######

                                        labeled_input('data-map-btn', "Select Data",
                                                      selectizeInput('data_map', label = NULL,
                                                                     choices = c("Total", "New Offense", "Supervision Violation", "Probation Violation", "Parole Violation", "Technical Violation"),
                                                                     multiple = FALSE)),
                                        labeled_input('adm-pop-map-btn', "Select Type",
                                                      selectizeInput('adm_or_pop_map', label = NULL,
                                                                     choices = c("Admissions", "Population"),
                                                                     multiple = FALSE)),
                                        labeled_input('year-map-btn', "Select Years",
                                                      selectizeInput('year_map', label = NULL,
                                                                     choices = c("2018 - 2019", "2019 - 2020"),
                                                                     multiple = FALSE)),

                                        #######
                                        # Download buttons
                                        #######

                                        tags$style(type="text/css", "#save_map {background-color:#004270; color: #fff;}"),
                                        tags$style(type="text/css", "#save_map_data {background-color:#004270; color: #fff;}"),
                                        labeled_input('save-map-btn', "",
                                                      downloadButton(outputId = 'save_map', label = "Download Map", class = "download_this")),
                                        labeled_input('save-map-data-btn', "",
                                                      downloadButton(outputId = 'save_map_data', label = "Download Data", class = "download_this"))),

                                    br()
                           ),

                           tabPanel("State Reports"),

                           tabPanel("Download"),

                           tabPanel("Methodology"),

                           tabPanel("Videos"),

                           tabPanel("Project Credits"))
)


