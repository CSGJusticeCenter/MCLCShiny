#######################################
# Project: MCLCShiny
# File: ui.R
# Authors: Mari Roberts
# Date: April 27, 2022
# Description:
#    User interface for shiny app
#######################################

source("data_libraries.R")
source("functions.R")

# builds theme object to be supplied to ui
my_theme <- bs_theme(
  bootswatch = "cosmo",
  base_font = font_google("Noto Sans")
) %>%
  bs_add_rules(sass::sass_file("styles.scss"))

# let thematic know to use the font from bs_lib
thematic_shiny(font = "auto")

##################
# User Interface
##################

ui <- fluidPage(
  theme = my_theme,

  #____________________________________________________________________________________________________________
  # MAP EXPLORER
  #____________________________________________________________________________________________________________

  div(id = "app-title",
      titlePanel("Map Explorer")
  ),
  br(),
  div(id = "header",
      #######
      # Dropdown menus
      #######
      # tags$style(type="text/css", "#data_map {background-color:#DEF0F6}"),
      # tags$style(type="text/css", "#adm_or_pop_map {background-color:#DEF0F6}"),
      # tags$style(type="text/css", "#year_map {background-color:#DEF0F6}"),
      labeled_input('data-map-btn', "Data",
                    selectizeInput('data_map', label = NULL,
                                   choices = c("Total", "New Offense", "Supervision Violation", "Probation Violation", "Parole Violation", "Technical Violation"),
                                   multiple = FALSE)),
      labeled_input('adm-pop-map-btn', "Admissions or Population",
                    selectizeInput('adm_or_pop_map', label = NULL,
                                   choices = c("Admissions", "Population"),
                                   multiple = FALSE)),
      labeled_input('year-map-btn', "Year",
                    selectizeInput('year_map', label = NULL,
                                   choices = c("2018 - 2019", "2019 - 2020"),
                                   multiple = FALSE)),

      #######
      # Download buttons
      #######
      tags$style(type="text/css", "#save_map {background-color:#355DA1}"),
      tags$style(type="text/css", "#save_map_data {background-color:#355DA1}"),
      labeled_input('save-map-btn', "Download Map",
                    downloadButton(outputId = 'save_map', label = "Download Map", class = "download_this")),
      labeled_input('save-map-data-btn', "Download Data",
                    downloadButton(outputId = 'save_map_data', label = "Download Data", class = "download_this"))
  ),
  br(),

  #######
  # Hex map
  #######

  # div(id = "selected-map",
  #     textOutput("selected_map")),
  highchartOutput("hex_map", height = 600, width = 1091),
  br(),

  #######
  # Hex map table
  #######

  div(id = "selected-map-table",
      textOutput("selected_map_table")),
  div(id = "table-map",
  # reactableOutput("table_map"),
  dataTableOutput("table_map"),
  ),

  br(),

  div(class = "small_text",
      icon("database"), "Source:",
      a(href = "https://csgjusticecenter.org/publications/more-community-less-confinement/", "More Community, Less Confinement (2021)"))

  #____________________________________________________________________________________________________________
  # STATE REPORTS
  #____________________________________________________________________________________________________________

  #____________________________________________________________________________________________________________
  # DOWNLOAD DATA
  #____________________________________________________________________________________________________________


)
