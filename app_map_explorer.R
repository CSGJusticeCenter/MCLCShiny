library(shiny)
library(bslib)
library(ggplot2)
library(thematic)
library(showtext)
library(patchwork)
library(glue)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(readr)
library(here)

######################
# Custom functions
###################

# add a nicely styled label above selection box
labeled_input <- function(id, label, input){
  div(id = id,
      span(label, style = "font-size: small;"),
      input)
}

# builds theme object to be supplied to ui
my_theme <- bs_theme(
  bootswatch = "cosmo",
  base_font = font_google("Mukta")
) %>%
  bs_add_rules(sass::sass_file("styles.scss"))

# let thematic know to use the font from bs_lib
thematic_shiny(font = "auto")

######################
# User Interface
######################
ui <- fluidPage(
  theme = my_theme,
  div(
    id = "app-title",
    titlePanel("Map Explorer"),
  ),
  br(),
  div(id = "header",
      #######
      # Dropdown menus
      #######
      # tags$style(type="text/css", "#data_map {background-color:#DEF0F6}"),
      # tags$style(type="text/css", "#adm_pop_map {background-color:#DEF0F6}"),
      # tags$style(type="text/css", "#year_map {background-color:#DEF0F6}"),
      labeled_input('data-map-btn', "Select Data",
                    selectizeInput('data_map', label = NULL,
                                   choices = c("Total", "Supervision Violation", "Technical Violation", "New Offense", "Probation", "Parole"),
                                   multiple = FALSE)),
      labeled_input('adm-pop-map-btn', "Select Admissions or Population",
                    selectizeInput('adm_pop_map', label = NULL,
                                   choices = c("Admissions", "Population"),
                                   multiple = FALSE)),
      labeled_input('year-map-btn', "Select Year",
                    selectizeInput('year_map', label = NULL,
                                   choices = c("2018", "2019", "2020"),
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
  div(class = "small_text",
      icon("database"), "Source:",
      a(href = "https://csgjusticecenter.org/publications/more-community-less-confinement/", "More Community, Less Confinement (2021)"))
)


server <- function(input, output, session) {

}

# Run the application
shinyApp(ui = ui, server = server)
