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

source("data_libraries.R")
source("functions.R")

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

####################################################################################################################################
# User Interface
####################################################################################################################################

ui <- fluidPage(
  theme = my_theme,
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
      labeled_input('data-map-btn', "Select Data",
                    selectizeInput('data_map', label = NULL,
                                   choices = c("Total", "New Offense", "Supervision Violation", "Probation Violation", "Parole Violation", "Technical Violation"),
                                   multiple = FALSE)),
      labeled_input('adm-pop-map-btn', "Select Admissions or Population",
                    selectizeInput('adm_or_pop_map', label = NULL,
                                   choices = c("Admissions", "Population"),
                                   multiple = FALSE)),
      labeled_input('year-map-btn', "Select Year",
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

  highchartOutput("hex_map", height = 600),
  br(),

  #######
  # Hex map table
  #######

  div(id = "table-map",
  reactableOutput("table_map"),
  ),

  br(),

  div(class = "small_text",
      icon("database"), "Source:",
      a(href = "https://csgjusticecenter.org/publications/more-community-less-confinement/", "More Community, Less Confinement (2021)"))
)

####################################################################################################################################
# Server
####################################################################################################################################

server <- function(input, output, session) {

  #######
  # Hex map title
  #######

  # # title of map based on user input
  # output$selected_map <- renderText({paste("Change in ", input$data_map, " ", input$adm_or_pop_map, " from ", input$year_map)})

  #######
  # Hex map data
  #######
  df_map <- reactive({
    mclc_explorer %>%
      filter(adm_or_pop == input$adm_or_pop_map,
             metric     == input$data_map,
             year       == input$year_map)
  })
  df_map_table <- reactive({
    filter_by <- paste0(input$data_map, " ", input$adm_or_pop_map)
    mclc_explorer_table %>%
      filter(data == filter_by) %>%
      arrange(state)
  })

  #######
  # Hex map
  #######
  output$hex_map <- renderHighchart({

    min_map <- round(min(df_map()$total, na.rm = TRUE), 0)
    max_map <- round(max(df_map()$total, na.rm = TRUE), 0)

    df_plot <- df_map() %>%
      mutate(
        tooltip = paste0(
          "<b>", state, "</b><br>",
          "Change from ", year, "<br>",
          total, "%<br>"
        )
      )

    highchart() %>%
      hc_add_series_map(
        map = hex_gj,
        df = df_plot,
        joinBy = "state_abb",
        value = "total",
        dataLabels = list(enabled = TRUE, format = "{point.state_abb}",
                          style = list(fontSize = "11px", fontWeight = "regular", textOutline = 0)),
        nullColor = "#e8e8e8"
      ) %>%
      hc_colorAxis(min = min_map,
                   max = max_map,
                   stops = color_stops(6, c("#af4d03", orange, lightorange, lightblue, regblue, darkblue))) %>%
      hc_setup() %>%
      hc_title(
        text = "This is a title with <i>margin</i> and <b>Strong or bold text</b>",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      )

    })

  #######
  # Table under map
  #######

  output$table_map <- renderReactable(
    reactable(df_map_table(),
              searchable = TRUE,
              defaultPageSize = 50,
              theme = reactableTheme(
                # Vertically center cells
                cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE),
                align = "center"),
              compact = TRUE,
              fullWidth = FALSE,
              columns = list(
                state         = colDef(name = "State",
                                       align = "left",
                                       minWidth = 150,
                                       style = function(value){list(fontWeight = "bold")}),
                data          = colDef(show = FALSE),
                `2018`        = colDef(minWidth = 100),
                `2019`        = colDef(minWidth = 100),
                `2020`        = colDef(minWidth = 100),
                `2018 - 2019` = colDef(minWidth = 125,
                                       name = "Change from\n2018-2019",
                                       format = colFormat(percent = TRUE, digits = 1)),
                `2019 - 2020` = colDef(minWidth = 125,
                                       name = "Change from\n2019-2020",
                                       format = colFormat(percent = TRUE, digits = 1)))
    )
  )

}

######################
# Run the application
######################
shinyApp(ui = ui, server = server)
reactableOutput("table_map_counts")

