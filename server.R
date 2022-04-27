#######################################
# Project: MCLCShiny
# File: server.R
# Authors: Mari Roberts
# Date: April 27, 2022
# Description:
#    Server for shiny app
#######################################

server <- function(input, output, session) {

  #____________________________________________________________________________________________________________
  # MAP EXPLORER
  #____________________________________________________________________________________________________________

  #######
  # Hex map title
  #######

  # title of map based on user input
  output$selected_map <- renderText({paste("Change in ", input$data_map, " ", input$adm_or_pop_map, ", ", input$year_map)})

  #######
  # Hex map data
  #######

  # filter data depending on user input
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
      hc_setup()
  })

  #######
  # Table under map
  #######

  # title of map table based on user input
  output$selected_map_table <- renderText({paste(input$data_map, " ", input$adm_or_pop_map)})

  output$table_map <- renderReactable(
    reactable(df_map_table(),
              searchable = TRUE,
              defaultPageSize = 50,
              compact = TRUE,
              fullWidth = TRUE,
              bordered = FALSE,
              rowStyle = list(`border-top` = "thin dashed", borderColor = "#355DA1"),
              theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE),
                align = "center",
                headerStyle = list(color = "#355DA1",
                                   "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                                   "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                                   borderColor = "#FFFFFF")),
              columns = list(
                state         = colDef(name = "State",
                                       align = "left",
                                       minWidth = 205,
                                       style = function(value){list(fontWeight = "bold")}),
                data          = colDef(show = FALSE),
                `2018`        = colDef(minWidth = 155),
                `2019`        = colDef(minWidth = 155),
                `2020`        = colDef(minWidth = 155),
                `2018 - 2019` = colDef(minWidth = 180,
                                       name = "2018-2019",
                                       format = colFormat(percent = TRUE, digits = 1)),
                `2019 - 2020` = colDef(minWidth = 180,
                                       name = "2019-2020",
                                       format = colFormat(percent = TRUE, digits = 1)))
    )
  )

  #____________________________________________________________________________________________________________
  # STATE REPORTS
  #____________________________________________________________________________________________________________

  #____________________________________________________________________________________________________________
  # DOWNLOAD DATA
  #____________________________________________________________________________________________________________



}
