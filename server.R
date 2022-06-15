#######################################
# Project: MCLCShiny
# File: server.R
# Authors: Mari Roberts
# Date last updated: June 10, 2022
# Description:
#    Server for shiny app
#######################################

server <- function(input, output, session) {

  ##############################################################################################################################
  # MAP EXPLORER
  ##############################################################################################################################

  #######
  # Hex map title
  #######

  # title of map based on user input
  output$selected_map <- renderText({paste("Change in ", input$data_map, " ", input$adm_or_pop_map, "from ", input$year_map)})

  #######
  # Hex map data
  #######

  # filter data depending on user input
  # map data
  df_map <- reactive({
    mclc_explorer %>%
      filter(adm_or_pop == input$adm_or_pop_map,
             metric     == input$data_map,
             year       == input$year_map)
  })
  # filter data depending on user input
  # table under map data
  df_map_table <- reactive({
    filter_by <- paste0(input$data_map, " ", input$adm_or_pop_map)
    mclc_explorer_table %>%
      filter(data == filter_by) %>%
      arrange(state) %>%
      rename(State = state)
  })
  # df_map_title <- reactive({
  #   title <- paste("Change in ", input$data_map, " ", input$adm_or_pop_map, "from ", input$year_map)
  # })

  #######
  # Hex map
  #######

  # create foundational hex map and store it as a reactive expression
  foundational_map <- reactive({

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$numericSymbolMagnitude <- 0
    hcoptslang$numericSymbols <-c( "%")
    options(highcharter.lang = hcoptslang)

    # get minimum and maximum value
    min_map <- round(min(df_map()$change, na.rm = TRUE), 0)
    max_map <- round(max(df_map()$change, na.rm = TRUE), 0)

    # create tooltip
    df_plot <- df_map() %>%
      mutate(tooltip = paste0("<b>", state, "</b><br>","Change from ", year, "<br>",change, "%<br>"))

    highchart() %>%

      hc_add_series_map(
        map = hex_gj,
        df = df_plot,
        joinBy = "state_abb",
        value = "change",
        dataLabels = list(enabled = TRUE, format = "{point.state_abb}",
                          style = list(fontSize = "11px", fontWeight = "regular", textOutline = 0)),
        nullColor = "#e8e8e8") %>%

      hc_colorAxis(min = min_map,
                   max = max_map,
                   stops = color_stops(7, c("#af4d03", orange, lightorange, "#FFFFFF", lightblue, regblue, darkblue))) %>%

      hc_add_dependency(name = "plugins/series-label.js") %>%
      hc_add_dependency(name = "plugins/accessibility.js") %>%
      hc_add_dependency(name = "plugins/exporting.js") %>%
      hc_add_dependency(name = "plugins/export-data.js") %>%
      hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
      hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%

      hc_add_theme(hc_theme_jc) %>%

      hc_plotOptions(series = list(animation = FALSE)) %>%

      hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical", valueDecimals = 0, valueSuffix = "%") %>%
      hc_xAxis(title = "", labels = list(y = 25)) %>%
      hc_yAxis(title = "", labels = list(format = "{value:,.0f}"))
    # hc_title(df_map_title())
  })

  # output hex map
  output$hex_map <- renderHighchart({
    foundational_map()
  })

  # store the current user-created version of the  map for download in a reactive expression
  final_map <- reactive({
    foundational_map()
  })

  #######
  # Table under hex map
  #######

  # title of table under map based on user input
  output$selected_map_table <- renderText({paste(input$data_map, " ", input$adm_or_pop_map)})

  # # table under hex map
  # output$table_map = DT::renderDataTable({
  #   # https://stackoverflow.com/questions/64097670/jquery-datatable-heading-and-search-on-the-same-line
  #   datatable(df_map_table(),
  #             class = list(stripe = FALSE),
  #             options = list(dom = 'ft',
  #                            pageLength = 50,
  #                            columnDefs = list(list(visible=FALSE, targets=c(1)))),
  #             rownames = FALSE) %>%
  #     formatPercentage(c("2018 - 2019", "2019 - 2020"), 2) %>%
  #     formatCurrency(c("2018", "2019", "2020"), currency = "", interval = 3, mark = ",")
  # })
  output$table_map <-renderFormattable(
    formattable(df_map_table(),
                align =c("l","l","l","l","l","l"),
                list(State = formatter("span", style = x ~ formattable::style("font-weight" = "bold")),
                     data = FALSE,
                     `2018` = formatter("span", x ~ comma(x, digits = 0)),
                     `2019` = formatter("span", x ~ comma(x, digits = 0)),
                     `2020` = formatter("span", x ~ comma(x, digits = 0)),
                     `2018 - 2019` = percent,
                     `2019 - 2020` = percent))
  )


  ##############################################################################################################################
  # State Reports
  ##############################################################################################################################



  ##############################################################################################################################
  # Download
  ##############################################################################################################################



  ##############################################################################################################################
  #Methodology
  ##############################################################################################################################



  ##############################################################################################################################
  # Videos
  ##############################################################################################################################



  ##############################################################################################################################
  #Project Credits"
  ##############################################################################################################################



}
