#######################################
# Project: MCLCShiny
# File: server.R
# Authors: Mari Roberts
# Date last updated: November 30, 2022
# Description:
#    Server for shiny app
#######################################

server <- function(input, output, session) {

  # Change URL depending on tab selection in navbar
  observeEvent(input$navbarID, {

    newURL <- paste0(
      session$clientData$url_protocol,
      "//",
      session$clientData$url_hostname,
      ":",
      session$clientData$url_port,
      session$clientData$url_pathname,
      "#",
      input$navbarID
    )
    updateQueryString(newURL, mode = "replace", session)
  })

  observe({
    currentTab <- sub("#", "", session$clientData$url_hash)
    if(!is.null(currentTab)){
      updateTabItems(session, "navbarID", selected = currentTab)
    }
  })

  ##############################################################################################################################
  # MAP EXPLORER
  ##############################################################################################################################

  #######
  # Hex map title
  #######

  # Title of map based on user input
  output$selected_map <- renderText({paste("Change in ", input$data_map, " ", input$adm_or_pop_map, "from ", input$year_map)})

  #######
  # Hex map data
  #######

  # Filter data depending on user input for map explorer
  # Map data
  df_map <- reactive({
    mclc_explorer %>%
      filter(adm_or_pop == input$adm_or_pop_map,
             metric     == input$data_map,
             year       == input$year_map)
  })

  # Data for table under map
  df_map_table <- reactive({
    filter_by <- paste0(input$data_map, " ", input$adm_or_pop_map)
    select_column = input$year_map
    df <- mclc_explorer_table[, c('state', 'data', '2018', '2019', '2020', '2021', select_column, 'total_new')]
    df <- df %>%
      filter(data == filter_by) %>%
      arrange(state) %>%
      rename(State = state,
             change = 7)
  })

  # Dynamically change name of map
  map_filename <- reactive({
    temp <- mclc_explorer %>%
      filter(adm_or_pop == input$adm_or_pop_map,
             metric     == input$data_map,
             year       == input$year_map) %>%
      select(data, year) %>% distinct()
    temp$year <- gsub(" - ", " ", temp$year)
    temp <- paste(temp$data, temp$year, sep = '_')
    temp <- gsub(" ", "_", temp)
  })


  #######
  # Hex map
  #######

  # Create foundational hex map and store it as a reactive expression
  # This is necessary to download the map
  foundational_map <- reactive({

    # Get minimum and maximum value
    min_map <- min(df_map()$change, na.rm = TRUE)
    max_map <- max(df_map()$change, na.rm = TRUE)

    # Get absolute value for comparison
    min_map_abs <- abs(min_map)
    max_map_abs <- abs(max_map)

    # Get neg or pos sign for min and max
    min_map_type <- ifelse(min_map >= 0, "positive", "negative")
    max_map_type <- ifelse(max_map >= 0, "positive", "negative")

    # Generate tile map
    # Has diverging scales when there are neg and pos values which centers the color gradient at zero
    # Has a gradient scale when both the min and max are both negative or both positive

    # Determine the new min and max so that zero is centered
    # For example, If the highest positive value is 20 than the negative value is -20
    if (min_map_type != max_map_type) {

      NEW_MAX <- case_when(
        max_map_abs > min_map_abs ~ max_map_abs,
        max_map_abs < min_map_abs ~ min_map_abs,
        max_map_abs == min_map_abs ~ max_map_abs)
      NEW_MIN <- case_when(
        min_map_abs > max_map_abs ~ min_map_abs,
        min_map_abs < max_map_abs ~ max_map_abs,
        min_map_abs == max_map_abs ~ min_map_abs)
      NEW_MAX <- ifelse(max_map_type == "negative", -abs(NEW_MAX), abs(NEW_MAX))
      NEW_MIN <- ifelse(min_map_type == "negative", -abs(NEW_MIN), abs(NEW_MIN))

      highchart() %>%

        hc_add_series_map(
          map = hex_gj,
          df = df_map(),
          joinBy = "state_abb",
          value = "change",
          dataLabels = list(enabled = TRUE, format = "{point.datalabel}",
                            style = list(fontSize = "14px",
                                         fontWeight = "regular",
                                         fontFamily = "Graphik",
                                         textOutline = 0)),
          nullColor = "#e8e8e8") %>%

        hc_colorAxis(min = NEW_MIN,
                     max = NEW_MAX,
                     stops = color_stops(7, c(darkorange, orange, lightorange, white, lightblue, regblue, darkblue)),
                     labels = list(format = "{value}%",
                                   style = list(fontSize = "14px"))
        ) %>%

        hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
                  #padding = 10,
                  symbolHeight = 200,
                  symbolWidth = 25
        ) %>%

        hc_add_theme(hc_theme_map_jc) %>%

        hc_xAxis(title = "") %>%
        hc_yAxis(title = "") %>%
        hc_title(
          text = paste0("Change in ", unique(df_map()$metric), " ", unique(df_map()$adm_or_pop), " from ", unique(df_map()$year)),
          align = "center",
          style = list(#fontFamily = "Graphik-Bold", # works in view but not in export
                       fontWeight = "bold",
                       fontFamily = "Graphik", # works in view and export but is the wrong font
                       fontSize = "30px",
                       useHTML = TRUE)
        ) %>%

        hc_setup() %>%
        hc_exporting(enabled = FALSE,
                     filename = map_filename(),
                     buttons = list(
                       contextButton = list(
                         menuItems = list('downloadPNG', 'downloadSVG')
                       ))) %>%

        hc_plotOptions(series = list(animation = FALSE,
                                     dataLabels = list(enabled = TRUE),
                                     cursor = "pointer",
                                     borderWidth = 3),
                       accessibility = list(enabled = TRUE,
                                            keyboardNavigation = list(enabled = TRUE),
                                            linkedDescription = 'This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year.',
                                            landmarkVerbosity = "one"),
                       area = list(accessibility = list(description = "This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year."))
        )

    } else {


      # Determine the new min and max where all values are negative
      NEW_MAX <- max_map
      NEW_MIN <- min_map

      highchart() %>%

        hc_add_series_map(
          map = hex_gj,
          df = df_map(),
          joinBy = "state_abb",
          value = "change",
          dataLabels = list(enabled = TRUE, format = "{point.datalabel}",
                            style = list(fontSize = "14px",
                                         fontWeight = "regular",
                                         fontFamily = "Graphik",
                                         textOutline = 0)),
          nullColor = "#e8e8e8") %>%

        hc_colorAxis(min = NEW_MIN,
                     max = NEW_MAX,
                     stops = color_stops(4, c(darkorange, orange, lightorange, white)),
                     labels = list(format = "{value}%",
                                   style = list(fontSize = "14px"))) %>%

        hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
                  #padding = 10,
                  symbolHeight = 200,
                  symbolWidth = 25) %>%

        hc_add_theme(hc_theme_map_jc) %>%


        hc_xAxis(title = "") %>%
        hc_yAxis(title = "") %>%
        hc_title(
          text = paste0("Change in ", unique(df_map()$metric), " ", unique(df_map()$adm_or_pop), " from ", unique(df_map()$year)),
          align = "center",
          style = list(#fontFamily = "Graphik-Bold", # works in view but not in export
                       fontWeight = "bold",
                       fontFamily = "Graphik",
                       fontSize = "30px",
                       useHTML = TRUE)) %>%

        hc_setup() %>%
        hc_exporting(enabled = FALSE,
                     filename = map_filename(),
                     buttons = list(
                       contextButton = list(
                         menuItems = list('downloadPNG', 'downloadSVG')
                       ))) %>%

        hc_plotOptions(series = list(animation = FALSE, dataLabels = list(enabled = TRUE), cursor = "pointer", borderWidth = 3),
                       accessibility = list(enabled = TRUE,
                                            keyboardNavigation = list(enabled = TRUE), linkedDescription = 'This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year.',
                                            landmarkVerbosity = "one"),
                       area = list(accessibility = list(description = "This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year."))
        )
    }

  })

  # output hex map
  output$hex_map <- renderHighchart({
    foundational_map()
  })

  #######
  # Download map button near dropdowns
  #######

  output$save_map <- downloadHandler(
    filename <- function() {
      paste("Change_", input$data_map, "_", input$adm_or_pop_map, "_", input$year_map, ".png", sep="")
    },

    content <- function(file) {
      file.copy(paste("data/Change_", input$data_map, "_", input$adm_or_pop_map, "_", input$year_map, ".png", sep=""), file)
    },
    contentType = "image/png"
  )

  # Links attempting to fix downloadHandler issues and/or highchart export issues

  # https://stackoverflow.com/questions/61347676/datalabels-in-r-highcharter-cannot-be-seen-after-print-as-png-or-jpg
  # https://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # https://groups.google.com/g/shiny-discuss/c/u7gwXc8_vyY/m/IZK_o7b7I8gJ
  # https://stackoverflow.com/questions/27008434/downloading-png-from-shiny-r-pt-2
  # https://github.com/jbkunst/highcharter/issues/151
  # https://stackoverflow.com/questions/48432042/r-shiny-downloadhandler-returns-app-html-rather-than-plots-or-data
  # https://www.highcharts.com/forum/viewtopic.php?f=9&t=45571#p162507
  # https://github.com/rstudio/shiny-server/issues/197

  # # Custom function that creates the hex map - need this to be called in downloadHandler
  # fnc_map_highchart <- function(data){
  #   # Get minimum and maximum value
  #   min_map <- min(data$change, na.rm = TRUE)
  #   max_map <- max(data$change, na.rm = TRUE)
  #
  #   # Get absolute value for comparison
  #   min_map_abs <- abs(min_map)
  #   max_map_abs <- abs(max_map)
  #
  #   # Get neg or pos sign for min and max
  #   min_map_type <- ifelse(min_map >= 0, "positive", "negative")
  #   max_map_type <- ifelse(max_map >= 0, "positive", "negative")
  #
  #   # Generate tile map
  #   # Has diverging scales when there are neg and pos values which centers the color gradient at zero
  #   # Has a gradient scale when both the min and max are both negative or both positive
  #
  #   # Determine the new min and max so that zero is centered
  #   # For example, If the highest positive value is 20 than the negative value is -20
  #   if (min_map_type != max_map_type) {
  #
  #     NEW_MAX <- case_when(
  #       max_map_abs > min_map_abs ~ max_map_abs,
  #       max_map_abs < min_map_abs ~ min_map_abs,
  #       max_map_abs == min_map_abs ~ max_map_abs)
  #     NEW_MIN <- case_when(
  #       min_map_abs > max_map_abs ~ min_map_abs,
  #       min_map_abs < max_map_abs ~ max_map_abs,
  #       min_map_abs == max_map_abs ~ min_map_abs)
  #     NEW_MAX <- ifelse(max_map_type == "negative", -abs(NEW_MAX), abs(NEW_MAX))
  #     NEW_MIN <- ifelse(min_map_type == "negative", -abs(NEW_MIN), abs(NEW_MIN))
  #
  #     highchart() %>%
  #
  #       hc_add_series_map(
  #         map = hex_gj,
  #         df = data,
  #         joinBy = "state_abb",
  #         value = "change",
  #         dataLabels = list(enabled = TRUE, format = "{point.datalabel}",
  #                           style = list(fontSize = "14px",
  #                                        fontWeight = "regular",
  #                                        fontFamily = "Graphik",
  #                                        textOutline = 0)),
  #         nullColor = "#e8e8e8") %>%
  #
  #       hc_colorAxis(min = NEW_MIN,
  #                    max = NEW_MAX,
  #                    stops = color_stops(7, c(darkorange, orange, lightorange, white, lightblue, regblue, darkblue)),
  #                    labels = list(format = "{value}%",
  #                                  style = list(fontSize = "14px"))
  #       ) %>%
  #
  #       hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
  #                 #padding = 10,
  #                 symbolHeight = 200,
  #                 symbolWidth = 25
  #       ) %>%
  #
  #       hc_add_theme(hc_theme_map_jc) %>%
  #
  #       hc_xAxis(title = "") %>%
  #       hc_yAxis(title = "") %>%
  #       hc_title(
  #         text = paste0("Change in ", unique(data$metric), " ", unique(data$adm_or_pop), " from ", unique(data$year)),
  #         align = "center",
  #         style = list(#fontFamily = "Graphik-Bold", # works in view but not in export
  #           fontWeight = "bold",
  #           fontFamily = "Graphik", # works in view and export but is the wrong font
  #           fontSize = "30px",
  #           useHTML = TRUE)
  #       ) %>%
  #
  #       hc_setup() %>%
  #       hc_exporting(enabled = FALSE,
  #                    filename = map_filename(),
  #                    buttons = list(
  #                      contextButton = list(
  #                        menuItems = list('downloadPNG', 'downloadSVG')
  #                      ))) %>%
  #
  #       hc_plotOptions(series = list(animation = FALSE,
  #                                    dataLabels = list(enabled = TRUE),
  #                                    cursor = "pointer",
  #                                    borderWidth = 3),
  #                      accessibility = list(enabled = TRUE,
  #                                           keyboardNavigation = list(enabled = TRUE),
  #                                           linkedDescription = 'This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year.',
  #                                           landmarkVerbosity = "one"),
  #                      area = list(accessibility = list(description = "This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year."))
  #       )
  #
  #   } else {
  #
  #
  #     # Determine the new min and max where all values are negative
  #     NEW_MAX <- max_map
  #     NEW_MIN <- min_map
  #
  #     highchart() %>%
  #
  #       hc_add_series_map(
  #         map = hex_gj,
  #         df = data,
  #         joinBy = "state_abb",
  #         value = "change",
  #         dataLabels = list(enabled = TRUE, format = "{point.datalabel}",
  #                           style = list(fontSize = "14px",
  #                                        fontWeight = "regular",
  #                                        fontFamily = "Graphik",
  #                                        textOutline = 0)),
  #         nullColor = "#e8e8e8") %>%
  #
  #       hc_colorAxis(min = NEW_MIN,
  #                    max = NEW_MAX,
  #                    stops = color_stops(4, c(darkorange, orange, lightorange, white)),
  #                    labels = list(format = "{value}%",
  #                                  style = list(fontSize = "14px"))) %>%
  #
  #       hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
  #                 #padding = 10,
  #                 symbolHeight = 200,
  #                 symbolWidth = 25) %>%
  #
  #       hc_add_theme(hc_theme_map_jc) %>%
  #
  #
  #       hc_xAxis(title = "") %>%
  #       hc_yAxis(title = "") %>%
  #       hc_title(
  #         text = paste0("Change in ", unique(data$metric), " ", unique(data$adm_or_pop), " from ", unique(data$year)),
  #         align = "center",
  #         style = list(#fontFamily = "Graphik-Bold", # works in view but not in export
  #           fontWeight = "bold",
  #           fontFamily = "Graphik",
  #           fontSize = "30px",
  #           useHTML = TRUE)) %>%
  #
  #       hc_setup() %>%
  #       hc_exporting(enabled = FALSE,
  #                    filename = map_filename(),
  #                    buttons = list(
  #                      contextButton = list(
  #                        menuItems = list('downloadPNG', 'downloadSVG')
  #                      ))) %>%
  #
  #       hc_plotOptions(series = list(animation = FALSE, dataLabels = list(enabled = TRUE), cursor = "pointer", borderWidth = 3),
  #                      accessibility = list(enabled = TRUE,
  #                                           keyboardNavigation = list(enabled = TRUE), linkedDescription = 'This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year.',
  #                                           landmarkVerbosity = "one"),
  #                      area = list(accessibility = list(description = "This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year."))
  #       )
  #   }
  # }
  # output$save_map <- downloadHandler(
  #   filename = function() {
  #     paste0("temp.html")
  #   },
  #   content = function(file) {
  #
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #
  #     data <- df_map()
  #     finalmap <- fnc_map_highchart(data = data)
  #
  #     htmlwidgets::saveWidget(finalmap, file = "temp.html", selfcontained = TRUE)
  #     file.copy('temp.html', file, overwrite = TRUE)
  #
  #   })
  # # Save map as png - doesn't work
  # output$save_map <- downloadHandler(
  #   filename = paste("MCLC_",input$data_map, "_", input$adm_or_pop_map, "_", input$year_map, ".png", sep=""),
  #   content = function(file) {
  #
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #
  #       saveWidget(foundational_map(), "temp.html", selfcontained = TRUE)
  #       webshot2::webshot(url = "temp.html", file = "temp.png", delay = 2)
  #     }
  # )

  ######### GGPLOT EXAMPLE TO SEE IF FONTS WORK - they do with ggplot #########
  # Plot1 <- reactive({
  #   ggplot(mtcars, aes(x = wt, y = mpg)) +
  #     geom_point()+ ggtitle(label = "Effect of Vitamin C on Tooth Growth",
  #                           subtitle = "Plot of length by dose") +
  #     theme(plot.title = element_text(face = "bold",
  #                                     size = 45,
  #                                     family = "Graphik"),
  #           plot.subtitle = element_text(family = "GraphikBold",
  #                                        size = 40))
  # })
  # output$plot1 <- renderPlot({
  #   p <- Plot1()
  #   print(p)
  # })
  #
  # output$save_map <-downloadHandler(
  #   filename = function() {
  #     paste('plot', '.png', sep='')
  #   },
  #   content=function(file){
  #     png(file)
  #     print(Plot1())
  #     dev.off()
  #   },
  #   contentType='image/png')

  #######
  # Table under hex map
  #######

  # Title of table under map based on user input
  output$selected_map_table <- renderText({paste(input$data_map, " ", input$adm_or_pop_map)})

  # Reactable table under hex map
  output$table_map <- renderReactable({

    filter_by <- paste0(input$data_map, " ", input$adm_or_pop_map)
    select_column = input$year_map
    select_column_name = paste0(input$year_map, " Change")
    df <- mclc_explorer_table[, c('state', 'data', '2018', '2019', '2020', '2021', select_column, 'total_new')]
    df <- df %>%
      filter(data == filter_by) %>%
      arrange(state) %>%
      rename(State = state,
             change = 7)

    reactable(df,
              style = list(fontFamily = "Graphik, sans-serif", fontSize = "1.4rem"),
              theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"), # was center
                                     headerStyle = list(textAlign = "right")
                                     ),
              defaultColDef = colDef(format = colFormat(separators = TRUE), align = "right"),
              compact = TRUE,
              fullWidth = FALSE,
              searchable = TRUE,

              language = reactableLang(
                searchPlaceholder = "Search for Your State"
                # # Accessible labels for assistive technology, such as screen readers
                # pagePreviousLabel = "Previous page",
                # pageNextLabel = "Next page"
              ),

              pagination = FALSE,
              columns = list(
                State           = colDef(name = "State", align = "left", minWidth = 120,
                                         style = list(fontWeight = "bold")),
                data            = colDef(show = F,
                                         name = "Metric", align = "left", minWidth = 240,
                                         style = list(fontWeight = "bold")),
                `2018`          = colDef(minWidth = 110),
                `2019`          = colDef(minWidth = 110),
                `2020`          = colDef(minWidth = 110),
                `2021`          = colDef(minWidth = 110),

                change = colDef(minWidth = 110,
                                name = select_column_name,
                                style = list(fontWeight = "bold"),
                                format = colFormat(percent = TRUE, digits = 1)),
                # add 4 Year trend graphs to each row
                total_new  =
                  colDef(minWidth = 140,
                         align = "center",
                         name = "4 Year Trend",
                         cell = function(value, index) {
                           dui_sparkline(
                             data = value[[1]],
                             height = 60,
                             #margin = list(top = 30, right = 20, bottom = 30, left = 20),
                             components = list(
                               dui_sparkpatternlines(
                                 id = "total",
                                 height = 4,
                                 width = 4,
                                 stroke = orange,
                                 strokeWidth = 2.5,
                                 orientation = "diagonal"),

                               dui_sparklineseries(
                                 curve = "linear",
                                 showArea = FALSE,
                                 fill = orange,
                                 stroke = orange)))})))

  })

  ##############################################################################################################################
  # State Reports
  ##############################################################################################################################

  #######
  # State page title
  #######

  # Title of state based on user input
  output$selected_state <- renderText({paste(input$adm_pop_report, " Trends in ", input$state_report, sep = "")})

  #######
  # Value boxes
  #######

  # Filter data to totals
  df_vb_total <- reactive({
    vb_adm_pop %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report &
             year == "2021" &
             metric == "Total")
  })

  # Filter data to sup viols
  df_vb_sup_viols <- reactive({
    vb_adm_pop %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report &
             year == "2021" &
             metric == "Supervision Violation")
  })

  # Filter data to tech viols
  df_vb_tech <- reactive({
    vb_adm_pop %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report &
             year == "2021" &
             metric == "Technical Violation")
  })

  # Filter data to new offense viols
  df_vb_new_off <- reactive({
    vb_adm_pop %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report &
             year == "2021" &
             metric == "New Offense")
  })

  # Value box for change in total admissions or population
  output$total_change <- renderValueBox({

    if (is.na(df_vb_total()$change)) {
      text <- "No Data"
    } else if (df_vb_total()$change < 0) {
      text <- tagList(HTML("&darr;"), paste0(df_vb_total()$change, "% from 2020"))
    } else {
      text <- tagList(HTML("&uarr;"), paste0(df_vb_total()$change, "% from 2020"))
    }

    if (is.na(df_vb_total()$total)) {
      header <- "No Data"
    } else {
      header <- comma(df_vb_total()$total, digits = 0)
    }

    valueBox2(
      header,
      title = paste0("Overall ", input$adm_pop_report, " in 2021"),
      subtitle = text,
      color = "black",
      href = NULL
    )

  })

  # Value box for change in supervision violation admissions or population
  output$sup_change <- renderValueBox({

    if (is.na(df_vb_sup_viols()$change)) {
      text <- "No Data"
    } else if (df_vb_sup_viols()$change < 0) {
      text <- tagList(HTML("&darr;"), paste0(df_vb_sup_viols()$change, "% from 2020"))
    } else {
      text <- tagList(HTML("&uarr;"), paste0(df_vb_sup_viols()$change, "% from 2020"))
    }

    if (is.na(df_vb_sup_viols()$total)) {
      header <- "No Data"
    } else {
      header <- comma(df_vb_sup_viols()$total, digits = 0)
    }

    valueBox2(
      header,
      title = paste0("Supervision Violation ", input$adm_pop_report, " in 2021"),
      subtitle = text,
      color = "black",
      href = NULL
    )

  })

  # Value box for change in technical violation admissions or population
  output$tech_change <- renderValueBox({

    if (is.na(df_vb_tech()$change)) {
      text <- "No Data"
    } else if (df_vb_tech()$change < 0) {
      text <- tagList(HTML("&darr;"), paste0(df_vb_tech()$change, "% from 2020"))
    } else {
      text <- tagList(HTML("&uarr;"), paste0(df_vb_tech()$change, "% from 2020"))
    }

    if (is.na(df_vb_tech()$total)) {
      header <- "No Data"
    } else {
      header <- comma(df_vb_tech()$total, digits = 0)
    }

    valueBox2(
      header,
      title = paste0("Technical Violation ", input$adm_pop_report, " in 2021"),
      subtitle = text,
      color = "black",
      href = NULL
    )

  })

  # Value box for change in new offense violation admissions or population
  output$new_off_change <- renderValueBox({

    if (is.na(df_vb_new_off()$change)) {
      text <- "No Data"
    } else if (df_vb_new_off()$change < 0) {
      text <- tagList(HTML("&darr;"), paste0(df_vb_new_off()$change, "% from 2020"))
    } else {
      text <- tagList(HTML("&uarr;"), paste0(df_vb_new_off()$change, "% from 2020"))
    }

    if (is.na(df_vb_new_off()$total)) {
      header <- "No Data"
    } else {
      header <- comma(df_vb_new_off()$total, digits = 0)
    }

    valueBox2(
      header,
      title = paste0("New Offense ", input$adm_pop_report, " in 2021"),
      subtitle = text,
      color = "black",
      href = NULL
    )

  })

  #######
  # Area chart
  #######

  # Area chart
  output$state_area_chart <- renderHighchart({
    if (input$adm_pop_report == "Admissions") {
      all_state_area_adm[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_exporting(enabled = FALSE,
                     filename = "Prison_Admissions_Overview",
                     buttons = list(
                       contextButton = list(
                         menuItems = list('downloadPNG', 'downloadSVG')
                       )))
    } else {
      all_state_area_pop[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_exporting(enabled = FALSE,
                     filename = "Prison_Population_Overview",
                     buttons = list(
                       contextButton = list(
                         menuItems = list('downloadPNG', 'downloadSVG')
                       )))
    }
  })

  # Download button
  output$save_state_area_chart <- downloadHandler(
    filename <- function() {
      paste(input$state_report, "_Prison_", input$adm_pop_report, ".png", sep="")
    },

    content <- function(file) {
      file.copy(paste("data/", input$state_report, "_Prison_", input$adm_pop_report, ".png", sep=""), file)
    },
    contentType = "image/png"
  )

  #######
  # Bar chart
  #######

  # Bar chart
  output$state_bar_chart <- renderHighchart({

    # Select highchart depending on selector input
    # Charts were saved in highchart.R
    if (input$adm_pop_report == "Admissions") {

      all_state_bar_adm[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_exporting(enabled = FALSE,
                     filename = "Supervision_Violation_Admissions_by_Type",
                     buttons = list(
                       contextButton = list(
                         menuItems = list('downloadPNG', 'downloadSVG')
                       )))
    } else {
      all_state_bar_pop[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_exporting(enabled = FALSE,
                     filename = "Supervision_Violation_Population_by_Type",
                     buttons = list(
                       contextButton = list(
                         menuItems = list('downloadPNG', 'downloadSVG')
                       )))
    }
  })

  # Download button
  output$save_state_bar_chart <- downloadHandler(
    filename <- function() {
      paste(input$state_report, "_Supervision_Violation_", input$adm_pop_report, "_by_Type.png", sep="")
    },

    content <- function(file) {
      file.copy(paste("data/", input$state_report, "_Supervision_Violation_", input$adm_pop_report, "_by_Type.png", sep=""), file)
    },
    contentType = "image/png"
  )

  #######
  # Supervision Violations Graph - Dynamically change between sentence and graph depending on data availability
  #######

  output$missing_data_nt_adm <- renderUI({
    out <- paste0(
        "<h3 class = 'nodata'>No data</h3>"
      , "<div class = 'notetxt'>"
      , input$state_report
      , " did not provide data on prison admissions due to technical and new offense violations."
      , "</div>"
    )
    HTML(out)
  })

  output$missing_data_nt_pop <- renderUI({
    out <- paste0(
        "<h3 class = 'nodata'>No data</h3>"
      , "<div class = 'notetxt'>"
      , input$state_report
      , " did not provide data on the number of people in prison due to technical and new offense violations."
      , "</div>"
    )
    HTML(out)
  })

  output$missing_data_nt_button <- renderText({
    ""
  })

  output$state_nt = renderUI({

    # If state is missing new offense and technical violations (Admissions)
    if(input$state_report %in% nt_na_adm & input$adm_pop_report == "Admissions"){
      htmlOutput("missing_data_nt_adm")

      # If state is missing new offense and technical violations (Population)
    } else if(input$state_report %in% nt_na_pop & input$adm_pop_report == "Population"){
      htmlOutput("missing_data_nt_pop")

      # If state has data (Admissions)
    } else if(input$state_report %in% nt_not_na_adm & input$adm_pop_report == "Admissions"){
      highchartOutput("state_bar_chart", height = 400, width = 390)

      # If state has data (Population)
    } else if(input$state_report %in% nt_not_na_pop & input$adm_pop_report == "Population"){
      highchartOutput("state_bar_chart", height = 400, width = 390)

    }

  })

  output$state_nt_button = renderUI({

    # If state is missing new offense and technical violations (Admissions)
    if(input$state_report %in% nt_na_adm & input$adm_pop_report == "Admissions"){
      textOutput("missing_data_nt_button")

      # If state is missing new offense and technical violations (Population)
    } else if(input$state_report %in% nt_na_pop & input$adm_pop_report == "Population"){
      textOutput("missing_data_nt_button")

      # If state has data (Admissions)
    } else if(input$state_report %in% nt_not_na_adm & input$adm_pop_report == "Admissions"){
      downloadButton(outputId = 'save_state_bar_chart', "", class = "download-chart")

      # If state has data (Population)
    } else if(input$state_report %in% nt_not_na_pop & input$adm_pop_report == "Population"){
      downloadButton(outputId = 'save_state_bar_chart', "", class = "download-chart")

    }

  })

  #######
  # Table under state graphs
  #######

  # This won't work because of library issues
  # output$state_table <- renderReactable({
  #   # Select reactable depending on selector input
  #   # tables were saved in reactable.R
  #   if (input$adm_pop_report == "Admissions") {
  #     state_reactable_adm[[input$state_report]]
  #   } else {
  #     state_reactable_pop[[input$state_report]]
  #   }
  # })

  # State table
  output$state_table <- renderReactable({

    # Filter data
    df <- state_table %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report) %>%
      group_by(text) %>%
      summarise(total_new = list(list(total)))
    df1 <- state_table_wide %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report) %>%
      arrange(order) %>%
      select(-adm_or_pop, -state)

    # Merge data
    df <- merge(df1, df, by = "text")
    df <- df %>% arrange(order) %>% select(-order)

    # Create table with 4 Year trend line in last column
    reactable(df,
              style = list(fontFamily = "Graphik, sans-serif", fontSize = "1.4rem"),
              theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"), # was center
                                     headerStyle = list(textAlign = "right")
              ),
              defaultColDef = colDef(format = colFormat(separators = TRUE), align = "right"),
              compact = TRUE,
              fullWidth = FALSE,
              searchable = FALSE,
              pagination = FALSE,
              columns = list(
                text            = colDef(name = "Metric",
                                       align = "left",
                                       minWidth = 275,
                                       style = list(fontWeight = "bold")),
                `2018`          = colDef(minWidth = 95),
                `2019`          = colDef(minWidth = 95),
                `2020`          = colDef(minWidth = 95),
                four_yr_change = colDef(minWidth = 110,
                                        name = "2018-2021 Change",
                                        style = list(fontWeight = "bold"),
                                        format = colFormat(percent = TRUE, digits = 1)),
                # Add 4 Year trend graphs to each row
                total_new  = colDef(minWidth = 110,
                                    name = "4 Year Trend",
                                    cell = function(value, index) {
                                      dui_sparkline(
                                        data = value[[1]],
                                        height = 80,
                                        margin = list(top = 30, right = 20, bottom = 30, left = 20),

                                        components = list(
                                          dui_sparkpatternlines(
                                            id = "total",
                                            height = 4,
                                            width = 4,
                                            stroke = total_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "sup_viols",
                                            height = 4,
                                            width = 4,
                                            stroke = viol_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "technical",
                                            height = 4,
                                            width = 4,
                                            stroke = tech_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "new_offense",
                                            height = 4,
                                            width = 4,
                                            stroke = new_o_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparklineseries(
                                            curve = "linear",
                                            showArea = FALSE,
                                            fill = colpal_fill[index],
                                            stroke = colpal_stroke[index])))}))
    )
  })

  #######
  # State notes
  #######

  # Filter data
  df_notes <- reactive({
    notes %>%
      filter(state == input$state_report)
  })

  # Title of state based on user input
  output$selected_state_note <- renderText({
    paste(df_notes()$notes)
  })

  #######
  # Parole Tab
  #######

  # Bar chart
  output$parole_bar_chart <- renderHighchart({
    # Select highchart depending on selector input
    # Carts were saved in highchart.R
    if (input$adm_pop_report == "Admissions") {
      parole_bar_adm[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_exporting(enabled = FALSE,
                     filename = "Parole_Violation_Admissions_by_Type",
                     buttons = list(
                       contextButton = list(
                         menuItems = list('downloadPNG', 'downloadSVG')
                       )))
    } else {
      parole_bar_pop[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_exporting(enabled = FALSE,
                     filename = "Parole_Violation_Population_by_Type",
                     buttons = list(
                       contextButton = list(
                         menuItems = list('downloadPNG', 'downloadSVG')
                       )))
    }
  })

  # Download button
  output$save_parole_bar_chart <- downloadHandler(
    filename <- function() {
      paste(input$state_report, "_Parole_Violation_", input$adm_pop_report, "_by_Type.png", sep="")
    },

    content <- function(file) {
      file.copy(paste("data/", input$state_report, "_Parole_Violation_", input$adm_pop_report, "_by_Type.png", sep=""), file)
    },
    contentType = "image/png"
  )

  # Parole table
  output$parole_table <- renderReactable({

    # Filter data
    df <- parole_table %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report) %>%
      group_by(text) %>%
      summarise(total_new = list(list(total)))
    df1 <- parole_table_wide %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report) %>%
      arrange(order)

    # Merge data
    df <- merge(df1, df, by = "text")
    df <- df %>% arrange(order) %>% select(-c(order, adm_or_pop, state, prob_vs_parole, metric))

    # Create table with 4 Year trend line in last column
    reactable(df,
              style = list(fontFamily = "Graphik, sans-serif", fontSize = "1.4rem"),
              theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"), # was center
                                     headerStyle = list(textAlign = "right")
              ),
              defaultColDef = colDef(format = colFormat(separators = TRUE), align = "right"),
              compact = TRUE,
              fullWidth = FALSE,
              searchable = FALSE,
              pagination = FALSE,
              columns = list(
                text            = colDef(name = "Metric",
                                         align = "left",
                                         style = list(fontWeight = "bold"),
                                         minWidth = 275),
                `2018`          = colDef(minWidth = 95),
                `2019`          = colDef(minWidth = 95),
                `2020`          = colDef(minWidth = 95),
                four_yr_change = colDef(minWidth = 110,
                                        name = "2018-2021 Change",
                                        style = list(fontWeight = "bold"),
                                        format = colFormat(percent = TRUE, digits = 1)),
                # add 4 Year trend graphs to each row
                total_new  = colDef(minWidth = 110,
                                    name = "4 Year Trend",
                                    cell = function(value, index) {
                                      dui_sparkline(
                                        data = value[[1]],
                                        height = 80,
                                        margin = list(top = 30, right = 20, bottom = 30, left = 20),

                                        components = list(
                                          dui_sparkpatternlines(
                                            id = "total",
                                            height = 4,
                                            width = 4,
                                            stroke = total_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "technical",
                                            height = 4,
                                            width = 4,
                                            stroke = tech_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "new_offense",
                                            height = 4,
                                            width = 4,
                                            stroke = new_o_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparklineseries(
                                            curve = "linear",
                                            showArea = FALSE,
                                            fill = colpal_fill1[index],
                                            stroke = colpal_stroke1[index])))}))
    )
  })

  # # This won't work because of library issues
  # output$parole_table <- renderReactable({
  #   # Select reactable depending on selector input
  #   # tables were saved in reactable.R
  #   if (input$adm_pop_report == "Admissions") {
  #     parole_reactable_adm[[input$state_report]]
  #   } else {
  #     parole_reactable_pop[[input$state_report]]
  #   }
  # })

  #######
  # Parole Graph - Dynamically change between sentence and graph depending on data availability
  #######

  output$missing_data_parole_nt_adm <- renderUI({
    out <- paste0(
        "<h3 class = 'nodata'>No data</h3>"
      , "<div class = 'notetxt'>"
      , input$state_report
      , " did not provide data on prison admissions due to technical and new offense parole violations."
      , "</div>"
    )
    HTML(out)
  })

  output$missing_data_parole_nt_pop <- renderUI({
    out <- paste0(
        "<h3 class = 'nodata'>No data</h3>"
      , "<div class = 'notetxt'>"
      , input$state_report
      , " did not provide data on the number of people in prison due to technical and new offense parole violations."
      , "</div>"
    )
    HTML(out)
  })

  output$abolished_parole_adm <- renderUI({
    out <- paste0(
        "<h3 class = 'nodata'>No data</h3>"
      , "<div class = 'notetxt'>"
      , input$state_report
      , " abolished parole and therefore did not provide data on prison admissions due to technical and new offense parole violations."
      , "</div>"
    )
    HTML(out)
  })

  output$abolished_parole_pop <- renderUI({
    out <- paste0(
        "<h3 class = 'nodata'>No data</h3>"
      , "<div class = 'notetxt'>"
      , input$state_report
      , " abolished parole and therefore did not provide data on the number of people in prison due to technical and new offense parole violations."
      , "</div>"
    )
    HTML(out)
  })
  
  output$parole_nt <- renderUI({
    
    # If state is missing new offense and technical violations (Admissions)
    if(input$state_report %in% parole_na_adm & input$adm_pop_report == "Admissions" & !(input$state_report %in% abolish_prob_parole)){
      
      fluidRow(column(width = 3),
               column(width = 6, align = "center", htmlOutput("missing_data_parole_nt_adm")),
               column(width = 3)
      )
      
      # If state is missing new offense and technical violations (Population)
    } else if(input$state_report %in% parole_na_pop & input$adm_pop_report == "Population" & !(input$state_report %in% abolish_prob_parole)){
      
      fluidRow(column(width = 3),
               column(width = 6, align = "center", htmlOutput("missing_data_parole_nt_pop")),
               column(width = 3)
      )
      
      # If state is missing new offense and technical violations (Admissions) AND abolished parole
    } else if(input$state_report %in% parole_na_adm & input$adm_pop_report == "Admissions" & (input$state_report %in% abolish_prob_parole)){
      
      fluidRow(column(width = 3),
               column(width = 6, align = "center", htmlOutput("abolished_parole_adm")),
               column(width = 3)
      )
      
      # If state is missing new offense and technical violations (Admissions) AND abolished parole
    } else if(input$state_report %in% parole_na_pop & input$adm_pop_report == "Population" & (input$state_report %in% abolish_prob_parole)){
      
      fluidRow(column(width = 3),
               column(width = 6, align = "center", htmlOutput("abolished_parole_pop")),
               column(width = 3)
      )
      
      # If state has data (Admissions)
    } else if(input$state_report %in% parole_not_na_adm & input$adm_pop_report == "Admissions"){
      
      fluidRow(column(width = 3),
               column(width = 5, align = "center", highchartOutput("parole_bar_chart", height = 400, width = 390)),
               column(width = 1, align = "left",   downloadButton(outputId = 'save_parole_bar_chart', "", class = "download-chart")),
               column(width = 3)
      )
      
      # If state has data (Population)
    } else if(input$state_report %in% parole_not_na_pop & input$adm_pop_report == "Population"){
      
      fluidRow(column(width = 3),
               column(width = 5, align = "center", highchartOutput("parole_bar_chart", height = 400, width = 390)),
               column(width = 1, align = "left",   downloadButton(outputId = 'save_parole_bar_chart', "", class = "download-chart")),
               column(width = 3)
      )
      
    }
    
  })

  # output$parole_nt = renderUI({
  # 
  #   # If state is missing new offense and technical violations (Admissions)
  #   if(input$state_report %in% parole_na_adm & input$adm_pop_report == "Admissions" & !(input$state_report %in% abolish_prob_parole)){
  #     htmlOutput("missing_data_parole_nt_adm")
  # 
  #     # If state is missing new offense and technical violations (Population)
  #   } else if(input$state_report %in% parole_na_pop & input$adm_pop_report == "Population" & !(input$state_report %in% abolish_prob_parole)){
  #     htmlOutput("missing_data_parole_nt_pop")
  # 
  #     # If state is missing new offense and technical violations (Admissions) AND abolished parole
  #   } else if(input$state_report %in% parole_na_adm & input$adm_pop_report == "Admissions" & (input$state_report %in% abolish_prob_parole)){
  #     htmlOutput("abolished_parole_adm")
  # 
  #     # If state is missing new offense and technical violations (Admissions) AND abolished parole
  #   } else if(input$state_report %in% parole_na_pop & input$adm_pop_report == "Population" & (input$state_report %in% abolish_prob_parole)){
  #     htmlOutput("abolished_parole_pop")
  # 
  #     # If state has data (Admissions)
  #   } else if(input$state_report %in% parole_not_na_adm & input$adm_pop_report == "Admissions"){
  #     highchartOutput("parole_bar_chart", height = 400, width = 390)
  # 
  #     # If state has data (Population)
  #   } else if(input$state_report %in% parole_not_na_pop & input$adm_pop_report == "Population"){
  #     highchartOutput("parole_bar_chart", height = 400, width = 390)
  # 
  #   }
  # 
  # })
  # output$parole_nt_button = renderUI({
  # 
  #   # If state is missing new offense and technical violations (Admissions)
  #   if(input$state_report %in% parole_na_adm & input$adm_pop_report == "Admissions"){
  #     textOutput("missing_data_parole_nt_button")
  # 
  #     # If state is missing new offense and technical violations (Population)
  #   } else if(input$state_report %in% parole_na_pop & input$adm_pop_report == "Population"){
  #     textOutput("missing_data_parole_nt_button")
  # 
  #     # If state has data (Admissions)
  #   } else if(input$state_report %in% parole_not_na_adm & input$adm_pop_report == "Admissions"){
  #     downloadButton(outputId = 'save_parole_bar_chart', "", class = "download-chart")
  # 
  #     # If state has data (Population)
  #   } else if(input$state_report %in% parole_not_na_pop & input$adm_pop_report == "Population"){
  #     downloadButton(outputId = 'save_parole_bar_chart', "", class = "download-chart")
  # 
  #   }
  # 
  # })

  #######
  # Probation Tab
  #######

  # Bar chart
  output$probation_bar_chart <- renderHighchart({
    # Select highchart depending on selector input
    # Carts were saved in highchart.R
    if (input$adm_pop_report == "Admissions") {
      probation_bar_adm[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_exporting(enabled = FALSE,
                     filename = "Probation_Violation_Admissions_by_Type",
                     buttons = list(
                       contextButton = list(
                         menuItems = list('downloadPNG', 'downloadSVG')
                       )))
    } else {
      probation_bar_pop[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_exporting(enabled = FALSE,
                     filename = "Probation_Violation_Population_by_Type",
                     buttons = list(
                       contextButton = list(
                         menuItems = list('downloadPNG', 'downloadSVG')
                       )))
    }
  })

  # Download button
  output$save_probation_bar_chart <- downloadHandler(
    filename <- function() {
      paste(input$state_report, "_Probation_Violation_", input$adm_pop_report, "_by_Type.png", sep="")
    },

    content <- function(file) {
      file.copy(paste("data/", input$state_report, "_Probation_Violation_", input$adm_pop_report, "_by_Type.png", sep=""), file)
    },
    contentType = "image/png"
  )

  # Probation table
  output$probation_table <- renderReactable({

    # Filter data
    df <- probation_table %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report) %>%
      group_by(text) %>%
      summarise(total_new = list(list(total)))
    df1 <- probation_table_wide %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report) %>%
      arrange(order)

    # Merge data
    df <- merge(df1, df, by = "text")
    df <- df %>% arrange(order) %>% select(-c(order, adm_or_pop, state, prob_vs_parole, metric))

    # Create table with 4 Year trend line in last column
    reactable(df,
              style = list(fontFamily = "Graphik, sans-serif", fontSize = "1.4rem"),
              theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"), # was center
                                     headerStyle = list(textAlign = "right")
              ),
              defaultColDef = colDef(format = colFormat(separators = TRUE), align = "right"),
              compact = TRUE,
              fullWidth = FALSE,
              searchable = FALSE,
              pagination = FALSE,
              columns = list(
                text            = colDef(name = "Metric",
                                         align = "left",
                                         style = list(fontWeight = "bold"),
                                         minWidth = 275),
                `2018`          = colDef(minWidth = 95),
                `2019`          = colDef(minWidth = 95),
                `2020`          = colDef(minWidth = 95),
                four_yr_change = colDef(minWidth = 110,
                                        name = "2018-2021 Change",
                                        style = list(fontWeight = "bold"),
                                        format = colFormat(percent = TRUE, digits = 1)),
                # add 4 Year trend graphs to each row
                total_new  = colDef(minWidth = 110,
                                    name = "4 Year Trend",
                                    cell = function(value, index) {
                                      dui_sparkline(
                                        data = value[[1]],
                                        height = 80,
                                        margin = list(top = 30, right = 20, bottom = 30, left = 20),

                                        components = list(
                                          dui_sparkpatternlines(
                                            id = "total",
                                            height = 4,
                                            width = 4,
                                            stroke = total_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "technical",
                                            height = 4,
                                            width = 4,
                                            stroke = tech_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "new_offense",
                                            height = 4,
                                            width = 4,
                                            stroke = new_o_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparklineseries(
                                            curve = "linear",
                                            showArea = FALSE,
                                            fill = colpal_fill1[index],
                                            stroke = colpal_stroke1[index])))}))
    )
  })

  # This won't work because of library issues
  # output$probation_table <- renderReactable({
  #   # Select reactable depending on selector input
  #   # tables were saved in reactable.R
  #   if (input$adm_pop_report == "Admissions") {
  #     probation_reactable_adm[[input$state_report]]
  #   } else {
  #     probation_reactable_pop[[input$state_report]]
  #   }
  # })

  #######
  # Probation Graph - Dynamically change between sentence and graph depending on data availability
  #######

  output$missing_data_probation_nt_adm <- renderUI({
    out <- paste0(
      "<h3 class = 'nodata'>No data</h3>"
      , "<div class = 'notetxt'>"
      , input$state_report
      , " did not provide data on prison admissions due to technical and new offense probation violations."
      , "</div>"
    )
    HTML(out)
  })

  output$missing_data_probation_nt_pop <- renderUI({
    out <- paste0(
      "<h3 class = 'nodata'>No data</h3>"
      , "<div class = 'notetxt'>"
      , input$state_report
      , " state did not provide data on the number of people in prison due to technical and new offense probation violations."
      , "</div>"
    )
    HTML(out)
  })

  output$missing_data_probation_nt_button <- renderText({
    ""
  })

  output$probation_nt = renderUI({

    # If state is missing new offense and technical violations (Admissions)
    if(input$state_report %in% probation_na_adm & input$adm_pop_report == "Admissions"){
      htmlOutput("missing_data_probation_nt_adm")

      # If state is missing new offense and technical violations (Population)
    } else if(input$state_report %in% probation_na_pop & input$adm_pop_report == "Population"){
      htmlOutput("missing_data_probation_nt_pop")

      # If state has data (Admissions)
    } else if(input$state_report %in% probation_not_na_adm & input$adm_pop_report == "Admissions"){
      highchartOutput("probation_bar_chart", height = 400, width = 390)

      # If state has data (Population)
    } else if(input$state_report %in% probation_not_na_pop & input$adm_pop_report == "Population"){
      highchartOutput("probation_bar_chart", height = 400, width = 390)

    }

  })

  output$probation_nt_button = renderUI({

    # If state is missing new offense and technical violations (Admissions)
    if(input$state_report %in% probation_na_adm & input$adm_pop_report == "Admissions"){
      textOutput("missing_data_probation_nt_button")

      # If state is missing new offense and technical violations (Population)
    } else if(input$state_report %in% probation_na_pop & input$adm_pop_report == "Population"){
      textOutput("missing_data_probation_nt_button")

      # If state has data (Admissions)
    } else if(input$state_report %in% probation_not_na_adm & input$adm_pop_report == "Admissions"){
      downloadButton(outputId = 'save_probation_bar_chart', "", class = "download-chart")

      # If state has data (Population)
    } else if(input$state_report %in% probation_not_na_pop & input$adm_pop_report == "Population"){
      downloadButton(outputId = 'save_probation_bar_chart', "", class = "download-chart")

    }

  })

  ####
  ## RACE/ETHNICITY DISPARITIES MYE HERE
  ###

  output$infogblack <- renderImage({

    png_file  <- glue("data/infogs/{input$adm_pop_report}_{input$state_report}_{input$pop_denom}_Black.png")

    if (file.exists(png_file)){
      plot <- png_file
      list(
          src =normalizePath(plot)
        , contentType = "image/png"
        , alt = raceethnicity$infograph_alt(rridata, input$adm_pop_report, input$pop_denom, "Black", input$state_report)
        , width = "100%"
      )
    } else {
      plot <- ggplot2::ggplot() + ggplot2::theme_void()
      file <- tempfile(fileext = ".png")
      ggplot2::ggsave(filename = file, plot = plot, width = 24, height = 0.5)
      list(
          src =normalizePath(file)
        , contentType = "image/png"
        , alt = raceethnicity$infograph_alt_noinfog(input$adm_pop_report, input$pop_denom, "Black", input$state_report)
        , width = "100%"
      )
    }
  }, deleteFile = FALSE)

  output$infoghisp <- renderImage({

    png_file  <- glue("data/infogs/{input$adm_pop_report}_{input$state_report}_{input$pop_denom}_Hispanic.png")

    if (file.exists(png_file)){
      plot <- png_file
      list(
          src =normalizePath(plot)
        , contentType = "image/png"
        , alt = raceethnicity$infograph_alt(rridata, input$adm_pop_report, input$pop_denom, "Hispanic", input$state_report)
        , width = "100%"
      )
    } else {
      plot <- ggplot2::ggplot() + ggplot2::theme_void()
      file <- tempfile(fileext = ".png")
      ggplot2::ggsave(filename = file, plot = plot, width = 24, height = 0.5)
      list(
          src =normalizePath(file)
        , contentType = "image/png"
        , alt = raceethnicity$infograph_alt_noinfog(input$adm_pop_report, input$pop_denom, "Hispanic", input$state_report)
        , width = "100%"
      )
    }
  }, deleteFile = FALSE)


  output$infogheader <- renderUI({

    dataavail <- rridata[[input$adm_pop_report]][[input$pop_denom]][[input$state_report]]$INFOGRAPH$DATAAVAIL

    out <- paste0(
        raceethnicity$pop_denom_text(input$pop_denom)
      , raceethnicity$infographic_header(dataavail, rridata[[input$adm_pop_report]][[input$pop_denom]][[input$state_report]]$INFOGRAPH$NOTE)
    )

    HTML(out)

  })


  output$table_rri_header <- renderUI({
    df <- raceethnicity$create_tabledf(rridata, input$adm_pop_report, input$pop_denom, input$state_report, "RRI", whichTABLE = "table_suppress")
    main <- "<h4 class='reh4'> Relative Rate Index (White Reference Group)</h4>"
    if (nrow(df) > 0){
      out <- main
    } else {
      out <- paste0( main
                    , "<div class = 'retxt'>"
                    , "Data to calculate relative rate index were not available for "
                    , input$state_report
                    , "</div>"
                    )
    }
    HTML(out)
  })

  output$table_rri <- renderUI({
    df <- raceethnicity$create_tabledf(rridata, input$adm_pop_report, input$pop_denom, input$state_report, "RRI", whichTABLE = "table_suppress")
    dataavail <- rridata[[input$adm_pop_report]][[input$pop_denom]][[input$state_report]]$INFOGRAPH$DATAAVAIL
    if (dataavail == 1){
      raceethnicity$create_reactable(df)
    }

  })


  output$table_rate_header <- renderUI({
    df <- raceethnicity$create_tabledf(rridata, input$adm_pop_report, input$pop_denom, input$state_report, "RATE", whichTABLE = "table_suppress")
    mult <- scales::comma(rridata[[input$adm_pop_report]][[input$pop_denom]][[input$state_report]][["RATE"]]$mult)
    main <- paste0("<h4 class='reh4'> Rate per ", mult, " persons in the Population</h4>")
    if (nrow(df) > 0){
      out <- main
    } else {
      out <- paste0(  main
                    , "<div class = 'retxt'>"
                    , "Data to calculate rates were not available for "
                    , input$state_report
                    , "</div>"
      )
    }
    HTML(out)
  })

  output$table_rate <- renderUI({
    df <- raceethnicity$create_tabledf(rridata, input$adm_pop_report, input$pop_denom, input$state_report, "RATE", whichTABLE = "table_suppress")
    if (nrow(df) > 1){
      raceethnicity$create_reactable(df)
    }
  })


  output$table_revcnt_header <- renderUI({
    df <- raceethnicity$create_tabledf(rridata, input$adm_pop_report, input$pop_denom, input$state_report, "REVCNT", whichTABLE = "table_suppress")
    main <- "<h4 class='reh4'>Parole Revocations Counts</h4>"
    if (nrow(df) > 0){
      out <- main
    } else {
      out <- paste0(  main
                    , "<div class = 'retxt'>"
                    , "Parole revocations data were not available for "
                    , input$state_report
                    , "</div>"
      )
    }
    HTML(out)
  })

  output$table_revcnt <- renderUI({
    df <- raceethnicity$create_tabledf(rridata, input$adm_pop_report, input$pop_denom, input$state_report, "REVCNT", whichTABLE = "table_suppress")
    if (nrow(df) > 1){
      raceethnicity$create_reactable(df)
    }
  })

  ##############################################################################################################################
  # Download
  ##############################################################################################################################

  # Select multiple years
  filteredYears <- reactive({
    unique(csg$year)
  })
  observeEvent(filteredYears(), {
    updatePickerInput(session, inputId = 'download_year',
                      label = 'Select Year(s)',
                      choices = filteredYears(),
                      selected = filteredYears())
  })

  # Select multiple state
  filteredState <- reactive({
    unique(csg$state)
  })
  observeEvent(filteredState(), {
    updatePickerInput(session, inputId = 'download_state',
                      label = 'Select State(s)',
                      choices = filteredState(),
                      selected = filteredState())
  })

  # Select multiple metrics
  filteredMetric <- reactive({
    unique(csg$metric)
  })
  observeEvent(filteredMetric(), {
    updatePickerInput(session, inputId = 'download_metric',
                      label = 'Select Metric(s)',
                      choices = filteredMetric(),
                      selected = filteredMetric())
  })

  # Filter data depending on user input
  df_download_table <- reactive({
    csg %>%
      filter(year %in% input$download_year) %>%
      filter(state %in% input$download_state) %>%
      filter(metric %in% input$download_metric) %>%
      arrange(state, year)
  })

  # Save data as csv
  output$save_data <- downloadHandler(
    filename = function() {
      paste("MCLC_", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      write.csv(df_download_table(), con,
                row.names = FALSE)
    }
  )

  # Reactable table of MCLC data for download
  output$selected_download_table <- renderReactable({


    df1 <- df_download_table() %>%
      mutate(state = factor(state))

    reactable(df1,
              style = list(fontFamily = "Graphik, sans-serif", fontSize = "1.4rem"),
              theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
              defaultColDef = colDef(format = colFormat(separators = TRUE), align = "left"),
              compact = TRUE,
              fullWidth = FALSE,
              defaultPageSize = 50,
              #filterable = TRUE,

              columns = list(
                state  = colDef(name = "State",
                                align = "left",
                                style = list(fontWeight = "bold"),
                                minWidth = 200,
                                ),
                metric = colDef(name = "Metric",
                                minWidth = 370),
                year   = colDef(name = "Year",
                                minWidth = 110),
                total  = colDef(name = "Total",
                                align = "right",
                                minWidth = 110,
                                filterable = FALSE)
              )
              )

  })

}
