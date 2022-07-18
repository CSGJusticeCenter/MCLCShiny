#######################################
# Project: MCLCShiny
# File: server.R
# Authors: Mari Roberts
# Date last updated: June 10, 2022
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

  # # title of map based on user input
  # output$selected_map <- renderText({paste("Change in ", input$data_map, " ", input$adm_or_pop_map, "from ", input$year_map)})

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

  #######
  # Hex map
  #######

  # create foundational hex map and store it as a reactive expression
  foundational_map <- reactive({

    ################ TO DO find min and max values and put in dataframe in import.R
    # get minimum and maximum value
    min_map <- min(df_map()$change, na.rm = TRUE)
    max_map <- max(df_map()$change, na.rm = TRUE)

    # get absolute value for comparison
    min_map_abs <- abs(min_map)
    max_map_abs <- abs(max_map)

    # get neg or pos sign
    min_map_type <- ifelse(min_map >= 0, "positive", "negative")
    max_map_type <- ifelse(max_map >= 0, "positive", "negative")

    # # create tooltip
    # df_plot <- df_map() %>%
    #   mutate(tooltip = paste0("<b>", state, "</b><br>","Change from ", year, "<br>",change, "%<br>"),
    #          datalabel = ifelse(is.na(change), paste0("", state_abb, ""),
    #                             paste0("<p style=", "text-align:center", ">", state_abb, "", "<br>",
    #                                    round(change, 0), "%</p>")))

    # determine the new min and max so that zero is centered
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

      # generate tile map
      # has diverging scales when there are neg and pos values which centers the color gradient at zero
      # has a gradient scale when both the min and max are both negative or both positive
      highchart() %>%

        hc_add_series_map(
          map = hex_gj,
          df = df_map(),
          joinBy = "state_abb",
          value = "change",
          dataLabels = list(enabled = TRUE, format = "{point.datalabel}",
                            style = list(fontSize = "14px", fontWeight = "regular", textOutline = 0)),
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
        hc_xAxis(title = "") %>%
        hc_yAxis(title = "") %>%
        hc_title(
          text = paste0("Change in ", unique(df_map()$metric), " ", unique(df_map()$adm_or_pop), " from ", unique(df_map()$year)),
          align = "left",
          style = list(fontWeight = "bold", fontSize = "24px", useHTML = TRUE)
        ) %>%

        hc_add_theme(hc_theme_map_jc) %>%
        hc_setup() %>%

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

      NEW_MAX <- max_map
      NEW_MIN <- min_map

      highchart() %>%

        hc_add_series_map(
          map = hex_gj,
          df = df_map(),
          joinBy = "state_abb",
          value = "change",
          dataLabels = list(enabled = TRUE, format = "{point.datalabel}",
                            style = list(fontSize = "14px", fontWeight = "regular", textOutline = 0)),
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

        hc_xAxis(title = "") %>%
        hc_yAxis(title = "") %>%
        hc_title(
          text = paste0("Change in ", unique(df_map()$metric), " ", unique(df_map()$adm_or_pop), " from ", unique(df_map()$year)),
          align = "left",
          style = list(fontWeight = "bold", fontSize = "24px", useHTML = TRUE)) %>%

        hc_add_theme(hc_theme_map_jc) %>%
        hc_setup() %>%

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

  # store the current user-created version of the  map for download in a reactive expression
  final_map <- reactive({
    foundational_map()
  })

  #######
  # Table under hex map
  #######

  # title of table under map based on user input
  output$selected_map_table <- renderText({paste(input$data_map, " ", input$adm_or_pop_map)})

  # table under hex map
  output$table_map = DT::renderDataTable({
    # https://stackoverflow.com/questions/64097670/jquery-datatable-heading-and-search-on-the-same-line
    datatable(df_map_table(),
              # callback = JS("$('#DataTables_Table_0_filter input').css('background-color', 'yellow');"),
              class = list(stripe = FALSE),
              options = list(dom = 'ft',
                             pageLength = 50,
                             language = list(search = "", searchPlaceholder = "Search"),
                             columnDefs = list(list(visible=FALSE, targets=c(1)),
                                               list(className = 'dt-left', targets = '_all'))),
              rownames = FALSE) %>%
              formatPercentage(c("2018 - 2019", "2019 - 2020"), 2) %>%
              formatCurrency(c("2018", "2019", "2020"), currency = " ", interval = 3, mark = ",") %>%
              formatRound(columns=c("2018", "2019", "2020"), digits = 0)
  })

  #######
  # Download buttons near dropdowns
  #######

  # save table under map as csv
  output$save_map_data <- downloadHandler(
    filename = function() {
      paste("MCLC_",input$data_map, "_", input$adm_or_pop_map, ".csv", sep="")
    },
    content = function(file) {
      write.csv(df_map_table(), file)}
    )

  # save map as pdf
  output$save_map <- downloadHandler(
    filename = paste("MCLC_",input$data_map, "_", input$adm_or_pop_map, "_", input$year_map, ".pdf", sep=""),
    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      saveWidget(foundational_map(), "temp.html", selfcontained = FALSE)
      webshot("temp.html", file = file, cliprect = "viewport")
    }
  )

  ##############################################################################################################################
  # State Reports
  ##############################################################################################################################

  #######
  # State page title
  #######

  # title of state based on user input
  output$selected_state <- renderText({paste(input$adm_pop_report, " Trends in ", input$state_report, sep = "")})

  #######
  # Value boxes
  #######

  # filter data to totals
  df_vb_total <- reactive({
    vb_adm_pop %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report &
             year == "2020" &
             metric == "Total")
  })

  # filter data to sup viols
  df_vb_sup_viols <- reactive({
    vb_adm_pop %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report &
             year == "2020" &
             metric == "Supervision Violation")
  })

  # filter data to tech viols
  df_vb_tech <- reactive({
    vb_adm_pop %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report &
             year == "2020" &
             metric == "Technical Violation")
  })

  # filter data to new offense viols
  df_vb_new_off <- reactive({
    vb_adm_pop %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report &
             year == "2020" &
             metric == "New Offense")
  })

  # value box for change in total admissions or population
  output$total_change <- renderValueBox({

    if (is.na(df_vb_total()$change)) {
      text <- "No Data"
    } else if (df_vb_total()$change < 0) {
      text <- tagList(HTML("&darr;"), paste0(df_vb_total()$change, "% from 2019"))
    } else {
      text <- tagList(HTML("&uarr;"), paste0(df_vb_total()$change, "% from 2019"))
    }

    if (is.na(df_vb_total()$total)) {
      header <- "No Data"
    } else {
      header <- comma(df_vb_total()$total, digits = 0)
    }

    valueBox2(
      header,
      title = paste0("Overall ", input$adm_pop_report, " in 2020"),
      subtitle = text,
      color = "black",
      href = NULL
    )

  })

  # value box for change in supervision violation admissions or population
  output$sup_change <- renderValueBox({

    if (is.na(df_vb_sup_viols()$change)) {
      text <- "No Data"
    } else if (df_vb_sup_viols()$change < 0) {
      text <- tagList(HTML("&darr;"), paste0(df_vb_sup_viols()$change, "% from 2019"))
    } else {
      text <- tagList(HTML("&uarr;"), paste0(df_vb_sup_viols()$change, "% from 2019"))
    }

    if (is.na(df_vb_sup_viols()$total)) {
      header <- "No Data"
    } else {
      header <- comma(df_vb_sup_viols()$total, digits = 0)
    }

    valueBox2(
      header,
      title = paste0("Supervision Violation ", input$adm_pop_report, " in 2020"),
      subtitle = text,
      color = "black",
      href = NULL
    )

  })

  # value box for change in technical violation admissions or population
  output$tech_change <- renderValueBox({

    if (is.na(df_vb_tech()$change)) {
      text <- "No Data"
    } else if (df_vb_tech()$change < 0) {
      text <- tagList(HTML("&darr;"), paste0(df_vb_tech()$change, "% from 2019"))
    } else {
      text <- tagList(HTML("&uarr;"), paste0(df_vb_tech()$change, "% from 2019"))
    }

    if (is.na(df_vb_tech()$total)) {
      header <- "No Data"
    } else {
      header <- comma(df_vb_tech()$total, digits = 0)
    }

    valueBox2(
      header,
      title = paste0("Technical Violation ", input$adm_pop_report, " in 2020"),
      subtitle = text,
      color = "black",
      href = NULL
    )

  })

  # value box for change in new offense violation admissions or population
  output$new_off_change <- renderValueBox({

    if (is.na(df_vb_new_off()$change)) {
      text <- "No Data"
    } else if (df_vb_new_off()$change < 0) {
      text <- tagList(HTML("&darr;"), paste0(df_vb_new_off()$change, "% from 2019"))
    } else {
      text <- tagList(HTML("&uarr;"), paste0(df_vb_new_off()$change, "% from 2019"))
    }

    if (is.na(df_vb_new_off()$total)) {
      header <- "No Data"
    } else {
      header <- comma(df_vb_new_off()$total, digits = 0)
    }

    valueBox2(
      header,
      title = paste0("New Offense ", input$adm_pop_report, " in 2020"),
      subtitle = text,
      color = "black",
      href = NULL
    )

  })

  #######
  # Area chart
  #######

  # output area chart
  output$state_area_chart <- renderHighchart({
    # select highchart depending on selector input
    # charts were saved in highchart.R
    if (input$adm_pop_report == "Admissions") {
      eval(parse(text = paste("all_state_area_adm$",input$state_report, sep = "")))
    } else {
      eval(parse(text = paste("all_state_area_pop$",input$state_report, sep = "")))
    }
  })

  #######
  # Bar chart
  #######

  # output bar chart
  output$state_bar_chart <- renderHighchart({
    # select highchart depending on selector input
    # charts were saved in highchart.R
    if (input$adm_pop_report == "Admissions") {
      eval(parse(text = paste("all_state_bar_adm$",input$state_report, sep = "")))
    } else {
      eval(parse(text = paste("all_state_bar_pop$",input$state_report, sep = "")))
    }
  })

  #######
  # Table under state graphs
  #######

  output$state_table <- renderReactable({

    # filter data
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

    # merge data
    df <- merge(df1, df, by = "text")
    df <- df %>% arrange(order) %>% select(-order)

    # create table with 3 year trend line in last column
    reactable(df,
              theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
              defaultColDef = colDef(format = colFormat(separators = TRUE), align = "center"),
              compact = TRUE,
              fullWidth = FALSE,
              columns = list(
                text            = colDef(name = "Metric",
                                         align = "left",
                                         minWidth = 275),
                `2018`          = colDef(minWidth = 95),
                `2019`          = colDef(minWidth = 95),
                `2020`          = colDef(minWidth = 95),
                three_yr_change = colDef(minWidth = 110,
                                         name = "3 Year Change",
                                         format = colFormat(percent = TRUE, digits = 1)),
                # add 3 year trend graphs to each row
                total_new  = colDef(minWidth = 110,
                                    name = "3 Year Trend",
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

  # filter data
  df_notes <- reactive({
    notes %>%
      filter(state == input$state_report)
  })

  # title of state based on user input
  output$selected_state_note <- renderText({
    paste(df_notes()$notes)
  })

  #######
  # Parole Tab
  #######

  # filter data for grouped barchart
  df_parole_bar_chart <- reactive({
    adm_pop_long %>%
      filter(state == input$state_report &
             adm_or_pop == input$adm_pop_report,
             prob_vs_parole == "Parole") %>%
      group_by(state, year, metric, adm_or_pop) %>%
      summarise(total = sum(total)) %>%
      filter(metric == "New Offense" | metric == "Technical Violation") %>%
      mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, digits = 0), "<br>"))
  })

  # output bar chart
  output$parole_bar_chart <- renderHighchart({

    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = df_parole_bar_chart()$metric) %>%
      hc_add_series(data = subset(df_parole_bar_chart(), metric == "Technical Violation"), name = "Technical Violation", type = "column", hcaes(x = year, y = total), color = tech_co) %>%
      hc_add_series(data = subset(df_parole_bar_chart(), metric == "New Offense"), name = "New Offense", type = "column", hcaes(x = year, y = total), color = new_o_co) %>%

      hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020)) %>%
      hc_yAxis(title = "") %>%
      hc_title(
        text = paste0("Supervision Violation ", unique(df_parole_bar_chart()$adm_or_pop)),
        align = "left",
        style = list(fontWeight = "bold", fontSize = "16px", useHTML = TRUE)
      ) %>%

      hc_add_theme(hc_theme_jc) %>%
      hc_setup() %>%

      hc_plotOptions(series = list(animation = FALSE, cursor = "pointer", borderWidth = 3),
                     accessibility = list(enabled = TRUE,
                                          keyboardNavigation = list(enabled = TRUE), linkedDescription = 'This bar chart was created by a selected state and selected data type, either admissions or population.
                                          Image description: A grouped bar chart showing the number of parole technical violation admissions or population,
                                          and new offense admissions or population. The chart is interactive, and the user can hover over each state to see the total for each metric and year.',
                                          landmarkVerbosity = "one"),
                     area = list(accessibility = list(description = 'This bar chart was created by a selected state and selected data type, either admissions or population.
                                          Image description: A grouped bar chart showing the number of parole technical violation admissions or population,
                                          and new offense admissions or population. The chart is interactive, and the user can hover over each state to see the total for each metric and year.')))

  })

  ##############################################################################################################################
  # Download
  ##############################################################################################################################



}
