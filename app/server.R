#######################################
# Project: MCLCShiny
# File: server.R
# Authors: Mari Roberts
# Date last updated: June 5, 2023 (MAR)
# Description:
#    Server for shiny app
#######################################

server <- function(input, output, session) {

  # # Enable caching
  # shinyjs::enable("cache")

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

  # Output blank text
  output$blank <- renderText({
    ""
  })

  ##############################################################################################################################
  # MAP EXPLORER
  ##############################################################################################################################

  # # save reactive values for which states are clicked on in the hex map
  # selected_state_map <- reactiveVal(NULL)
  #
  # # map clicking
  # click_js <- JS("function(event) {
  #   var state = event.point.state_abb;
  #   Shiny.setInputValue('selected_state_map', state);
  #   $('#navbarID a[href=\"#statereports\"]').tab('show');
  # }")

  #######
  # Hex map data
  #######

  # Data for table under map
  df_map_table <- reactive({
    filter_by <- paste0(input$data_map, " ",
                        input$adm_or_pop_map)
    select_column = input$year_map
    df <- mclc_explorer_table[, c('state',
                                  'data',
                                  '2018',
                                  '2019',
                                  '2020',
                                  '2021',
                                  select_column,
                                  'total_new')]
    df <- df %>%
      filter(data == filter_by) %>%
      arrange(state) %>%
      rename(State = state,
             change = 7)
  }) %>%
    bindCache(input$data_map,
              input$adm_or_pop_map,
              input$year_map)

  # Dynamically change name of map
  map_filename <- reactive({
    name <- mclc_explorer %>%
      filter(adm_or_pop == input$adm_or_pop_map,
             metric     == input$data_map,
             year       == input$year_map) %>%
      select(data, year) %>% distinct()
    name$year <- gsub(" - ", " ", name$year)
    name <- paste(name$data, name$year, sep = '_')
    name <- gsub(" ", "_", name)
  }) %>%
    bindCache(input$data_map,
              input$adm_or_pop_map,
              input$year_map)


  #######
  # Hex map
  #######

  # Select foundational hex map and store it as a reactive expression
  # Charts were created in highchart.R
  # This is necessary to download the map
  foundational_map <- reactive({

    if(input$adm_or_pop_map == "Admissions"){

      if(input$year_map == "2018 - 2019"){
        adm_maps_2018_2019[[input$data_map]]%>%
          highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
          highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
          highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
          highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
          hc_boost(enabled = TRUE)
      }

      else if(input$year_map == "2018 - 2021"){
        adm_maps_2018_2021[[input$data_map]]%>%
          highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
          highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
          highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
          highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
          hc_boost(enabled = TRUE)
      }

      else if(input$year_map == "2019 - 2020"){
        adm_maps_2019_2020[[input$data_map]]%>%
          highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
          highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
          highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
          highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
          hc_boost(enabled = TRUE)
      }

      else if(input$year_map == "2020 - 2021"){
        adm_maps_2020_2021[[input$data_map]]%>%
          highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
          highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
          highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
          highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
          hc_boost(enabled = TRUE)
      }

    }

    else if(input$adm_or_pop_map == "Population"){

      if(input$year_map == "2018 - 2019"){
        pop_maps_2018_2019[[input$data_map]]%>%
          highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
          highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
          highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
          highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
          hc_boost(enabled = TRUE)
      }

      else if(input$year_map == "2018 - 2021"){
        pop_maps_2018_2021[[input$data_map]]%>%
          highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
          highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
          highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
          highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
          hc_boost(enabled = TRUE)
      }

      else if(input$year_map == "2019 - 2020"){
        pop_maps_2019_2020[[input$data_map]]%>%
          highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
          highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
          highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
          highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
          hc_boost(enabled = TRUE)
      }

      else if(input$year_map == "2020 - 2021"){
        pop_maps_2020_2021[[input$data_map]]%>%
          highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
          highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
          highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
          highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
          hc_boost(enabled = TRUE)
      }
    }

  }) %>%
    bindCache(input$data_map,
              input$adm_or_pop_map,
              input$year_map)

  # output hex map
  output$hex_map <- renderHighchart({
    foundational_map()
    # %>%
    # hc_plotOptions(series = list(events = list(click = click_js)))
  })

  # # redirect to the statereports tab and update selected state
  # observeEvent(input$selected_state_map, {
  #   selected_state_map(input$selected_state_map)
  #   updateNavbarPage(session, "navbarID", selected = "statereports")
  # })
  #
  # # update selectInput choices based on selected state from hex map
  # observeEvent(selected_state_map(), {
  #
  #   # get state name from state abbreviation
  #   state_name <- state.name[match(selected_state_map(), state.abb)]
  #
  #   updateSelectInput(session, "state_report",
  #                     selected = state_name,
  #                     choices = ifelse(is.null(state_name), NULL, state_name))
  # })

  #######
  # Download map button near dropdowns
  #######

  output$save_map <- downloadHandler(
    filename <- function() {
      paste("Change_", input$data_map, "_", input$adm_or_pop_map, "_",
            input$year_map, ".png", sep="")
    },
    content <- function(file) {
      file.copy(paste("data/plots/Change_", input$data_map, "_",
                      input$adm_or_pop_map, "_",
                      input$year_map, ".png", sep=""), file)
    },
    contentType = "image/png"
  )

  #######
  # Table under hex map
  #######

  # Title of table under map based on user input
  output$selected_map_table <-
    renderText({paste(input$data_map, " ",
                      input$adm_or_pop_map)}) %>%
    bindCache(input$data_map,
              input$adm_or_pop_map)


  # Title of map based on user input
  output$selected_map_table <- renderText({
    if (input$adm_or_pop_map == "Admissions" & input$data_map == "Total") {
      paste(input$data_map, " ", input$adm_or_pop_map, " to State Prison", sep = "")

    } else if (input$adm_or_pop_map == "Population" & input$data_map == "Total") {
      paste(input$data_map, " ", input$adm_or_pop_map, " in State Prison", sep = "")

    } else if (input$adm_or_pop_map == "Admissions" & input$data_map != "Total") {
      paste("State Prison Admissions for ", input$data_map, "s", sep = "")

    } else if (input$adm_or_pop_map == "Population" & input$data_map != "Total") {
      paste("People in State Prison for ", input$data_map, "s", sep = "")
    }

  }) %>%
    bindCache(input$data_map,
              input$adm_or_pop_map)


  # Reactable table under hex map
  output$table_map <- renderReactable({

    filter_by <- paste0(input$data_map, " ",
                        input$adm_or_pop_map)
    select_column <- input$year_map
    select_column_name <- paste0(select_column, " Change")
    select_trend_data_column <-
      if(input$year_map == "2018 - 2021"){
        paste0("trend_data_18_21")
      } else if(input$year_map == "2018 - 2019"){
        paste0("trend_data_18_19")
      } else if(input$year_map == "2019 - 2020"){
        paste0("trend_data_19_20")
      } else if(input$year_map == "2020 - 2021"){
        paste0("trend_data_20_21")
      }

    select_trend_column <-
      if(input$year_map == "2018 - 2021"){
        paste0("trend_18_21")
      } else if(input$year_map == "2018 - 2019"){
        paste0("trend_18_19")
      } else if(input$year_map == "2019 - 2020"){
        paste0("trend_19_20")
      } else if(input$year_map == "2020 - 2021"){
        paste0("trend_20_21")
      }

    df <- mclc_explorer_table %>%
      select(state, data, `2018`, `2019`, `2020`, `2021`,
             all_of(select_column),
             all_of(select_trend_data_column),
             all_of(select_trend_column)) %>%
      filter(data == filter_by) %>%
      arrange(state) %>%
      rename(change = all_of(select_column),
             total_new = 8,
             trend     = 9) %>%
      mutate(
        trend = case_when(
          trend == "negative"   ~ regblue
          , trend == "positive" ~ orange
          , trend == "same"     ~ "#585858"
        )
      )

    reactable(df,
              style = list(fontFamily = "Graphik, sans-serif",
                           fontSize = "1.4rem"),
              theme = reactableTheme(cellStyle = list(display = "flex",
                                                      flexDirection = "column",
                                                      justifyContent = "center"),
                                     headerStyle = list(textAlign = "right")
              ),
              defaultColDef = colDef(format = colFormat(separators = TRUE),
                                     align = "right"),
              compact = TRUE,
              fullWidth = FALSE,
              searchable = TRUE,

              language = reactableLang(searchPlaceholder = "Search for Your State"),

              pagination = FALSE,
              columns = list(
                state           = colDef(name = "State",
                                         align = "left",
                                         minWidth = 120,
                                         style = list(fontWeight = "bold")),
                data            = colDef(show = F,
                                         name = "Metric",
                                         align = "left",
                                         minWidth = 240,
                                         style = list(fontWeight = "bold")),
                `2018`          = colDef(na = "–", minWidth = 110),
                `2019`          = colDef(na = "–", minWidth = 110),
                `2020`          = colDef(na = "–", minWidth = 110),
                `2021`          = colDef(na = "–", minWidth = 110),

                change = colDef(na = "–",
                                minWidth = 110,
                                name = select_column_name,
                                style = list(fontWeight = "bold"),
                                format = colFormat(percent = TRUE, digits = 1)),
                # add 4 Year trend graphs to each row
                total_new  =
                  colDef(na = "–",
                         minWidth = 140,
                         align = "center",
                         name = "Trend Line",
                         sortable = FALSE,
                         cell = function(value, index) {
                           dui_sparkline(
                             data = value[[1]],
                             height = 60,
                             components = list(
                               dui_sparkpatternlines(
                                 id = "total",
                                 height = 4,
                                 width = 4,
                                 stroke = df$trend[index],
                                 strokeWidth = 2.5,
                                 orientation = "diagonal"),

                               dui_sparklineseries(
                                 curve = "linear",
                                 showArea = FALSE,
                                 fill = df$trend[index],
                                 stroke = df$trend[index]
                               )
                             )
                           )}),
                #trend, don't show, used in determining
                trend = colDef(show = FALSE)
              ))
  }) %>%
    bindCache(input$data_map,
              input$adm_or_pop_map,
              input$year_map)




  ##############################################################################################################################
  # State Reports
  ##############################################################################################################################

  #######
  # State page title
  #######

  # Title of state based on user input
  output$selected_state <- renderText({
    if (input$adm_pop_report == "Admissions") {
      paste("Prison Admission Trends in", input$state_report)
    } else if (input$adm_pop_report == "Population") {
      paste("Prison Population Trends in", input$state_report)
    } else {
      ""
    }
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

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
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  # Filter data to sup violations
  df_vb_sup_violations <- reactive({
    vb_adm_pop %>%
      filter(state == input$state_report &
               adm_or_pop == input$adm_pop_report &
               year == "2021" &
               metric == "Supervision Violation")
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  # Filter data to tech violations
  df_vb_tech <- reactive({
    vb_adm_pop %>%
      filter(state == input$state_report &
               adm_or_pop == input$adm_pop_report &
               year == "2021" &
               metric == "Technical Violation")
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  # Filter data to new offense violations
  df_vb_new_off <- reactive({
    vb_adm_pop %>%
      filter(state == input$state_report &
               adm_or_pop == input$adm_pop_report &
               year == "2021" &
               metric == "New Offense Violation")
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  # Value box for change in total admissions or population
  output$total_change <- renderValueBox({

    # No subtitle needed regarding no parole or prob data
    fnc_value_box(
      title      = paste0("Overall "),
      adm_or_pop = paste0(input$adm_pop_report, " in 2021"),
      subtitle   = " ",
      value      = df_vb_total()$value_shown,
      finding    = df_vb_total()$text,
      color      = "black",
      href       = NULL,
      width      = 5
    )

  })

  # Value box for change in supervision violation admissions or population
  output$sup_change <- renderValueBox({

    fnc_value_box(
      title      = paste0("Supervision Violation "),
      adm_or_pop = paste0(input$adm_pop_report, " in 2021"),
      subtitle   = df_vb_sup_violations()$subheader,
      value      = df_vb_sup_violations()$value_shown,
      finding    = df_vb_sup_violations()$text,
      color      = "black",
      href       = NULL,
      width      = 5
    )
  })

  # Value box for change in technical violation admissions or population
  output$tech_change <- renderValueBox({

    fnc_value_box(
      title      = paste0("Technical Violation "),
      adm_or_pop = paste0(input$adm_pop_report, " in 2021"),
      subtitle   = df_vb_tech()$subheader,
      value      = df_vb_tech()$value_shown,
      finding    = df_vb_tech()$text,
      color      = "black",
      href       = NULL,
      width      = 5
    )
  })

  # Value box for change in new offense violation admissions or population
  output$new_off_change <- renderValueBox({

    fnc_value_box(
      title      = paste0("New Offense Violation "),
      adm_or_pop = paste0(input$adm_pop_report, " in 2021"),
      subtitle   = df_vb_new_off()$subheader,
      value      = df_vb_new_off()$value_shown,
      finding    = df_vb_new_off()$text,
      color      = "black",
      href       = NULL,
      width      = 5
    )
  })

  #######
  # Area chart
  #######

  # Select highchart depending on selector input
  # Charts were saved in highchart.R
  # Area chart
  output$state_area_chart <- renderHighchart({
    if (input$adm_pop_report == "Admissions") {
      all_state_area_adm[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_boost(enabled = TRUE)
    } else {
      all_state_area_pop[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_boost(enabled = TRUE)
    }
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  # Download button
  output$save_state_area_chart <- downloadHandler(
    filename <- function() {
      paste(input$state_report, "_Prison_",
            input$adm_pop_report, ".png", sep="")
    },

    content <- function(file) {
      file.copy(paste("data/plots/",
                      input$state_report, "_Prison_",
                      input$adm_pop_report, ".png", sep=""), file)
    },
    contentType = "image/png"
  )

  output$state_area_button = renderUI({

    # If state is missing new offense violations and technical violations (Admissions)
    if(!(input$state_report %in% states) &
       input$adm_pop_report == "Admissions"){
      textOutput("blank")

      # If state is missing new offense violations and technical violations (Population)
    } else if(!(input$state_report %in% states) &
              input$adm_pop_report == "Population"){
      textOutput("blank")

      # If state has data (Admissions)
    } else if(input$state_report %in% states &
              input$adm_pop_report == "Admissions"){
      downloadButton(outputId = 'save_state_area_chart', "",
                     class = "download-chart")

      # If state has data (Population)
    } else if(input$state_report %in% states &
              input$adm_pop_report == "Population"){
      downloadButton(outputId = 'save_state_area_chart', "",
                     class = "download-chart")
    }

  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  output$state_area = renderUI({

    # If state is missing new offense violations and technical violations (Admissions)
    if(!(input$state_report %in% states) &
       input$adm_pop_report == "Admissions"){
      textOutput("blank")

      # If state is missing new offense violations and technical violations (Population)
    } else if(!(input$state_report %in% states) &
              input$adm_pop_report == "Population"){
      textOutput("blank")

      # If state has data (Admissions)
    } else if(input$state_report %in% states &
              input$adm_pop_report == "Admissions"){
      highchartOutput("state_area_chart",
                      height = 400, width = 390)

      # If state has data (Population)
    } else if(input$state_report %in% states &
              input$adm_pop_report == "Population"){
      highchartOutput("state_area_chart",
                      height = 400, width = 390)
    }

  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

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
        hc_boost(enabled = TRUE)
    } else {
      all_state_bar_pop[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_boost(enabled = TRUE)
    }
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  # Download button
  output$save_state_bar_chart <- downloadHandler(
    filename <- function() {
      paste(input$state_report, "_Supervision_Violation_",
            input$adm_pop_report, "_by_Type.png", sep="")
    },

    content <- function(file) {
      file.copy(paste("data/plots/",
                      input$state_report, "_Supervision_Violation_",
                      input$adm_pop_report, "_by_Type.png", sep=""), file)
    },
    contentType = "image/png"
  )


  #######
  # Supervision Violations Graph - Dynamically change between
  # sentence and graph depending on data availability
  #######

  # If data is missing a supervision violation admissions graph
  output$missing_data_nt_adm <- renderUI({
    out <- missingness_sentences %>%
      filter(state == input$state_report)
    out <- out$supervision_violation_admissions_graph
    HTML(out)
  }) %>%
    bindCache(input$state_report)

  # If data is missing a supervision violation population graph
  output$missing_data_nt_pop <- renderUI({
    out <- missingness_sentences %>%
      filter(state == input$state_report)
    out <- out$supervision_violation_population_graph
    HTML(out)
  }) %>%
    bindCache(input$state_report)

  # Remove download button if no graph
  output$missing_data_nt_button <- renderText({
    ""
  })

  # Show graph or missing data sentence depending on state
  output$state_nt = renderUI({

    # If state is missing new offense violations and technical violations (Admissions)
    if(input$state_report %in% nt_na_adm &
       input$adm_pop_report == "Admissions"){
      htmlOutput("missing_data_nt_adm")

      # If state is missing new offense violations and technical violations (Population)
    } else if(input$state_report %in% nt_na_pop &
              input$adm_pop_report == "Population"){
      htmlOutput("missing_data_nt_pop")

      # If state has data (Admissions)
    } else if(input$state_report %in% nt_not_na_adm &
              input$adm_pop_report == "Admissions"){
      highchartOutput("state_bar_chart", height = 400, width = 390)

      # If state has data (Population)
    } else if(input$state_report %in% nt_not_na_pop &
              input$adm_pop_report == "Population"){
      highchartOutput("state_bar_chart", height = 400, width = 390)
    }

  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  # Show graph download button or no button depending on state
  output$state_nt_button = renderUI({

    # If state is missing new offense violations and technical violations (Admissions)
    if(input$state_report %in% nt_na_adm &
       input$adm_pop_report == "Admissions"){
      textOutput("missing_data_nt_button")

      # If state is missing new offense violations and technical violations (Population)
    } else if(input$state_report %in% nt_na_pop &
              input$adm_pop_report == "Population"){
      textOutput("missing_data_nt_button")

      # If state has data (Admissions)
    } else if(input$state_report %in% nt_not_na_adm &
              input$adm_pop_report == "Admissions"){
      downloadButton(outputId = 'save_state_bar_chart', "",
                     class = "download-chart")

      # If state has data (Population)
    } else if(input$state_report %in% nt_not_na_pop &
              input$adm_pop_report == "Population"){
      downloadButton(outputId = 'save_state_bar_chart', "",
                     class = "download-chart")
    }

  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  #######
  # Table under state graphs
  #######

  # # This won't work because of htmlwidget and dataui issues
  # output$state_table <- reactable::renderReactable({
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
              theme = reactableTheme(cellStyle = list(display = "flex",
                                                      flexDirection = "column",
                                                      justifyContent = "center"), # was center
                                     headerStyle = list(textAlign = "right")
              ),
              defaultColDef = colDef(format = colFormat(separators = TRUE),
                                     align = "right"),
              compact = TRUE,
              fullWidth = FALSE,
              searchable = FALSE,
              pagination = FALSE,
              columns = list(
                text            = colDef(name = "Metric",
                                         align = "left",
                                         minWidth = 275,
                                         style = list(fontWeight = "bold")),
                `2018`          = colDef(na = "–", minWidth = 95),
                `2019`          = colDef(na = "–", minWidth = 95),
                `2020`          = colDef(na = "–", minWidth = 95),
                `2021`          = colDef(na = "–", minWidth = 95),
                four_yr_change  = colDef(na = "–", minWidth = 110,
                                         name = "2018-2021 Change",
                                         style = list(fontWeight = "bold"),
                                         format = colFormat(percent = TRUE,
                                                            digits = 1)),
                # Add 4 Year trend graphs to each row
                total_new  = colDef(minWidth = 110,
                                    name = "4 Year Trend",
                                    cell = function(value, index) {
                                      dui_sparkline(
                                        data = value[[1]],
                                        height = 80,
                                        margin = list(top = 30,
                                                      right = 20,
                                                      bottom = 30,
                                                      left = 20),

                                        components = list(
                                          dui_sparkpatternlines(
                                            id = "total",
                                            height = 4,
                                            width = 4,
                                            stroke = total_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"),

                                          dui_sparkpatternlines(
                                            id = "sup_violations",
                                            height = 4,
                                            width = 4,
                                            stroke = viol_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"),

                                          dui_sparkpatternlines(
                                            id = "technical",
                                            height = 4,
                                            width = 4,
                                            stroke = tech_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"),

                                          dui_sparkpatternlines(
                                            id = "new_offense",
                                            height = 4,
                                            width = 4,
                                            stroke = new_o_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"),

                                          dui_sparklineseries(
                                            curve = "linear",
                                            showArea = FALSE,
                                            fill = colpal_fill[index],
                                            stroke = colpal_stroke[index])))})))
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  #######
  # State notes
  #######

  # Filter data
  df_notes <- reactive({
    notes %>%
      filter(state == input$state_report)
  }) %>%
    bindCache(input$state_report)

  # Title of state based on user input
  output$selected_state_note <- renderUI({
    HTML(df_notes()$notes)
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
        hc_boost(enabled = TRUE)
    } else {
      parole_bar_pop[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_boost(enabled = TRUE)
    }
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  # Download button
  output$save_parole_bar_chart <- downloadHandler(
    filename <- function() {
      paste(input$state_report, "_Parole_Violation_",
            input$adm_pop_report, "_by_Type.png", sep="")
    },

    content <- function(file) {
      file.copy(paste("data/plots/",
                      input$state_report, "_Parole_Violation_",
                      input$adm_pop_report, "_by_Type.png", sep=""), file)
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
    df <- df %>% arrange(order) %>%
      select(-c(order, adm_or_pop, state, prob_vs_parole, metric))

    # Create table with 4 Year trend line in last column
    reactable(df,
              style = list(fontFamily = "Graphik, sans-serif", fontSize = "1.4rem"),
              theme = reactableTheme(cellStyle = list(display = "flex",
                                                      flexDirection = "column",
                                                      justifyContent = "center"), # was center
                                     headerStyle = list(textAlign = "right")
              ),
              defaultColDef = colDef(format = colFormat(separators = TRUE),
                                     align = "right"),
              compact = TRUE,
              fullWidth = FALSE,
              searchable = FALSE,
              pagination = FALSE,
              columns = list(
                text            = colDef(name = "Metric",
                                         align = "left",
                                         style = list(fontWeight = "bold"),
                                         minWidth = 275),
                `2018`          = colDef(na = "–", minWidth = 95),
                `2019`          = colDef(na = "–", minWidth = 95),
                `2020`          = colDef(na = "–", minWidth = 95),
                `2021`          = colDef(na = "–", minWidth = 95),
                four_yr_change  = colDef(na = "–", minWidth = 110,
                                         name = "2018-2021 Change",
                                         style = list(fontWeight = "bold"),
                                         format = colFormat(percent = TRUE,
                                                            digits = 1)),
                # add 4 Year trend graphs to each row
                total_new  = colDef(minWidth = 110,
                                    name = "4 Year Trend",
                                    cell = function(value, index) {
                                      dui_sparkline(
                                        data = value[[1]],
                                        height = 80,
                                        margin = list(top = 30,
                                                      right = 20,
                                                      bottom = 30,
                                                      left = 20),

                                        components = list(
                                          dui_sparkpatternlines(
                                            id = "total",
                                            height = 4,
                                            width = 4,
                                            stroke = total_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"),

                                          dui_sparkpatternlines(
                                            id = "technical",
                                            height = 4,
                                            width = 4,
                                            stroke = tech_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"),

                                          dui_sparkpatternlines(
                                            id = "new_offense",
                                            height = 4,
                                            width = 4,
                                            stroke = new_o_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"),

                                          dui_sparklineseries(
                                            curve = "linear",
                                            showArea = FALSE,
                                            fill = colpal_fill1[index],
                                            stroke = colpal_stroke1[index])))})))
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

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

  # If data is missing a parole violation admissions graph
  output$missing_data_parole_nt_adm <- renderUI({
    out <- missingness_sentences %>%
      filter(state == input$state_report)
    out <- out$parole_violation_admissions_graph
    HTML(out)
  }) %>%
    bindCache(input$state_report)

  # If data is missing a parole violation population graph
  output$missing_data_parole_nt_pop <- renderUI({
    out <- missingness_sentences %>%
      filter(state == input$state_report)
    out <- out$parole_violation_population_graph
    HTML(out)
  }) %>%
    bindCache(input$state_report)

  # Show parole graph or missing data sentence depending on state
  output$parole_nt <- renderUI({

    # If state is missing new offense violations and technical violations (Admissions)
    if(input$state_report %in% parole_na_adm &
       input$adm_pop_report == "Admissions"){

      fluidRow(column(width = 3),
               column(width = 6, align = "center",
                      htmlOutput("missing_data_parole_nt_adm")),
               column(width = 3))

      # If state is missing new offense violations and technical violations (Population)
    } else if(input$state_report %in% parole_na_pop &
              input$adm_pop_report == "Population"){

      fluidRow(column(width = 3),
               column(width = 6, align = "center",
                      htmlOutput("missing_data_parole_nt_pop")),
               column(width = 3))

      # If state has data (Admissions)
    } else if(input$state_report %in% parole_not_na_adm &
              input$adm_pop_report == "Admissions"){

      fluidRow(column(width = 3),
               column(width = 5, align = "center",
                      highchartOutput("parole_bar_chart",
                                      height = 400,
                                      width = 390)),
               column(width = 1, align = "left",
                      downloadButton(outputId = 'save_parole_bar_chart', "",
                                     class = "download-chart")),
               column(width = 3))

      # If state has data (Population)
    } else if(input$state_report %in% parole_not_na_pop &
              input$adm_pop_report == "Population"){

      fluidRow(column(width = 3),
               column(width = 5, align = "center",
                      highchartOutput("parole_bar_chart",
                                      height = 400, width = 390)),
               column(width = 1, align = "left",
                      downloadButton(outputId = 'save_parole_bar_chart', "",
                                     class = "download-chart")),
               column(width = 3))
    }
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

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
        hc_boost(enabled = TRUE)
    } else {
      probation_bar_pop[[input$state_report]] %>%
        highcharter::hc_add_dependency(name = "plugins/series-label.js") %>%
        highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
        highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
        highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
        hc_boost(enabled = TRUE)
    }
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  # Download button
  output$save_probation_bar_chart <- downloadHandler(
    filename <- function() {
      paste(input$state_report, "_Probation_Violation_",
            input$adm_pop_report, "_by_Type.png", sep="")
    },

    content <- function(file) {
      file.copy(paste("data/plots/", input$state_report, "_Probation_Violation_",
                      input$adm_pop_report, "_by_Type.png", sep=""), file)
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
    df <- df %>% arrange(order) %>%
      select(-c(order, adm_or_pop, state, prob_vs_parole, metric))

    # Create table with 4 Year trend line in last column
    reactable(df,
              style = list(fontFamily = "Graphik, sans-serif",
                           fontSize = "1.4rem"),
              theme = reactableTheme(cellStyle = list(display = "flex",
                                                      flexDirection = "column",
                                                      justifyContent = "center"), # was center
                                     headerStyle = list(textAlign = "right")
              ),
              defaultColDef = colDef(format = colFormat(separators = TRUE),
                                     align = "right"),
              compact = TRUE,
              fullWidth = FALSE,
              searchable = FALSE,
              pagination = FALSE,
              columns = list(
                text            = colDef(name = "Metric",
                                         align = "left",
                                         style = list(fontWeight = "bold"),
                                         minWidth = 275),
                `2018`          = colDef(na = "–", minWidth = 95),
                `2019`          = colDef(na = "–", minWidth = 95),
                `2020`          = colDef(na = "–", minWidth = 95),
                `2021`          = colDef(na = "–", minWidth = 95),
                four_yr_change  = colDef(na = "–", minWidth = 110,
                                         name = "2018-2021 Change",
                                         style = list(fontWeight = "bold"),
                                         format = colFormat(percent = TRUE,
                                                            digits = 1)),
                # add 4 Year trend graphs to each row
                total_new  = colDef(minWidth = 110,
                                    name = "4 Year Trend",
                                    cell = function(value, index) {
                                      dui_sparkline(
                                        data = value[[1]],
                                        height = 80,
                                        margin = list(top = 30,
                                                      right = 20,
                                                      bottom = 30,
                                                      left = 20),

                                        components = list(
                                          dui_sparkpatternlines(
                                            id = "total",
                                            height = 4,
                                            width = 4,
                                            stroke = total_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"),

                                          dui_sparkpatternlines(
                                            id = "technical",
                                            height = 4,
                                            width = 4,
                                            stroke = tech_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"),

                                          dui_sparkpatternlines(
                                            id = "new_offense",
                                            height = 4,
                                            width = 4,
                                            stroke = new_o_co,
                                            strokeWidth = 2.5,
                                            orientation = "diagonal"),

                                          dui_sparklineseries(
                                            curve = "linear",
                                            showArea = FALSE,
                                            fill = colpal_fill1[index],
                                            stroke = colpal_stroke1[index])))})))
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

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

  # If data is missing a probation violation admissions graph
  output$missing_data_probation_nt_adm <- renderUI({
    out <- missingness_sentences %>%
      filter(state == input$state_report)
    out <- out$probation_violation_admissions_graph
    HTML(out)
  }) %>%
    bindCache(input$state_report)

  # If data is missing a probation violation population graph
  output$missing_data_probation_nt_pop <- renderUI({
    out <- missingness_sentences %>%
      filter(state == input$state_report)
    out <- out$probation_violation_population_graph
    HTML(out)
  }) %>%
    bindCache(input$state_report)

  # Show probation graph or missing data sentence depending on state
  output$probation_nt <- renderUI({

    # If state is missing new offense violations and technical violations (Admissions)
    if(input$state_report %in% probation_na_adm &
       input$adm_pop_report == "Admissions"){

      fluidRow(column(width = 3),
               column(width = 6, align = "center",
                      htmlOutput("missing_data_probation_nt_adm")),
               column(width = 3))

      # If state is missing new offense violations and technical violations (Population)
    } else if(input$state_report %in% probation_na_pop &
              input$adm_pop_report == "Population"){

      fluidRow(column(width = 3),
               column(width = 6,
                      align = "center",
                      htmlOutput("missing_data_probation_nt_pop")),
               column(width = 3))

      # If state has data (Admissions)
    } else if(input$state_report %in% probation_not_na_adm &
              input$adm_pop_report == "Admissions"){

      fluidRow(column(width = 3),
               column(width = 5, align = "center",
                      highchartOutput("probation_bar_chart",
                                      height = 400, width = 390)),
               column(width = 1, align = "left",
                      downloadButton(outputId = 'save_probation_bar_chart', "",
                                     class = "download-chart")),
               column(width = 3))

      # If state has data (Population)
    } else if(input$state_report %in% probation_not_na_pop &
              input$adm_pop_report == "Population"){

      fluidRow(column(width = 3),
               column(width = 5, align = "center",
                      highchartOutput("probation_bar_chart",
                                      height = 400, width = 390)),
               column(width = 1, align = "left",
                      downloadButton(outputId = 'save_probation_bar_chart', "",
                                     class = "download-chart")),
               column(width = 3))
    }
  }) %>%
    bindCache(input$state_report,
              input$adm_pop_report)

  ####
  ## RACE/ETHNICITY DISPARITIES  TAB
  ###

  # When Race/Ethinicity tab is selected, show pop up about how data is not MCLC
  # This will only occur once per session automatically, see localsession
  localsession <- TRUE
  observeEvent(input$tabsetpanel, {
    if (input$tabsetpanel == 4 & localsession)  {
      localsession <<- FALSE
      re_modal()
      observeEvent(input$close_modal, {
        removeModal()
        dataavail <- rridata[[input$adm_pop_report]][[input$pop_denom]][[input$state_report]]$INFOGRAPH$DATAAVAIL

        if (dataavail == 0) {
          first_guide$init()
          first_guide$remove(step = c("#infopanel-id", "ip1"))
        } else {
          first_guide$init()
          first_guide$remove(step = c("#infopanel-id", "ip2"))
        }

        first_guide$start()
      })
    }
  })

  observeEvent(input$show_guide, {
    re_modal()
  })

  output$retitleend <- renderUI({
    case_when(
      input$adm_pop_report == "Admissions" ~ "in Readmissions to Prison from Parole"
      , input$adm_pop_report == "Population" ~ "in Incarcerated Populations Readmitted to Prison from Parole"
    )
  })

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
    } else { #should not be needed - used as back-up
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
    } else { #should not be needed - used as back-up
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
      raceethnicity$pop_denom_text(input$pop_denom, input$adm_pop_report)
      , raceethnicity$infographic_header(dataavail, input$pop_denom, input$adm_pop_report, rridata[[input$adm_pop_report]][[input$pop_denom]][[input$state_report]]$INFOGRAPH$NOTE)
    )
    HTML(out)
  })

  output$howitscalculated <- renderUI({
    out <- rridata[[input$adm_pop_report]][[input$pop_denom]][[input$state_report]]$CALCTXT
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
    main <- raceethnicity$rate_table_header(input$pop_denom, input$adm_pop_report, rridata[[input$adm_pop_report]][[input$pop_denom]][[input$state_report]][["RATE"]]$mult)
    if (nrow(df) > 0){
      out <- main
    } else {
      out <- paste0(  main
                      , "<div class = 'retxt'>"
                      , "Data to calculate rates were not available for "
                      , input$state_report
                      , "</div>")
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
    main <- "<h4 class='reh4'>Readmissions to Prison from Parole Counts</h4>"
    if (nrow(df) > 0){
      out <- main
    } else {
      out <- paste0(  main
                      , "<div class = 'retxt'>"
                      , "Readmissions to prison from parole data were not available for "
                      , input$state_report
                      , "</div>")
    }
    HTML(out)
  })

  output$table_revcnt <- renderUI({
    df <- raceethnicity$create_tabledf(rridata, input$adm_pop_report, input$pop_denom, input$state_report, "REVCNT", whichTABLE = "table_suppress")
    if (nrow(df) > 1){
      raceethnicity$create_reactable(df)
    }
  })

  # conditional panel for tables
  output$showtablepanel <- reactive({ input$showtables })
  outputOptions(output, 'showtablepanel', suspendWhenHidden = FALSE)


  # conditional panel for infographics
  output$showinfogpanel <- reactive({
    dataavail <- rridata[[input$adm_pop_report]][[input$pop_denom]][[input$state_report]]$INFOGRAPH$DATAAVAIL
    ifelse(dataavail == 1, TRUE, FALSE)
  })
  outputOptions(output, 'showinfogpanel', suspendWhenHidden = FALSE)

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
  }) %>%
    bindCache(input$download_year,
              input$download_state,
              input$download_metric)

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
              style = list(fontFamily = "Graphik, sans-serif",
                           fontSize = "1.4rem"),
              theme = reactableTheme(cellStyle = list(display = "flex",
                                                      flexDirection = "column",
                                                      justifyContent = "center")),
              defaultColDef = colDef(format = colFormat(separators = TRUE),
                                     align = "left"),
              compact = TRUE,
              fullWidth = FALSE,
              defaultPageSize = 50,
              columns = list(
                state  = colDef(name = "State",
                                align = "left",
                                style = list(fontWeight = "bold"),
                                minWidth = 200),
                metric = colDef(name = "Metric",
                                minWidth = 370),
                year   = colDef(name = "Year",
                                minWidth = 110),
                total  = colDef(na = "–",
                                name = "Total",
                                align = "right",
                                minWidth = 110,
                                filterable = FALSE)))
  })

}
