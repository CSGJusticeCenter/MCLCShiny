
server <- function(input, output, session) {
  # Change URL depending on tab selection in navbar ############################
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


  # NATL TRENDS HEX MAP EXPLORER ###############################################
  
  # Natl Trends Hex Map --------------------------------------------------------
  
  # DETERMINE FILE NAME FOR DOWNLOADING MAP PNG 
  # Dynamically change name of map
  map_filename <- reactive({
    name <- svii_explorer |>
      filter(type     == input$adm_or_pop_map,
             metric   == input$data_map,
             year_chg == input$year_map) |>
      select(data, year_chg) |> distinct()
    name$year <- gsub(" - ", " ", name$year_chg)
    name <- paste(name$data, name$year_chg, sep = '_')
    name <- gsub(" ", "_", name)
  }) |>
    bindCache(input$data_map,
              input$adm_or_pop_map,
              input$year_map)

  
  # SELECT HEX MAP OBJECT 
  # Select foundational hex map and store it as a reactive expression
  # This allows the map to be downloaded after the map is changed
  # Charts were created in highchart.R
  foundational_map <- reactive({
    map <- natl_hex_lst[[input$adm_or_pop_map]][[input$year_map]][[input$data_map]]
    map |>
      highcharter::hc_add_dependency(name = "plugins/series-label.js") |>
      highcharter::hc_add_dependency(name = "plugins/accessibility.js") |>
      highcharter::hc_add_dependency(name = "plugins/exporting.js") |>
      highcharter::hc_add_dependency(name = "plugins/export-data.js") |>
      hc_boost(enabled = TRUE)}) |>
    bindCache(input$data_map,
              input$adm_or_pop_map,
              input$year_map)

  # Output hex map
  output$hex_map <- renderHighchart({
    foundational_map()
  })
  
  # DOWNLOAD HEX MAP PNG 
  # Save map
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

  # natl trends hex map table --------------------------------------------------

  # TITLE OF TABLE UNDER MAP 
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
    }) |>
    bindCache(input$data_map,
              input$adm_or_pop_map)


  # REACTABLE TABLE UNDER HEX MAP 
  output$table_map <- renderReactable({

    this_data <- paste0(input$data_map, " ",
                        input$adm_or_pop_map)
    this_yrchg <- input$year_map
    this_yrchg_name <- paste0(this_yrchg, " Change")
    this_trend_data <- paste0(
      "trend_data_", 
      substr(word(this_yrchg,  1), 3, 4), "_", 
      substr(word(this_yrchg, -1), 3, 4)
    )
    this_trend <- paste0(
      "trend_", 
      substr(word(this_yrchg,  1), 3, 4), "_", 
      substr(word(this_yrchg, -1), 3, 4)
    )
    
    df <- svii_explorer_table |>
      select(
        state = state_name, 
        data, 
        all_of(as.character(svii_yr$min_yr[1]:svii_yr$max_yr[1])),
        change     = all_of(this_yrchg), 
        trend_data = all_of(this_trend_data), 
        trend      = all_of(this_trend)
      ) |>
      filter(data == this_data) |>
      select(-data) |> 
      arrange(state) |>
      mutate(
        trend = case_when(
          trend == "negative" ~ regblue, 
          trend == "positive" ~ orange, 
          trend == "same"     ~ "#585858", 
          # w/o this the 'color' is NA and table won't render 
          # this is for cases where there is no trend (i.e. NA to NA) 
          is.na(trend)        ~ "white" 
        )
      )
    
    
    reactable(
      df,
      style = list(fontFamily = "Graphik, sans-serif", fontSize = "1.4rem"),
      theme = reactableTheme(
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
        headerStyle = list(textAlign = "right")
      ),
      compact = TRUE,
      fullWidth = FALSE, 
      searchable = TRUE,
      language = reactableLang(searchPlaceholder = "Search for Your State"),
      pagination = FALSE,
      defaultColDef = colDef(
        format = colFormat(separators = TRUE), 
        minWidth = 90, 
        align = "right", 
        na = "-"
      ),
      columns = list(
        state = colDef(
          name = "State",
          align = "left",
          minWidth = 130, #120 to fit states; 130 to have NH on single line
          style = list(fontWeight = "bold")
        ),
        change = colDef(
          minWidth = 110,
          name = this_yrchg_name,
          style = list(fontWeight = "bold"),
          format = colFormat(percent = TRUE, digits = 0)
        ),
        trend_data  = colDef(
          minWidth = 140,
          align = "center",
          name = "Trend Line",
          sortable = FALSE,
          cell = function(value, index) {
            points_list <- which(!is.na(value[[1]]))-1
            dui_sparkline(
              data = value[[1]],
              height = 60,
              components = list(
                dui_sparkpointseries(
                  points =  points_list,
                  stroke = df$trend[index],
                  fill = df$trend[index],
                  size = 2
                ),
                dui_sparklineseries(
                  curve = "linear",
                  showArea = FALSE,
                  fill = df$trend[index],
                  stroke = df$trend[index]
                )
              )
            )}
        ),
        trend = colDef(show = FALSE)
      )
    )
  }) |>
    bindCache(input$data_map,
              input$adm_or_pop_map,
              input$year_map)




  # STATE DASHBOARDS ###########################################################

  # 0 state title --------------------------------------------------------------
  # Title of state based on user input
  output$selected_state <- renderText({
    if (input$adm_pop_report == "Admissions") {
      paste("Prison Admission Trends in", input$state_report)
    } else if (input$adm_pop_report == "Population") {
      paste("Prison Population Trends in", input$state_report)
    } else {
      ""
    }
  }) |>
    bindCache(input$state_report,
              input$adm_pop_report)

  # 0 value boxes --------------------------------------------------------------
  # Filter data to totals
  df_vb_total <- reactive({
    svii_valbox |>
      filter(state_name == input$state_report &
               type == input$adm_pop_report &
               year == svii_yr$max_yr[1] &
               metric == "Total")
  }) |>
    bindCache(input$state_report,
              input$adm_pop_report)

  # Filter data to sup violations
  df_vb_sup_violations <- reactive({
    svii_valbox |>
      filter(state_name == input$state_report &
               type == input$adm_pop_report &
               year == svii_yr$max_yr[1] &
               metric == "Supervision Violation")
  }) |>
    bindCache(input$state_report,
              input$adm_pop_report)

  # Filter data to tech violations
  df_vb_tech <- reactive({
    svii_valbox |>
      filter(state_name == input$state_report &
               type == input$adm_pop_report &
               year == svii_yr$max_yr[1] &
               metric == "Technical Violation")
  }) |>
    bindCache(input$state_report,
              input$adm_pop_report)

  # Filter data to new offense violations
  df_vb_new_off <- reactive({
    svii_valbox |>
      filter(state_name == input$state_report &
               type == input$adm_pop_report &
               year == svii_yr$max_yr[1] &
               metric == "New Offense Violation")
  }) |>
    bindCache(input$state_report,
              input$adm_pop_report)

  # Value box for change in total admissions or population
  output$total_change <- renderValueBox({

    # No subtitle needed regarding no parole or prob data
    fnc_value_box(
      title      = paste0("Overall "),
      adm_or_pop = paste0(input$adm_pop_report, " in ", svii_yr$max_yr[1]),
      subtitle   = HTML("<br>"),
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
      adm_or_pop = paste0(input$adm_pop_report, " in ", svii_yr$max_yr[1]),
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
      adm_or_pop = paste0(input$adm_pop_report, " in ", svii_yr$max_yr[1]),
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
      adm_or_pop = paste0(input$adm_pop_report, " in ", svii_yr$max_yr[1]),
      subtitle   = df_vb_new_off()$subheader,
      value      = df_vb_new_off()$value_shown,
      finding    = df_vb_new_off()$text,
      color      = "black",
      href       = NULL,
      width      = 5
    )
  })

  # 1 OVERVIEW -----------------------------------------------------------------
  
  # 1 AREA CHART ---------------------------------------------------------------
  # Select highchart depending on selector input
  # Charts were saved in highchart.R
  output$state_area_chart <- renderHighchart({
    state_area_lst[[input$adm_pop_report]][[input$state_report]] |>
      highcharter::hc_add_dependency(name = "plugins/series-label.js") |>
      highcharter::hc_add_dependency(name = "plugins/accessibility.js") |>
      highcharter::hc_add_dependency(name = "plugins/exporting.js") |>
      highcharter::hc_add_dependency(name = "plugins/export-data.js") |>
      hc_boost(enabled = TRUE)
    }) |>
    bindCache(input$state_report,
              input$adm_pop_report)
  
  # Download button for state area chart
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

  # Show download button if data is available
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
    }) |>
    bindCache(input$state_report,
              input$adm_pop_report)

  # Show plot if data is available
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
    }) |>
    bindCache(input$state_report,
              input$adm_pop_report)

  # 1 BAR CHART (supervision/both)----------------------------------------------
  # Select highchart depending on selector input
  # Charts were saved in highchart.R
  output$state_bar_chart <- renderHighchart({
    state_bar_lst[[input$adm_pop_report]][["Both"]][[input$state_report]] |>
      highcharter::hc_add_dependency(name = "plugins/series-label.js") |>
      highcharter::hc_add_dependency(name = "plugins/accessibility.js") |>
      highcharter::hc_add_dependency(name = "plugins/exporting.js") |>
      highcharter::hc_add_dependency(name = "plugins/export-data.js") |>
      hc_boost(enabled = TRUE)
    }) |>
    bindCache(input$state_report,
              input$adm_pop_report)
  
  # BAR CHART DOWNLOAD 
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


  # 1 BAR CHART change between graph/sentence ---------------------------------
  # Supervision Violations Graph - Dynamically change between sentence and graph depending on data availability
  # Show "Data Unavailable", "Did Not Respond" or "Partial Data Submitted" or chart if required data is available

  # If data is missing a supervision violation
  output$missing_data_nt <- renderUI({
    out <- missingness_sentences |>
      filter(state == input$state_report) |> 
      rename(thiscol = paste0("supervision_violation_", tolower(input$adm_pop_report), "_graph"))
    out <- out$thiscol
    HTML(out)
  }) |>
    bindCache(input$state_report, 
              input$adm_pop_report)

  # Remove download button if no graph
  output$missing_data_nt_button <- renderText({
    ""
  })

  # Show graph or missing data sentence depending on state
  output$state_nt = renderUI({
    
    miss_text <- missingness_sentences |> 
     filter(state == input$state_report) |> 
     pull(paste0("supervision_violation_", tolower(input$adm_pop_report), "_graph"))
    
    if (is.na(miss_text)){
      # if 'miss_text' == NA --> data to plot 
      highchartOutput("state_bar_chart", height = 400, width = 390)
    } else {
      # if 'miss_text' != NA --> show sentence instead 
      htmlOutput("missing_data_nt")
    }
    
    }) |>
    bindCache(input$state_report,
              input$adm_pop_report)

  # Show graph download button or no button depending on state
  output$state_nt_button = renderUI({
    
    downloadButton(outputId = 'save_state_bar_chart', "",
                   class = "download-chart")
    
    miss_text <- missingness_sentences |> 
      filter(state == input$state_report) |> 
      pull(paste0("supervision_violation_", tolower(input$adm_pop_report), "_graph"))
    
    if (is.na(miss_text)){
      # if 'miss_text' == NA --> data to plot --> ADD DOWNLOAD BUTTON  
      downloadButton(outputId = 'save_state_bar_chart', "", class = "download-chart")
    } else {
      # if 'miss_text' != NA --> show sentence instead --> NO DOWNLOAD BUTTON 
      textOutput("missing_data_nt_button")
    }
    
    }) |>
    bindCache(input$state_report,
              input$adm_pop_report)


  # 1 TABLE (overview) ---------------------------------------------------------
  # State table
  output$state_table <- renderReactable({

    # Filter data
    df <- svii_table |>
      filter(state_name == input$state_report &
             type       == input$adm_pop_report) |>
      arrange(text) |>
      select(
        text, 
        all_of(as.character(svii_yr$min_yr[1]:svii_yr$max_yr[1])), 
        `2018 - 2023`, 
        trend_data_18_23
      )
    
    state_reactable(df)
    
    }) |>
    bindCache(input$state_report,
              input$adm_pop_report)

  # 1 STATE NOTES --------------------------------------------------------------

  # Filter data
  df_parole_asterisks_notes <- reactive({
    formatted_notes |> 
      filter(state == input$state_report) |> 
      select(state, notes = parole_asterisks)
  }) |>
    bindCache(input$state_report)

  # Parole asterisks state notes
  output$state_parole_asterisks_notes <- renderUI({
    HTML(df_parole_asterisks_notes()$notes)
  })

  # Filter data
  df_probation_asterisks_notes <- reactive({
    formatted_notes |> 
      filter(state == input$state_report) |> 
      select(state, notes = probation_asterisks)
  }) |>
    bindCache(input$state_report)

  # Probation asterisks state notes
  output$state_probation_asterisks_notes <- renderUI({
    HTML(df_probation_asterisks_notes()$notes)
  })

  # Filter parole data
  df_parole_notes <- reactive({
    formatted_notes |> 
      filter(state == input$state_report) |> 
      select(state, notes = parole_metrics)
  }) |>
    bindCache(input$state_report)

  # Parole state notes
  output$state_parole_notes <- renderUI({
    HTML(df_parole_notes()$notes)
  })

  # Filter probation data
  df_probation_notes <- reactive({
    formatted_notes |> 
      filter(state == input$state_report) |> 
      select(state, notes = probation_metrics)
  }) |>
    bindCache(input$state_report)

  # Probation state notes
  output$state_probation_notes <- renderUI({
    HTML(df_probation_notes()$notes)
  })

  # Filter data
  df_additional_notes <- reactive({
    formatted_notes |> 
      filter(state == input$state_report) |> 
      select(state, notes = additional_notes)
  }) |>
    bindCache(input$state_report)

  # Additional state notes
  output$state_additional_notes <- renderUI({
    HTML(df_additional_notes()$notes)
  })


  # 2 PAROLE -------------------------------------------------------------------

  # 2 BAR CHART (parole) -------------------------------------------------------
  # Select highchart depending on selector input
  # Charts were saved in highchart.R
  output$parole_bar_chart <- renderHighchart({
    state_bar_lst[[input$adm_pop_report]][["Parole"]][[input$state_report]] |>
      highcharter::hc_add_dependency(name = "plugins/series-label.js") |>
      highcharter::hc_add_dependency(name = "plugins/accessibility.js") |>
      highcharter::hc_add_dependency(name = "plugins/exporting.js") |>
      highcharter::hc_add_dependency(name = "plugins/export-data.js") |>
      hc_boost(enabled = TRUE)
  }) |>
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

  # 2 table (parole) -----------------------------------------------------------
  output$parole_table <- renderReactable({

    # Filter data
    df <- svii_par |>
      filter(state_name == input$state_report &
               type       == input$adm_pop_report) |>
      arrange(text) |>
      select(
        text, 
        all_of(as.character(svii_yr$min_yr[1]:svii_yr$max_yr[1])), 
        `2018 - 2023`, 
        trend_data_18_23
      )
    state_reactable(
      df, 
      these_col_fill = colpal_fill[2:4], 
      these_col_stroke = colpal_stroke[2:4]
    )
    
    }) |>
    bindCache(input$state_report,
              input$adm_pop_report)

  # 2 BAR CHART change between graph/sentence ----------------------------------
  # Parole Graph - Dynamically change between sentence and graph depending on data availability
  # Show "Data Unavailable", "Did Not Respond" or "Partial Data Submitted" or chart if required data is available

  # If data is missing a parole violation admissions graph
  output$missing_data_parole_nt <- renderUI({
    out <- missingness_sentences |>
      filter(state == input$state_report) |> 
      rename(thiscol = paste0("parole_violation_", tolower(input$adm_pop_report), "_graph"))
    out <- out$thiscol
    HTML(out)
  }) |>
    bindCache(input$state_report, 
              input$adm_pop_report)

  # Show parole graph or missing data sentence depending on state
  output$parole_nt <- renderUI({
    
    miss_text <- missingness_sentences |> 
      filter(state == input$state_report) |> 
      pull(paste0("parole_violation_", tolower(input$adm_pop_report), "_graph"))
    
    if (is.na(miss_text)){
      # if 'miss_text' == NA --> data to plot
      fluidRow(
        column(width = 3),
        column(
          width = 5,
          align = "center",
          highchartOutput("parole_bar_chart", height = 400, width = 390)
        ),
        column(
          width = 1,
          align = "left",
          downloadButton(outputId = 'save_parole_bar_chart', "", class = "download-chart")
        ),
        column(width = 3)
      )
    } else {
      # if 'miss_text' != NA --> show sentence instead
      fluidRow(
        column(width = 3),
        column(width = 6, align = "center", htmlOutput("missing_data_parole_nt")),
        column(width = 3)
      )
    }
    
    }) |>
    bindCache(input$state_report,
              input$adm_pop_report)


  # 3 PROBATION ----------------------------------------------------------------
  
  # 3 BAR CHART (probation) ------------------------------------------------------
  # Select highchart depending on selector input
  # Charts were saved in highchart.R
  output$probation_bar_chart <- renderHighchart({
    state_bar_lst[[input$adm_pop_report]][["Probation"]][[input$state_report]] |>
      highcharter::hc_add_dependency(name = "plugins/series-label.js") |>
      highcharter::hc_add_dependency(name = "plugins/accessibility.js") |>
      highcharter::hc_add_dependency(name = "plugins/exporting.js") |>
      highcharter::hc_add_dependency(name = "plugins/export-data.js") |>
      hc_boost(enabled = TRUE)
    })  |>
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

  # 3 table (Probation) --------------------------------------------------------
  output$probation_table <- renderReactable({

    # Filter data
    df <- svii_prob |>
      filter(state_name == input$state_report &
               type       == input$adm_pop_report) |>
      arrange(text) |>
      select(
        text, 
        all_of(as.character(svii_yr$min_yr[1]:svii_yr$max_yr[1])), 
        `2018 - 2023`, 
        trend_data_18_23
      )
    
    state_reactable(
      df, 
      these_col_fill = colpal_fill[2:4], 
      these_col_stroke = colpal_stroke[2:4]
    )
    
    }) |>
    bindCache(input$state_report,
              input$adm_pop_report)

  # 3 BAR CHART change between graph/sentence ----------------------------------
  # Probation Graph - Dynamically change between sentence and graph depending on data availability
  # Show "Data Unavailable", "Did Not Respond" or "Partial Data Submitted" or chart if required data is available

  # If data is missing a probation violation admissions graph
  output$missing_data_probation_nt <- renderUI({
    out <- missingness_sentences |>
      filter(state == input$state_report) |> 
      rename(thiscol = paste0("probation_violation_", tolower(input$adm_pop_report), "_graph"))
    out <- out$thiscol
    HTML(out)
  }) |>
    bindCache(input$state_report, 
              input$adm_pop_report)

  # Show probation graph or missing data sentence depending on state
  output$probation_nt <- renderUI({

    miss_text <- missingness_sentences |> 
      filter(state == input$state_report) |> 
      pull(paste0("probation_violation_", tolower(input$adm_pop_report), "_graph"))
    
    if (is.na(miss_text)){
      # if 'miss_text' == NA --> data to plot
      fluidRow(
        column(width = 3),
        column(
          width = 5,
          align = "center",
          highchartOutput("probation_bar_chart", height = 400, width = 390)
        ),
        column(
          width = 1,
          align = "left",
          downloadButton(outputId = 'save_probation_bar_chart', "", class = "download-chart")
        ),
        column(width = 3)
      )
    } else {
      # if 'miss_text' != NA --> show sentence instead
      fluidRow(
        column(width = 3),
        column(width = 6, align = "center", htmlOutput("missing_data_probation_nt")),
        column(width = 3)
      )
    }
    
    
  }) |>
    bindCache(input$state_report,
              input$adm_pop_report)


  # DOWNLOAD DATA ##############################################################

  # Select multiple years
  filteredYears <- reactive({
    unique(svii_download$year)
  })
  observeEvent(filteredYears(), {
    updatePickerInput(session, inputId = 'download_year',
                      label = 'Select Year(s)',
                      choices = filteredYears(),
                      selected = filteredYears())
  })

  # Select multiple state
  filteredState <- reactive({
    unique(svii_download$state)
  })
  observeEvent(filteredState(), {
    updatePickerInput(session, inputId = 'download_state',
                      label = 'Select State(s)',
                      choices = filteredState(),
                      selected = filteredState())
  })

  # Select multiple metrics
  filteredMetric <- reactive({
    unique(svii_download$metric)
  })
  observeEvent(filteredMetric(), {
    updatePickerInput(session, inputId = 'download_metric',
                      label = 'Select Metric(s)',
                      choices = filteredMetric(),
                      selected = filteredMetric())
  })

  # Filter data depending on user input
  df_download_table <- reactive({
    svii_download |>
      filter(year %in% input$download_year) |>
      filter(state %in% input$download_state) |>
      filter(metric %in% input$download_metric) |>
      arrange(state, year)
  }) |>
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

    df1 <- df_download_table() |>
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

} # END SERVER 
