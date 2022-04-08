server <- function(input, output, session) {

  ################################################################################
  ################################################################################
  # Map Explorer
  ################################################################################
  ################################################################################

  df_map <- reactive({
    mclc_explorer %>%
      filter(adm_or_pop == input$adm_or_pop_map_counts,
             metric     == input$data_map_counts,
             year       == input$year_map_counts)
  })

  df_map_table <- reactive({
    filter_by <- paste0(input$data_map_counts, " ", input$adm_or_pop_map_counts)
    mclc_explorer_table %>%
      filter(data == filter_by) %>%
      arrange(state)
  })

  ##############
  # Hex map title
  ##############

  # Title of map
  output$selected_map <- renderText({

    paste("Change in ", input$data_map_counts, " ", input$adm_or_pop_map_counts, " from ", input$year_map_counts)

  })

  ##############
  # Table below map changes depending on count vs change
  ##############

  output$table_map_counts <- renderReactable(
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
                data          = colDef(name = "Data",
                                       minWidth = 140),
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

  ################################################################################
  ################################################################################
  #  Map
  ################################################################################
  ################################################################################

  reactive_map <- reactive({

    combined_new <- merge(combined, df_map(), by.x = "name.x", by.y = "state")
    combined_labels_new <- merge(combined_labels, df_map(), by.x = "name.x", by.y = "state")

    NA_color <- "grey80"

    ggplot(combined_new) +
      geom_sf(aes(fill = total),     color = NA       ) +  #color (non-NA) hex
      geom_sf(    fill = NA,          aes(color = "NA")    ) +  #dummy legend for NA values
      geom_sf(    fill = NA,              color = "grey50" ) +  #hex borders
      geom_sf_text(
        data=mutate(combined_labels_new, geometry=geometry+c(0, 5))
        , aes(label=abb_usps)
        , fontface="bold"
        , size=5
      ) +
      geom_sf_text(
        data=mutate(combined_labels_new, geometry=geometry+c(0,-5))
        , aes(label=scales::percent(total, accuracy=0.1))
        , size=4
      ) +
      scale_fill_gradient2(
        name = "Change"
        , low  = "#65ace1"
        , mid  = "#ffffff"
        , high = "#ee7600"
        , midpoint = 0
        , labels = scales::percent
        , na.value = NA_color
      ) +
      scale_color_manual( #dummy legend for NA color
        name = NULL
        , values = NA_color
        , labels = 'No data'
      ) +
      guides(
        fill  = guide_colorbar(order = 1)
        , color = guide_legend(override.aes = list(fill = NA_color))
      ) +
      theme_void()+
      theme(legend.title=element_text(size=14),
            legend.text=element_text(size=14))
  })

  # output reactive leaflet map
  output$reactive_map <- renderPlot({
    reactive_map()
  })

  ##############
  # Download data and map options
  ##############

  # download button for map
  output$save_map <- downloadHandler(
    filename = function(){
      paste(input$data_map_counts, "_", input$adm_or_pop_map_counts, "_Change_", input$year_map_counts, '.png', sep = '')
    },
    content = function(filename){
      req(reactive_map())
      ggsave(filename, plot = reactive_map(), device = 'png', width=11, height=8.5)
    }
  )

  # download button for data
  output$save_data <- downloadHandler(
    filename = function() {
      paste0(input$data_map_counts, "_", input$adm_or_pop_map_counts, "_", input$year_map_counts, ".xlsx", sep="")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, sheetName = "sheet1")
      writeData(wb, sheet = 1, x = df_map_table(), startCol = 1, startRow = 1)
      saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )

  ################################################################################
  ################################################################################
  # State Reports
  ################################################################################
  ################################################################################

  # Print state name depending on state selected
  output$selected_state <- renderText({
    paste("Trends in ", input$state)
  })

  ##############
  # Value boxes
  ##############

  # filter data to totals
  df_vb_total <- reactive({
    vb_adm_pop %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop &
               year == "2020" &
               metric == "Total")
  })

  # filter data to sup viols
  df_vb_sup_viols <- reactive({
    vb_adm_pop %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop &
               year == "2020" &
               metric == "Supervision Violations")
  })

  # filter data to tech viols
  df_vb_tech <- reactive({
    vb_adm_pop %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop &
               year == "2020" &
               metric == "Technical")
  })

  # filter data to state and rev rate
  df_bjs_rate <- reactive({
    bjs_prob_parole %>%
      filter(state == input$state)
  })

  output$total_change <- renderValueBox({

    valueBox2(
      comma(df_vb_total()$total, digits = 0),
      title = paste0(input$adm_or_pop, " in 2020"),
      subtitle = tagList(HTML("&darr;"), paste0(df_vb_total()$change, "% from 2019")),
      color = "black",
      href = NULL
    )

  })

  output$sup_change <- renderValueBox({

    valueBox2(
      comma(df_vb_sup_viols()$total, digits = 0),
      title = paste0("Violation ", input$adm_or_pop, " in 2020"),
      subtitle = tagList(HTML("&darr;"), paste0(df_vb_sup_viols()$change, "% from 2019")),
      color = "black",
      href = NULL
    )

  })

  output$tech_change <- renderValueBox({

    valueBox2(
      comma(df_vb_tech()$total, digits = 0),
      title = paste0("Technical ", input$adm_or_pop, " in 2020"),
      subtitle = tagList(HTML("&darr;"), paste0(df_vb_tech()$change, "% from 2019")),
      color = "black",
      href = NULL
    )

  })

  output$rev_rate <- renderValueBox({

    valueBox2(
      paste0(round(df_bjs_rate()$rev_rate_20*100, 2), "%"),
      title = "Revovation Rate in 2020",
      subtitle = tagList(HTML("&darr;"), paste0(round(df_bjs_rate()$rev_rate_change*100, 0), "% from 2019")),
      color = "black",
      href = NULL
    )

  })

  ##############
  # Overall area plot
  ##############

  output$totals_chart <- renderPlotly({

    # filter data
    df <-
      adm_pop_long %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop &
               (metric == "Total" | metric == "Supervision Violations" | metric == "Technical")) %>%
      group_by(state, year, metric) %>%
      summarise(total = sum(total))
    data_total <- df %>% filter(metric == "Total")
    data_supviols <- df %>% filter(metric == "Supervision Violations")
    data_technical <- df %>% filter(metric == "Technical")

    # area plot
    plot_ly() %>%
      # total
      add_trace(x = data_total$year,
                y = data_total$total,
                name = "Total",
                split = data_total$metric,
                type = 'scatter',
                mode = 'none',
                fill = 'tozeroy',
                fillcolor = total_co,
                hoverinfo = 'text',
                hovertext = paste('<b>',data_total$metric, '</b><br><br>',
                                  'Year: ', data_total$year, '<br>',
                                  'Total: ', formattable::comma(data_total$total, digits = 0),'<br>')) %>%
      # supervision violations
      add_trace(x = data_supviols$year,
                y = data_supviols$total,
                name = "Supervision Violations",
                split = data_supviols$metric,
                type = 'scatter',
                mode = 'none',
                fill = 'tozeroy',
                fillcolor = viol_co,
                hoverinfo = 'text',
                text = paste('<b>',data_supviols$metric, '</b><br><br>',
                             'Year: ', data_supviols$year, '<br>',
                             'Total: ', formattable::comma(data_supviols$total, digits = 0),'<br>')) %>%
      # technical
      add_trace(x = data_technical$year,
                y = data_technical$total,
                name = "Technical",
                split = data_technical$metric,
                type = 'scatter',
                mode = 'none',
                fill = 'tozeroy',
                fillcolor = tech_co,
                hoverinfo = 'text',
                hovertext = paste('<b>',data_technical$metric, '</b><br><br>',
                                  'Year: ', data_technical$year, '<br>',
                                  'Total: ', formattable::comma(data_technical$total, digits = 0),'<br>')) %>%
      # customize layout
      layout(title = list(text = paste0('<b>Prison ', input$adm_or_pop, '</b>\n'), font = list(size = 14)),
             font = list(size = 12),
             plot_bgcolor='#FFFFFF',
             xaxis = list(
               title = "",
               showline= T,
               linewidth=2,
               linecolor='black',
               gridcolor = 'FFFFFF'),
             yaxis = list(
               title = "",
               showticklabels = TRUE,
               tickformat=",d",
               gridcolor = 'FFFFFF'
             ),
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5, y = -0.1)) %>%
      # remove plotly buttons
      config(
        modeBarButtonsToRemove = list(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
          "resetScale2d", "zoom3d", "pan3d",
          "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "orbitRotation",
          "tableRotation", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
          "sendDataToCloud", "hoverClosestGl2d", "hoverClosestPie", "toggleHover",
          "resetViews", "toggleSpikelines", "resetViewMapbox"
        ), displaylogo = FALSE) %>%
      # customize file name
      config(plot_ly(),
             toImageButtonOptions= list(filename = paste0(input$state, "_", input$adm_or_pop)))
  })

  ########
  # Supervision violations by type chart
  ########

  output$sup_viols_type_chart <- renderPlotly({

    state <- input$state
    adm_or_pop <- input$adm_or_pop

    # filter data
    df <-
      adm_pop_long %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop) %>%
      group_by(metric, year) %>%
      summarise(total = sum(total)) %>%
      filter(metric == "New Offense" | metric == "Technical")

    # bar chart of supervision violations by type
    df %>%
      plot_ly(
        type = 'bar',
        x = ~year,
        y = ~total,
        color = ~metric,
        colors = c(`New Offense` = new_o_co,
                   Technical = tech_co),
        hoverinfo = 'text',
        hovertext = paste('<b>',df$metric, '</b><br><br>',
                          'Year: ', df$year, '<br>',
                          'Total: ', formattable::comma(df$total, digits = 0),'<br>')) %>%
      # customize layout
      layout(title = list(text = paste0('<b>Supervision Violation ', input$adm_or_pop, ' by Type</b>\n'), font = list(size = 14)),
             font = list(size = 12),
             plot_bgcolor='#FFFFFF',
             xaxis = list(
               title = "",
               showline= T, linewidth=2, linecolor='black',
               gridcolor = 'FFFFFF'),
             yaxis = list(
               title = "",
               showticklabels = TRUE,
               tickformat=",d"
             ),
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5, y = -0.1)) %>%
      # remove plotly buttons
      config(
        modeBarButtonsToRemove = list(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
          "resetScale2d", "zoom3d", "pan3d",
          "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "orbitRotation",
          "tableRotation", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
          "sendDataToCloud", "hoverClosestGl2d", "hoverClosestPie", "toggleHover",
          "resetViews", "toggleSpikelines", "resetViewMapbox"
        ), displaylogo = FALSE) %>%
      # customize file name
      config(plot_ly(),
             toImageButtonOptions= list(filename = paste0(state, "_Supervision_Violation_", adm_or_pop)))

  })

  ########
  # State table under graphs
  ########

  output$state_table <- renderReactable({

    # filter data
    df <- state_table %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop) %>%
      group_by(text) %>%
      summarise(total_new = list(list(total)))
    df1 <- state_table_wide %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop) %>%
      arrange(order) %>%
      select(-adm_or_pop, -state)

    # merge data
    df <- merge(df1, df, by = "text")
    df <- df %>% arrange(order) %>% select(-order)

    # choose colors
    colpal_fill <- c("url(#total)",
                     "url(#sup_viols)",
                     "url(#technical)",
                     "url(#new_offense)")
    colpal_stroke <- c(total_co, viol_co , tech_co, new_o_co)

    # create table with 3 year trend line in last column
    reactable(df,
              theme = reactableTheme(
                # Vertically center cells
                cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE),
                align = "center"),
              compact = TRUE,
              fullWidth = FALSE,
              columns = list(
                text            = colDef(name = "Metric",
                                         align = "left",
                                         minWidth = 250),
                `2018`          = colDef(minWidth = 75),
                `2019`          = colDef(minWidth = 75),
                `2020`          = colDef(minWidth = 75),
                three_yr_change = colDef(name = "3 Yr Change",
                                         format = colFormat(percent = TRUE, digits = 1)),
                # add 3 year trend graphs to each row
                total_new  = colDef(name = "3 Yr Trend",
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
                                            strokeWidth = 1,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "sup_viols",
                                            height = 4,
                                            width = 4,
                                            stroke = viol_co,
                                            strokeWidth = 1,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "technical",
                                            height = 4,
                                            width = 4,
                                            stroke = tech_co,
                                            strokeWidth = 1,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "new_offense",
                                            height = 4,
                                            width = 4,
                                            stroke = new_o_co,
                                            strokeWidth = 1,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparklineseries(
                                            curve = "linear",
                                            showArea = TRUE,
                                            fill = colpal_fill[index],
                                            stroke = colpal_stroke[index])))})))
  })

  ##################################
  # Parole and probation charts
  ##################################

  ########
  # Parole area plot
  ########

  output$areachart_parole <- renderPlotly({

    # filter data
    df <-
      adm_pop_long %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop &
               prob_vs_parole == "Parole" &
               metric != "New Offense") %>%
      group_by(state, year, metric) %>%
      summarise(total = sum(total))
    df$year <- as.factor(df$year)
    data_total <- df %>% filter(metric == "Parole")
    data_technical <- df %>% filter(metric == "Technical")

    # area plot
    plot_ly() %>%
      # total
      add_trace(x = data_total$year,
                y = data_total$total,
                name = "Total",
                split = data_total$metric,
                type = 'scatter',
                mode = 'none',
                fill = 'tozeroy',
                fillcolor = pp_co,
                hoverinfo = 'text',
                hovertext = paste('<b>',data_total$metric, '</b><br><br>',
                                  'Year: ', data_total$year, '<br>',
                                  'Total: ', formattable::comma(data_total$total, digits = 0),'<br>')) %>%
      # technical
      add_trace(x = data_technical$year,
                y = data_technical$total,
                name = "Technical",
                split = data_technical$metric,
                type = 'scatter',
                mode = 'none',
                fill = 'tozeroy',
                fillcolor = tech_co,
                hoverinfo = 'text',
                hovertext = paste('<b>',data_technical$metric, '</b><br><br>',
                                  'Year: ', data_technical$year, '<br>',
                                  'Total: ', formattable::comma(data_technical$total, digits = 0),'<br>')) %>%
      # customize layout
      layout(title = list(text = paste0('<b>Parole ', input$adm_or_pop, '</b>\n'), font = list(size = 14)),
             font = list(size = 12),
             plot_bgcolor='#FFFFFF',
             xaxis = list(
               title = "",
               showline= T,
               linewidth=2,
               linecolor='black',
               gridcolor = 'FFFFFF'),
             yaxis = list(
               title = "",
               showticklabels = TRUE,
               tickformat=",d",
               gridcolor = 'FFFFFF'
             ),
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5, y = -0.1)) %>%
      # remove plotly buttons
      config(
        modeBarButtonsToRemove = list(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
          "resetScale2d", "zoom3d", "pan3d",
          "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "orbitRotation",
          "tableRotation", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
          "sendDataToCloud", "hoverClosestGl2d", "hoverClosestPie", "toggleHover",
          "resetViews", "toggleSpikelines", "resetViewMapbox"
        ), displaylogo = FALSE) %>%
      # customize file name
      config(plot_ly(),
             toImageButtonOptions= list(filename = paste0(input$state, "_Parole_", input$adm_or_pop)))

  })

  ########
  # Parole bar chart
  ########

  output$barchart_parole <- renderPlotly({

    state <- input$state
    adm_or_pop <- input$adm_or_pop

    # filter data
    df <-
      adm_pop_long %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop &
               prob_vs_parole == "Parole") %>%
      filter(metric == "Technical" | metric == "New Offense")  %>%
      group_by(metric, year) %>%
      summarise(total = sum(total))

    # bar chart of parole violations by type
    df %>%
      plot_ly(
        type = 'bar',
        x = ~year,
        y = ~total,
        color = ~metric,
        colors = c(`New Offense` = new_o_co,
                   Technical = tech_co),
        hoverinfo = 'text',
        hovertext = paste('<b>', df$metric, '</b><br><br>',
                          'Year: ', df$year, '<br>',
                          'Total: ', formattable::comma(df$total, digits = 0),'<br>')) %>%
      # customize layout
      layout(title = list(text = paste0('<b>Parole ', input$adm_or_pop, ' by Type</b>\n'), font = list(size = 14)),
             font = list(size = 12),
             plot_bgcolor='#FFFFFF',
             xaxis = list(
               title = "",
               showline= T, linewidth=2, linecolor='black',
               gridcolor = 'FFFFFF'),
             yaxis = list(
               title = "",
               showticklabels = TRUE,
               tickformat=",d"
             ),
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5, y = -0.1)) %>%
      # remove plotly buttons
      config(
        modeBarButtonsToRemove = list(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
          "resetScale2d", "zoom3d", "pan3d",
          "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "orbitRotation",
          "tableRotation", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
          "sendDataToCloud", "hoverClosestGl2d", "hoverClosestPie", "toggleHover",
          "resetViews", "toggleSpikelines", "resetViewMapbox"
        ), displaylogo = FALSE) %>%
      # customize file name
      config(plot_ly(),
             toImageButtonOptions= list(filename = paste0(state, "_Parole_Type_", adm_or_pop)))

  })

  ########
  # Probation area plot
  ########

  output$areachart_prob <- renderPlotly({
    # filter data
    df <-
      adm_pop_long %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop &
               prob_vs_parole == "Probation" &
               metric != "New Offense") %>%
      group_by(state, year, metric) %>%
      summarise(total = sum(total))
    df$year <- as.factor(df$year)
    data_total <- df %>% filter(metric == "Probation")
    data_technical <- df %>% filter(metric == "Technical")

    # area plot
    plot_ly() %>%
      # total
      add_trace(x = data_total$year,
                y = data_total$total,
                name = "Total",
                split = data_total$metric,
                type = 'scatter',
                mode = 'none',
                fill = 'tozeroy',
                fillcolor = pp_co,
                hoverinfo = 'text',
                hovertext = paste('<b>',data_total$metric, '</b><br><br>',
                                  'Year: ', data_total$year, '<br>',
                                  'Total: ', formattable::comma(data_total$total, digits = 0),'<br>')) %>%
      # technical
      add_trace(x = data_technical$year,
                y = data_technical$total,
                name = "Technical",
                split = data_technical$metric,
                type = 'scatter',
                mode = 'none',
                fill = 'tozeroy',
                fillcolor = tech_co,
                hoverinfo = 'text',
                hovertext = paste('<b>',data_technical$metric, '</b><br><br>',
                                  'Year: ', data_technical$year, '<br>',
                                  'Total: ', formattable::comma(data_technical$total, digits = 0),'<br>')) %>%
      # customize layout
      layout(title = list(text = paste0('<b>Probation ', input$adm_or_pop, '</b>\n'), font = list(size = 14)),
             font = list(size = 12),
             plot_bgcolor='#FFFFFF',
             xaxis = list(
               title = "",
               showline= T,
               linewidth=2,
               linecolor='black',
               gridcolor = 'FFFFFF'),
             yaxis = list(
               title = "",
               showticklabels = TRUE,
               tickformat=",d",
               gridcolor = 'FFFFFF'
             ),
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5, y = -0.1)) %>%
      # remove plotly buttons
      config(
        modeBarButtonsToRemove = list(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
          "resetScale2d", "zoom3d", "pan3d",
          "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "orbitRotation",
          "tableRotation", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
          "sendDataToCloud", "hoverClosestGl2d", "hoverClosestPie", "toggleHover",
          "resetViews", "toggleSpikelines", "resetViewMapbox"
        ), displaylogo = FALSE) %>%
      # customize file name
      config(plot_ly(),
             toImageButtonOptions= list(filename = paste0(input$state, "_Probation_", input$adm_or_pop)))
  })

  ########
  # Probation bar chart
  ########

  output$barchart_prob <- renderPlotly({

    state <- input$state
    adm_or_pop <- input$adm_or_pop

    # filter data
    df <-
      adm_pop_long %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop &
               prob_vs_parole == "Probation") %>%
      filter(metric == "Technical" | metric == "New Offense")  %>%
      group_by(metric, year) %>%
      summarise(total = sum(total))

    # bar chart of probation violations by type
    df %>%
      plot_ly(
        type = 'bar',
        x = ~year,
        y = ~total,
        color = ~metric,
        colors = c(`New Offense` = new_o_co,
                   Technical = tech_co),
        hoverinfo = 'text',
        hovertext = paste('<b>',df$metric, '</b><br><br>',
                          'Year: ', df$year, '<br>',
                          'Total: ', formattable::comma(df$total, digits = 0),'<br>')) %>%
      # customize layout
      layout(title = list(text = paste0('<b>Probation ', input$adm_or_pop, ' by Type</b>\n'), font = list(size = 14)),
             font = list(size = 12),
             plot_bgcolor='#FFFFFF',
             xaxis = list(
               title = "",
               showline= T, linewidth=2, linecolor='black',
               gridcolor = 'FFFFFF'),
             yaxis = list(
               title = "",
               showticklabels = TRUE,
               tickformat=",d"
             ),
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5, y = -0.1)) %>%
      # remove plotly buttons
      config(
        modeBarButtonsToRemove = list(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
          "resetScale2d", "zoom3d", "pan3d",
          "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "orbitRotation",
          "tableRotation", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
          "sendDataToCloud", "hoverClosestGl2d", "hoverClosestPie", "toggleHover",
          "resetViews", "toggleSpikelines", "resetViewMapbox"
        ), displaylogo = FALSE) %>%
      # customize file name
      config(plot_ly(),
             toImageButtonOptions= list(filename = paste0(state, "_Probation_Type_", adm_or_pop)))

  })

  ##################################
  # Parole and probation tables under graphs
  ##################################

  ########
  # Parole table under graphs
  ########

  output$parole_table <- renderReactable({

    # filter data
    df <- parole_table %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop) %>%
      group_by(text) %>%
      summarise(total_new = list(list(total)))
    df1 <- parole_table_wide %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop) %>%
      arrange(order) %>%
      select(-adm_or_pop, -state, -prob_vs_parole)

    # merge data
    df <- merge(df1, df, by = "text")
    df <- df %>% arrange(order) %>% select(-order)

    # choose colors
    colpal_fill <- c("url(#total)",
                     "url(#technical)",
                     "url(#new_offense)")
    colpal_stroke <- c(pp_co, tech_co, new_o_co)

    # create table with 3 year trend line in last column
    reactable(df,
              theme = reactableTheme(
                # Vertically center cells
                cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE),
                align = "center"),
              compact = TRUE,
              fullWidth = FALSE,
              columns = list(
                text            = colDef(name = "Metric",
                                         align = "left",
                                         minWidth = 250),
                `2018`          = colDef(minWidth = 75),
                `2019`          = colDef(minWidth = 75),
                `2020`          = colDef(minWidth = 75),
                three_yr_change = colDef(name = "3 Yr Change",
                                         format = colFormat(percent = TRUE, digits = 1)),
                total_new  = colDef(name = "3 Yr Trend",
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
                                            stroke = pp_co,
                                            strokeWidth = 1,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "technical",
                                            height = 4,
                                            width = 4,
                                            stroke = tech_co,
                                            strokeWidth = 1,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "new_offense",
                                            height = 4,
                                            width = 4,
                                            stroke = new_o_co,
                                            strokeWidth = 1,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparklineseries(
                                            curve = "linear",
                                            showArea = TRUE,
                                            fill = colpal_fill[index],
                                            stroke = colpal_stroke[index])))})))
  })

  ########
  # Probation table under graphs
  ########

  output$prob_table <- renderReactable({

    # filter data
    df <- prob_table %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop) %>%
      group_by(text) %>%
      summarise(total_new = list(list(total)))
    df1 <- prob_table_wide %>%
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop) %>%
      arrange(order) %>%
      select(-adm_or_pop, -state, -prob_vs_parole)

    # merge data
    df <- merge(df1, df, by = "text")
    df <- df %>% arrange(order) %>% select(-order)

    # choose colors
    colpal_fill <- c("url(#total)",
                     "url(#technical)",
                     "url(#new_offense)")
    colpal_stroke <- c(pp_co, tech_co, new_o_co)

    # create table with 3 year trend line in last column
    reactable(df,
              theme = reactableTheme(
                # Vertically center cells
                cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE),
                align = "center"),
              compact = TRUE,
              fullWidth = FALSE,
              columns = list(
                text            = colDef(name = "Metric",
                                         align = "left",
                                         minWidth = 250),
                `2018`          = colDef(minWidth = 75),
                `2019`          = colDef(minWidth = 75),
                `2020`          = colDef(minWidth = 75),
                three_yr_change = colDef(name = "3 Yr Change",
                                         format = colFormat(percent = TRUE, digits = 1)),
                total_new  = colDef(name = "3 Yr Trend",
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
                                            stroke = pp_co,
                                            strokeWidth = 1,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "technical",
                                            height = 4,
                                            width = 4,
                                            stroke = tech_co,
                                            strokeWidth = 1,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparkpatternlines(
                                            id = "new_offense",
                                            height = 4,
                                            width = 4,
                                            stroke = new_o_co,
                                            strokeWidth = 1,
                                            orientation = "diagonal"
                                          ),

                                          dui_sparklineseries(
                                            curve = "linear",
                                            showArea = TRUE,
                                            fill = colpal_fill[index],
                                            stroke = colpal_stroke[index])))})))

  })

  #-------------------------------------------------------------------------------
  # Download Data
  #-------------------------------------------------------------------------------

  # Render text depending on data selection: CSG vs BJS
  output$selected_data <- renderText({
    input$dataset
  })
  output$selected_data_info <- renderText({
    if     (input$dataset == "More Community, Less Confinement (CSG)"){
      "This dataset contains prison admissions and population numbers by state from 2018 to 2020. This dataset includes a breakdown of community supervision violation type."
    }
    else if(input$dataset == "Annual Probation Survey and Annual Parole Survey (BJS)"){
      "This dataset includes administrative data from probation and parole agencies in the United States. Data collected include the total number of adults on state and federal probation and parole on January 1 and December 31 of each year, the number of adults entering and exiting probation and parole supervision each year, and the characteristics of adults under the supervision of probation and parole agencies. Published data include both national- and state-level data. The surveys cover all 50 states, the federal system, and the District of Columbia."
    }
  })

  # change year drop down options depending on data selecion
  filteredYears <- reactive({
    if     (input$dataset == "More Community, Less Confinement (CSG)"){
      unique(csg$year)
    }
    else if(input$dataset == "Annual Probation Survey and Annual Parole Survey (BJS)"){
      unique(bjs$year)
    }
  })
  observeEvent(filteredYears(), {
    updatePickerInput(session, inputId = 'year_table', label = 'Year(s)', choices = filteredYears(), selected = filteredYears())
  })

  # react to selected states
  filteredStates <- reactive({
    if     (input$dataset == "More Community, Less Confinement (CSG)"){
      unique(csg$state)
    }
    else if(input$dataset == "Annual Probation Survey and Annual Parole Survey (BJS)"){
      unique(bjs$state)
    }
  })
  observeEvent(filteredStates(), {
    updatePickerInput(session, inputId = 'state_table', label = 'State(s)', choices = filteredStates(), selected = filteredStates())
  })

  # creative reactive element for table depending on data set
  datasetInput <- reactive({
    dataset <- switch(input$dataset,
                      "Annual Probation Survey and Annual Parole Survey (BJS)" = bjs,
                      "More Community, Less Confinement (CSG)"                 = csg)
    dataset <- dataset %>% filter(year %in% input$year_table) %>%
      filter(state %in% input$state_table) %>%
      arrange(state, year)
  })

  # generate table depending on data set
  output$main_table <- DT::renderDataTable({

    if (input$dataset == "More Community, Less Confinement (CSG)"){
      datatable(
        datasetInput(),
        colnames = c('State', 'Year', 'Data', 'Total'),
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          paging = TRUE,
          lengthMenu = list(c(10, 20, 100, -1), c('10', '20', '100', 'All')),
          pageLength = 20,
          columnDefs = list(list(className = 'dt-left', targets = '_all')),
          dom = 'Blfrtip',
          buttons =
            list('copy', 'print', list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv',   filename = paste0("mclc_", Sys.Date())),
                list(extend = 'excel', filename = paste0("mclc_", Sys.Date())),
                list(extend = 'pdf',   filename = paste0("mclc_", Sys.Date()))),
              text = 'Download')))
        ) %>%
        formatCurrency("total", currency = "", interval = 3, mark = ",", digits = 0) %>%
        formatStyle(columns = c("state"), width='65px') %>%
        formatStyle(columns = c("year", "total"), width='55px') %>%
        formatStyle(columns = c("text"), width='120px')
    }

    else if(input$dataset == "Annual Probation Survey and Annual Parole Survey (BJS)"){
      DT::datatable(
        datasetInput(),
        colnames = c('State',
                     'Year',
                     'Overall Population' ,
                     'Parole Population',
                     'Probation Population',
                     'Overall Incarcerated' ,
                     'Parole Incarcerated',
                     'Probation Incarcerated',
                     'Overall Revocation Rate' ,
                     'Parole Revocation Rate',
                     'Probation Revocation Rate'),
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          paging = TRUE,
          lengthMenu = list(c(10, 20, 100, -1), c('10', '20', '100', 'All')),
          pageLength = 20,
          columnDefs = list(list(className = 'dt-left', targets = '_all')),
          dom = 'Blfrtip',
          buttons =
            list('copy', 'print', list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv',   filename = paste0("bjs_probation_parole_", Sys.Date())),
                list(extend = 'excel', filename = paste0("bjs_probation_parole_", Sys.Date())),
                list(extend = 'pdf',   filename = paste0("bjs_probation_parole_", Sys.Date()))),
              text = 'Download')))
        ) %>%
        formatPercentage(c("overall_rev_rate", "parole_rev_rate", "prob_rev_rate"), 2) %>%
        formatCurrency(c("overall_population", "parole_population", "prob_population",
                         "overall_incarcerated", "parole_incarcerated", "prob_incarcerated"), currency = "", interval = 3, mark = ",", digits = 0) %>%
        formatStyle(columns = c("state"), width='65px') %>%
        formatStyle(columns = c("year"), width='10px') %>%
        formatStyle(columns = c("overall_population", "parole_population", "prob_population",
                                "overall_incarcerated", "parole_incarcerated", "prob_incarcerated",
                                "overall_rev_rate", "parole_rev_rate", "prob_rev_rate"), width='20px')
    }
  })

}
