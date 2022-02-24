server <- function(input, output, session) {
  
  #-------------------------------------------------------------------------------
  # Map Explorer
  #-------------------------------------------------------------------------------
  
  # change sidebar depending on selection 
  # change from previous year only includes years 2019-2020
  # count includes years 2018-2020
  df_map_temp <- reactive ({
    if(input$choice_map_counts == "Count"){     if(input$year_map_counts == "2018"){filter(mclc_explorer, year=="2018" & choice == "Count")}
                                           else if(input$year_map_counts == "2019"){filter(mclc_explorer, year=="2019" & choice == "Count")}
                                           else if(input$year_map_counts == "2020"){filter(mclc_explorer, year=="2020" & choice == "Count")}
                                          }
    else if(input$choice_map_counts == "Change from Previous Year" & input$year_map_counts2 == "2019"){filter(mclc_explorer, year == "2019" & choice == "Change from Previous Year")} 
    else if(input$choice_map_counts == "Change from Previous Year" & input$year_map_counts2 == "2020"){filter(mclc_explorer, year == "2020" & choice == "Change from Previous Year")}
  })
  
  # filter data depending on choice above
  df_map <- reactive({ 
    df_map_temp() %>% 
      filter(adm_or_pop == input$adm_or_pop_map_counts,
             metric == input$data_map_counts)
  })
  
  # format for datatable
  df_map_table <- reactive({
    df_map() %>%
      arrange(desc(total)) %>%
      select(State = states,
             Year = year,
             Data = metric,
             Type = adm_or_pop,
             Value = total,
             choice)
  })
  
  ##############
  # Hex map
  ##############
  
  output$map_counts <- renderPlot({

    df_map <- sp::merge(us, df_map(), by.x = 'iso3166_2', by.y = "Code")

    # map
    gg <- ggplot()
    # add outline
    gg <- gg + geom_map(data=us_map, map=us_map,
                        aes(x=long, y=lat, map_id=id),
                        color="white", size=0.5)
    # add data
    gg <- gg + geom_map(data=df_map@data, map=us_map,
                        aes(fill=total, map_id=iso3166_2))
    # overlay borders without ugly line on legend
    gg <- gg + geom_map(data=df_map@data, map=us_map,
                        aes(map_id=iso3166_2),
                        fill="#ffffff", alpha=0, color="white",
                        show_guide=FALSE)
    gg <- gg +
      geom_text(data=centers, aes(label = id, x = x, y = y), color = "white", size = 4) +
      coord_map() +
      labs(x=NULL, y=NULL) +
      theme_bw() +
      theme(panel.border=element_blank(),
            panel.grid=element_blank(),
            legend.position = c(0.5, 0.9),
            legend.title=element_text(size=14),
            legend.text=element_text(size=14),
            axis.ticks=element_blank(),
            axis.text=element_blank(),
            plot.title = element_text(hjust = 0.5,
                                      face = "bold",
                                      size = 16))
    
    if(input$choice_map_counts == "Count"){
      
      title <- paste0(input$data_map_counts, " Prison ", input$adm_or_pop_map_counts, " in ", input$year_map_counts)
      gg + scale_fill_gradientn("Number of People",
                                colours = count_colors,
                                na.value="#D3D3D3",
                                label = scales::comma,
                                guide = guide_legend(keyheight = unit(3, units = "mm"),
                                                     keywidth=unit(12, units = "mm"),
                                                     label.position = "bottom",
                                                     title.position = 'top', nrow=1)) +
           ggtitle(title)
    }
    
    else if(input$choice_map_counts == "Change from Previous Year"){
      
      change_year <- as.numeric(input$year_map_counts2)
      change_year <- change_year - 1
      title <- paste0("Change in ", input$data_map_counts, " Prison ", input$adm_or_pop_map_counts, " between ", change_year, " and ", input$year_map_counts2)
      
      gg + scale_fill_scico("Change from Previous Year",
                            palette = "vik", 
                            na.value="#D3D3D3",
                            label = scales::percent,
                            limits = c(-1, 1)*max(abs(df_map()$total)),
                            guide = guide_legend(keyheight = unit(3, units = "mm"),
                                                 keywidth=unit(12, units = "mm"),
                                                 label.position = "bottom",
                                                 title.position = 'top', nrow=1)) +
           ggtitle(title)
    }
    
  }, height="auto")
  
  ##############
  # Table changes depending on count vs change
  ##############
  
  output$table_map_counts <- DT::renderDataTable(
    
    if(input$choice_map_counts == "Count"){
      
      df_map_table() %>% 
        datatable(#extensions = 'Buttons',
                  selection = 'single',
                  rownames = FALSE,
                  options = list(
                    searching = TRUE,
                    # hide choice column
                    columnDefs = list(list(visible=FALSE, targets=c(5))),
                    # dom = "Blfrtip",
                    # buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel", "pdf"),text = "Download")), 
                    lengthMenu = list(c(5, 10, 20, -1), 
                                      c(5, 10, 20, "All")),                 
                    pageLength = 5)) %>% 
        # format color not working
        formatStyle("Type", target = 'row', 
                    backgroundColor = "#FFFFFF") %>% 
        # add commas
        formatRound('Value', interval = 3, digits = 0, mark = ",") 
      
    }
    else if(input$choice_map_counts == "Change from Previous Year"){
      
      df_map_table() %>% 
        arrange(Value) %>% 
        datatable(#extensions = 'Buttons',
                  selection = 'single',
                  rownames = FALSE,
                  options = list(
                    searching = TRUE,
                    # hide choice column
                    columnDefs = list(list(visible=FALSE, targets=c(5))),
                    # dom = "Blfrtip",
                    # buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel", "pdf"),text = "Download")), 
                    lengthMenu = list(c(5, 10, 20, -1), 
                                      c(5, 10, 20, "All")),                 
                    pageLength = 5)) %>% 
        # format color not working
        formatStyle("Type", target = 'row', 
                    backgroundColor = "#FFFFFF") %>% 
        # add % sign
        formatPercentage('Value', digits = 2) 
    }
  )
  
  #-------------------------------------------------------------------------------
  # Leaflet Map
  #-------------------------------------------------------------------------------

  output$leaflet_map <- renderLeaflet({
    
    df_map <- sp::merge(us, df_map(), by.x = 'iso3166_2', by.y = "Code")
    
    # pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
    pal_fun <- colorNumeric('inferno', df_map$total)
    
    p_popup <- paste0(df_map$states, ": ", round(df_map$total, 2), "%")
    
    leaflet(df_map, options = leafletOptions(zoomControl = FALSE,
                                             minZoom = 3.5, 
                                             maxZoom = 3.5,
                                             dragging = FALSE)) %>%
      
      addPolygons(stroke = FALSE, # remove borders
                  fillColor = ~pal_fun(total), 
                  color = "white",
                  fillOpacity = 0.8, 
                  smoothFactor = 0.5, 
                  popup = p_popup) %>% 
      
      # set view to US
      setView(lng = -96.25, lat = 30.50, zoom = 3.5) 

  })
  
  #-------------------------------------------------------------------------------
  # State Reports
  #-------------------------------------------------------------------------------
  
  # Print state name and adm or pop selected
  output$selected_state <- renderText({ 
    paste("Trends in ", input$state)
  })  
  
  # Total admissions and sup viols chart
  output$totals_chart <- renderPlot({ 
    
    df_totals <-
      adm_pop_long %>%
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop) %>%
      group_by(metric, year) %>%
      summarise(total = sum(total)) %>%
      filter(metric == "Other" | metric == "Technical" | metric == "Supervision Violations")
    
    totals <- df_totals %>%
      filter(metric != "Technical") %>%
      group_by(year) %>%
      summarise(total = sum(total))
    
    totals_viols <- df_totals %>%
      filter(metric == "Supervision Violations")
    
    totals_tech <- df_totals %>%
      filter(metric == "Technical")
    
    title <- paste0("Prison ", input$adm_or_pop, "\n")
    
    df_totals$year <- as.numeric(df_totals$year)
    df_totals$metric <- as.factor(df_totals$metric)
    totals$year <- as.numeric(totals$year)
    totals_viols$year <- as.numeric(totals_viols$year)
    totals_tech$year <- as.numeric(totals_tech$year)
    
    total <-
      adm_pop_long %>%
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop &
             metric == "Total")
      
    max <- max(total$total, na.rm = FALSE)
    
    ggplot(df_totals, aes(x=year, y=total, fill=metric)) +
      geom_area()+
      ggtitle(title) +
      theme_csgjc_horizontal_legend() +
      coord_cartesian(ylim=c(0,max*1.3), expand = FALSE ) +
      # totals
      geom_text(size = 4.5, aes(x = year + 0.15,label=ifelse(year == min(year),scales::comma(total), NA), fill = NULL),     data = totals, position = position_stack(vjust = 1.25), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(label=ifelse(year != min(year) & year != max(year),scales::comma(total), NA), fill = NULL), data = totals, position = position_stack(vjust = 1.23), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(x = year - 0.15,label=ifelse(year == max(year),scales::comma(total), NA), fill = NULL),     data = totals, position = position_stack(vjust = 1.37), check_overlap = TRUE) +
      # sup viols
      geom_text(size = 4.5, aes(x = year + 0.15,label=ifelse(year == min(year),scales::comma(total), NA), fill = NULL),     data = totals_viols, position = position_stack(vjust = 1.53), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(label=ifelse(year != min(year) & year != max(year),scales::comma(total), NA), fill = NULL), data = totals_viols, position = position_stack(vjust = 1.5), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(x = year - 0.15,label=ifelse(year == max(year),scales::comma(total), NA), fill = NULL),     data = totals_viols, position = position_stack(vjust = 1.65), check_overlap = TRUE) +
      # tech viols
      geom_text(size = 4.5, aes(x = year + 0.15,label=ifelse(year == min(year),scales::comma(total), NA), fill = NULL),     data = totals_tech, position = position_stack(vjust = 1.22), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(label=ifelse(year != min(year) & year != max(year),scales::comma(total), NA), fill = NULL), data = totals_tech, position = position_stack(vjust = 1.22), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(x = year - 0.15,label=ifelse(year == max(year),scales::comma(total), NA), fill = NULL),     data = totals_tech, position = position_stack(vjust = 1.26), check_overlap = TRUE) +
      # colors
      scale_fill_manual(values = c(total_co, viol_co, tech_co),
                        labels = c("Total", "Supervision Violation", "Technical Violation"),
                        breaks = c("Other", "Supervision Violations", "Technical"),
                        name = "") +
      scale_x_continuous(breaks = c(1,2,3), labels = c("            2018", "2019", "2020             "))
    # labs(caption="\nSource: More Community, Less Confinement (2020)")
    
    # df <- adm_pop_long %>%
    #   filter(states == input$state &
    #          adm_or_pop == input$adm_or_pop &
    #          metric == "Total") 
    # df$year <- as.numeric(df$year)
    # 
    # title <- paste0("Total ", input$adm_or_pop, "\n")
    # 
    # ggplot(df, aes(x=year, y = total)) +
    #   geom_area(fill = blue2, color = blue2, alpha = 0.5) +
    #   geom_text(data = df, aes(x = year, y = total, label = scales::comma(total)),
    #             position=position_dodge(0.8), vjust = -1, size = 5) +
    #   scale_y_continuous(label = scales::comma,
    #                      limits = c(0, 1.15*max(df$total)),
    #                      expand = c(0,0)) +
    #   theme_csgjc_horizontal_legend() +
    #   theme(axis.text.y = element_blank(),
    #         axis.line.x = element_line(colour = 'black', size=1, linetype='solid')) +
    #   scale_x_continuous(breaks = c(1,2,3), labels = c("2018", "2019", "2020")) +
    #   ggtitle(title)
    
    # df %>%
    #   ggplot(aes(year, total)) +
    #   stat_smooth(geom = 'area', fill = "red", alpha = 0.5) +
    #   geom_text(data = df, aes(x = year, y = total, label = scales::comma(total)),
    #             position=position_dodge(0.8), vjust = -1, size = 5) +
    #   scale_y_continuous(label = scales::comma,
    #                      limits = c(0, 1.15*max(df$total)),
    #                      expand = c(0,0)) +
    #   theme_csgjc_horizontal_legend() +
    #   theme(axis.text.y = element_blank(),
    #         axis.line.x = element_line(colour = 'black', size=1, linetype='solid')) +
    #   scale_x_continuous(breaks = c(1,2,3), labels = c("2018", "2019", "2020")) +
    #   ggtitle(title) 
    
  })
  
  # Supervision violations by type chart
  output$sup_viols_type_chart <- renderPlot({
    
    df <- 
      adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "New Offense" | metric == "Technical")  
    
    total <- adm_pop_long %>%
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop &
             metric == "Supervision Violations") 
    
    dodger = position_dodge(width = 0.9)
    
    title <- paste0("Supervision Violation ",input$adm_or_pop, " by Type\n")
    
    ggplot(data = df,
           aes_string(x = 'year', y = 'total', fill = 'metric')) +
      geom_bar(stat = "identity", position = "dodge", alpha = 1) +
      geom_text(aes(label=scales::comma(total)),
                position=dodger,
                size = 4.5,
                colour = "#000000",
                vjust = -0.5) +
      ggtitle(title) +
      theme_csgjc_plot_legend() +
      theme(axis.text.y = element_blank(),
            axis.line.x = element_line(colour = 'black', size=1, linetype='solid')) +
      scale_fill_manual(values = c(new_o_co, tech_co),
                        labels = c("New Offense", "Technical Violation"), 
                        breaks = c("New Offense", "Technical"),
                        name = "") +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.17*max(total$total)),
                         expand = c(0,0)) +
      coord_cartesian(clip = "off") 
      # labs(caption="\nSource: More Community, Less Confinement (2020)")
    
  })
  
  ##############
  # Value boxes
  ##############
  
  # filter data
  df_total_18 <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop &
             year == "2019" &
             metric == "Total")
  })
  
  vb <- valueBox2(
    value = "10,080",
    title = "Admissions in 2020",
    subtitle = tagList(HTML("&darr;"), "25% from 2019"),
    # icon = icon("arrow-down"),
    # width = 10,
    color = "green",
    href = NULL
  )
  
  vb2 <- valueBox2(
    value = "4,761",
    title = "Supervision Violations in 2020",
    subtitle = tagList(HTML("&darr;"), "30% from 2019"),
    # icon = icon("arrow-down"),
    # width = 10,
    color = "green",
    href = NULL
  )
  
  vb3 <- valueBox2(
    value = "2,080",
    title = "Technical Violations in 2020",
    subtitle = tagList(HTML("&darr;"), "19% from 2019"),
    # icon = icon("arrow-down"),
    # width = 10,
    color = "green",
    href = NULL
  )
  
  output$total_change <- renderValueBox(vb)
  
  output$sup_change <- renderValueBox(vb2)
  
  output$tech_change <- renderValueBox(vb3)
  
  ##################################
  # State table under graphs
  ##################################
  
  # State table under graphs
  output$state_table <- renderReactable({
    
    # filter data
    df <- state_table %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop) %>% 
      select(-c(states, adm_or_pop, metric, data))
    
    # overview table with expandable rows
    reactable(df,
              groupBy = "type",
              defaultSortOrder = 'desc',
              defaultSorted = 'type',
              striped = FALSE,
              highlight = TRUE,
              pagination = FALSE,
              # compact = TRUE,
              # showSortable = TRUE,
              outlined = TRUE,
              borderless = TRUE,
              # theme = reactableTheme(
              #   borderColor = "#C8C8C8",
              #   stripedColor = "#F5F5F5",
              #   highlightColor = "#C8C8C8",
              #   cellPadding = "4px 6px",
              #   headerStyle = list(background = "#C8C8C8"),
              #   style = list(#fontFamily = "Cambria", 
              #                fontSize = 14)),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE)),
              
              columns = list(
                type             = colDef(name = "Breakdown",
                                          html = TRUE,
                                          align = "left",
                                          minWidth = 250,
                                          style = list(fontWeight = "bold")),
                text              = colDef(name = "Metric",
                                           minWidth = 160),
                `2018`            = colDef(aggregate = "sum"),
                `2019`            = colDef(aggregate = "sum"),
                `2020`            = colDef(aggregate = "sum")
              )
    )
    
  })
  
  ##################################
  # Probation and Parole Charts
  ##################################
  
  # parole plot
  output$barchart_parole <- renderPlot({
    
    df_parole <- 
      adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               prob_vs_parole == "Parole") %>% 
      filter(metric == "Technical" | metric == "New Offense")  %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) 
    
    totals <- df_parole %>%
      group_by(year) %>%
      summarise(total = sum(total))
    
    title <- paste0("Prison ",input$adm_or_pop, " due to Parole Violations by Type\n")
    
    ggplot(data = df_parole,
           aes_string(x = 'year', y = 'total', fill = 'metric')) +
      geom_bar(stat = "identity", position = "dodge", alpha = 1) +
      geom_text(aes(label=scales::comma(total)),
                position=position_dodge(width = 0.9),
                size = 4.5,
                colour = "#000000",
                vjust = -0.5) +
      theme_csgjc_plot_legend() +
      ggtitle(title) +
      theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      scale_fill_manual(values = c(new_o_co, tech_co),
                        name = "") +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.17*max(df_parole$total)),
                         expand = c(0,0)
      ) +
      coord_cartesian(clip = "off")
    
  })
  
  # prob plot
  output$barchart_prob <- renderPlot({
    
    df_prob <- 
      adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               prob_vs_parole == "Probation") %>% 
      filter(metric == "Technical" | metric == "New Offense")  %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) 
    
    totals <- df_prob %>%
      group_by(year) %>%
      summarise(total = sum(total))
    
    title <- paste0("Probation ",input$adm_or_pop, " by Type\n")
    
    ggplot(data = df_prob,
           aes_string(x = 'year', y = 'total', fill = 'metric')) +
      geom_bar(stat = "identity", position = "dodge", alpha = 1) +
      geom_text(aes(label=scales::comma(total)),
                position=position_dodge(width = 0.9),
                size = 4.5,
                colour = "#000000",
                vjust = -0.5) +
      theme_csgjc_plot_legend() +
      ggtitle(title) +
      theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      scale_fill_manual(values = c(new_o_co, tech_co),
                        name = "") +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.17*max(df_prob$total)),
                         expand = c(0,0)
      ) +
      coord_cartesian(clip = "off")
    
  })
  
  ##################################
  # BJS Probation and Parole Charts
  ##################################
  
  # prob prob bar and line chart
  output$barchart_bjs_prob_total <- renderPlot({
    
    entries_woi <- bjs_prob %>% 
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop &
               data == "entries_wo_inc")

    total <-  bjs_prob %>% 
      filter(state == input$state &
               adm_or_pop == input$adm_or_pop &
               data == "entries_total")
    
    title <- paste0("Probation Entries\n")
    
    ggplot() + 
      geom_bar(data = entries_woi, aes(x = year, y = total, fill = "Probation Entries without Incarceration"), stat="identity") +
      geom_line(data = total, aes(x = year, y = total, group = 1, color = metric), size = 1.25) +
      geom_text(data = entries_woi, aes(x = year, y = total, label = scales::comma(total)),
                position=position_dodge(0.8), vjust = -0.6, size = 5) +
      geom_text(data = total, aes(x = year, y = total, label = scales::comma(total)),
                position=position_dodge(0.8), vjust = -0.6, size = 5) +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.15*max(total$total)),
                         expand = c(0,0)) +
      scale_colour_manual(" ", values=c("Total Probation Entries" = red))+
      scale_fill_manual("",values=blue2)+
      theme_csgjc_horizontal_legend() +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.position = "top",
            legend.box="horizontal",
            axis.text.y = element_blank()) +
      ggtitle(title) 

  })
  
  # # prob barchart
  # output$barchart_bjs_prob <- renderPlot({
  #   
  #   
  #   
  # })
  
  #-------------------------------------------------------------------------------
  # Download Data
  #-------------------------------------------------------------------------------
  
  datasetInput <- reactive({
    dataset <- switch(input$dataset,
                      "Bureau of Justice Statistics" = bjs,
                      "More Community, Less Confinement (CSG)" = csg)
    dataset <- dataset %>% filter(year %in% input$year_table |
                                    year %in% input$year_table2) %>% 
      filter(state %in% input$state_table |
             state %in% input$state_table2)
  })
  
  # Generate a summary of the dataset ----
  output$main_table <- DT::renderDataTable({
    datatable(datasetInput())
  })
  
  
}
