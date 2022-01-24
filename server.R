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

# output$table_out <- DT::renderDataTable(
#   datatable(data = mclc_datatable,
#             extensions = 'Buttons',
#             filter = "top",
#             selection = 'single',
#             options = list(
#                dom = "Blfrtip",
#                buttons =
#                  list("copy", list(
#                    extend = "collection", buttons = c("csv", "excel", "pdf"),
#                    text = "Download")), # end of buttons customization
#                # customize the length menu
#                lengthMenu = list(c(25, 50, 100, -1), # declare values
#                                  c(25, 50, 100, "All") # declare titles
#                ), # end of lengthMenu customization
#                pageLength = 25
#              ) # end of options
#   )
# )
  
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
    
      # %>%
      # addLegend(position = "topright",
      #           # colors = brewer.pal(7, "Blues"),
      #           pal = pal_fun,
      #           values = ~df_map$total,
      #           opacity = 1,
      #           # labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2, big.mark = ",")),
      #           labFormat = labelFormat(suffix="%"),
      #           title = "<strong>Count</strong>") 
    
      # %>%
      # addLegendNumeric(pal = pal_fun,
      #                  values = df_map$total,
      #                  orientation = 'horizontal',
      #                  position = 'topleft',
      #                  width = 150,
      #                  height = 20,
      #                  labFormat = labelFormat(suffix="%"),
      #                  title = '% Change')

  })
  
  #-------------------------------------------------------------------------------
  # State Reports
  #-------------------------------------------------------------------------------
  
  # Print state name and adm or pop selected
  output$selected_state <- renderText({ 
    paste("Trends in ", input$state)
  })  
  
  # # Print state name and adm or pop selected
  # output$selected_state_adm_pop <- renderText({ 
  #   paste("This snapshot shows available data for ", input$state, " from 2018 to 2020.")
  # })
  
  # Totals areachart
  output$areachart <- renderPlot({ 
    
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
    
    title <- paste0("Prison ", input$adm_or_pop, " by Type\n")
    
    df_totals$year <- as.numeric(df_totals$year)
    df_totals$metric <- as.factor(df_totals$metric)
    totals$year <- as.numeric(totals$year)
    totals_viols$year <- as.numeric(totals_viols$year)
    totals_tech$year <- as.numeric(totals_tech$year)
    
    ggplot(df_totals, aes(x=year, y=total, fill=metric)) + 
      geom_area()+
      ggtitle(title) +
      theme_csgjc_areaplot() +
      theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      # geom_hline(yintercept=0, colour="black", size = 1) +
      # totals
      geom_text(size = 4.5, aes(x = year + 0.15,label=ifelse(year == min(year),scales::comma(total), NA), fill = NULL),     data = totals, position = position_stack(vjust = 1.25), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(label=ifelse(year != min(year) & year != max(year),scales::comma(total), NA), fill = NULL), data = totals, position = position_stack(vjust = 1.23), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(x = year - 0.15,label=ifelse(year == max(year),scales::comma(total), NA), fill = NULL),     data = totals, position = position_stack(vjust = 1.37), check_overlap = TRUE) +
      # sup viols
      geom_text(size = 4.5, aes(x = year + 0.15,label=ifelse(year == min(year),scales::comma(total), NA), fill = NULL),     data = totals_viols, position = position_stack(vjust = 1.53), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(label=ifelse(year != min(year) & year != max(year),scales::comma(total), NA), fill = NULL), data = totals_viols, position = position_stack(vjust = 1.5), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(x = year - 0.15,label=ifelse(year == max(year),scales::comma(total), NA), fill = NULL),     data = totals_viols, position = position_stack(vjust = 1.65), check_overlap = TRUE) +
      # tech viols
      geom_text(color = "white", size = 4.5, aes(x = year + 0.15,label=ifelse(year == min(year),scales::comma(total), NA), fill = NULL),     data = totals_tech, position = position_stack(vjust = 1.22), check_overlap = TRUE) +
      geom_text(color = "white", size = 4.5, aes(label=ifelse(year != min(year) & year != max(year),scales::comma(total), NA), fill = NULL), data = totals_tech, position = position_stack(vjust = 1.22), check_overlap = TRUE) +
      geom_text(color = "white", size = 4.5, aes(x = year - 0.15,label=ifelse(year == max(year),scales::comma(total), NA), fill = NULL),     data = totals_tech, position = position_stack(vjust = 1.26), check_overlap = TRUE) +
      # colors
      scale_fill_manual(values = c(total_co, viol_co, tech_co), 
                        labels = c("Total", "Supervision Violation", "Technical Violation"), 
                        breaks = c("Other", "Supervision Violations", "Technical"),
                        name = "") +  
      scale_x_continuous(breaks = c(1,2,3), labels = c("            2018", "2019", "2020             ")) 
      # labs(caption="\nSource: More Community, Less Confinement (2020)")
  })
  
  # Totals barchart
  output$barchart <- renderPlot({
    
    df_totals <- 
      adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "New Offense" | metric == "Technical")  
    
    totals <- df_totals %>%
      group_by(year) %>%
      summarise(total = sum(total))
    
    dodger = position_dodge(width = 0.9)
    
    title <- paste0("Prison ",input$adm_or_pop, " due to\nSupervision Violations by Type\n")
    
    ggplot(data = df_totals,
           aes_string(x = 'year', y = 'total', fill = 'metric')) +
      geom_bar(stat = "identity", position = "dodge", alpha = 1) +
      geom_text(aes(label=scales::comma(total)),
                position=dodger,
                size = 4.5,
                colour = "#000000",
                vjust = -0.5) +
      ggtitle(title) +
      theme_csgjc_plot_legend() +
      theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      scale_fill_manual(values = c(new_o_co, tech_co),
                        labels = c("New Offense", "Technical Violation"), 
                        breaks = c("New Offense", "Technical"),
                        name = "") +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.17*max(df_totals$total)),
                         expand = c(0,0)) +
      coord_cartesian(clip = "off") 
      # labs(caption="\nSource: More Community, Less Confinement (2020)")
    
  })
  
  ##############
  # Donut charts and value boxes
  ##############
  
  # output$donutchart_sup <- renderGirafe({
  #   
  #   # filter data depending on input
  #   df_donutchart <- 
  #     adm_pop_long %>% 
  #     filter(states == input$state &
  #              adm_or_pop == input$adm_or_pop,
  #            year == 2020) %>% 
  #     group_by(metric, year) %>% 
  #     summarise(total = sum(total)) %>% 
  #     filter(metric == "Other" | metric == "Supervision Violations") %>% 
  #     select(-year)
  #   
  #   # sum 
  #   df_donutchart$overall <- sum(df_donutchart$total)
  #   
  #   # create variables
  #   df_donutchart <- df_donutchart %>% group_by(metric) %>% 
  #     mutate(percentage = total / overall,
  #            hover_text = paste0(metric, ": ", total)) %>%
  #     mutate(percentage_label = paste0(round(100 * percentage, 0), "%")) %>% 
  #     select(-overall)
  #   
  #   # make dataframe
  #   df_donutchart <- as.data.frame(df_donutchart)
  #   
  #   # donut plot
  #   donut_plot <- ggplot(df_donutchart, aes_string(y = 'total', fill = 'metric')) +
  #     geom_bar_interactive(
  #       aes(x = 1, tooltip = hover_text),
  #       width = 0.5,
  #       stat = "identity",
  #       show.legend = FALSE
  #     ) +
  #     annotate(
  #       geom = "text",
  #       x = 0,
  #       y = 0,
  #       label = df_donutchart[["percentage_label"]][df_donutchart[["metric"]] == "Supervision Violations"],
  #       size = 20,
  #       color = "#000000"
  #     ) +
  #     scale_fill_manual(values = c(Other = "#DEF0F6", `Supervision Violations` = "#E18731")) +
  #     coord_polar(theta = "y") +
  #     ggtitle("Proportion of Prison Admissions\nThat Are Supervision Violations") +
  #     theme_void() +
  #     theme(plot.title = element_text(size = 20,
  #                                     hjust = 0.5))
  #   
  #   ggiraph(ggobj = donut_plot)
  # })
  
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
    
    title <- paste0("Parole ",input$adm_or_pop, " by Type\n")
    
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
  
  # prob parole bar and line chart
  output$barchart_bjs_parole <- renderPlot({
    
    df <- 
      df_prob_parole %>% 
      filter(state == input$state)
    df <- df %>% filter(adm_or_pop != "Population (EOY)")
    df <- df %>% mutate(metric = case_when(
      data == "total_pop_start" & type == "Probation" ~ "Probation Population",
      data == "total_entries" & type == "Probation" ~ "Probation Entries",
      data == "total_discharges" & type == "Probation" ~ "Probation Exits",
      
      data == "total_pop_start" & type == "Parole" ~ "Parole Population",
      data == "total_entries" & type == "Parole" ~ "Parole Entries",
      data == "total_discharges" & type == "Parole" ~ "Parole Exits"
    ))
    
    title <- paste0("Parole Population, Entries and Exits\n")
    
    entry <- df %>% filter(metric == "Parole Entries")
    exit <- df %>% filter(metric == "Parole Exits")
    areas <- df %>% dplyr::filter(metric == "Parole Population")
    
    ggplot()+
      geom_bar(data = areas, aes(x=year, y=total, fill=metric),
               stat="identity", width = 0.5) +
      geom_line(data = entry, aes(x = year, y= total), size=1.25, color = entries)+
      geom_line(data = exit, aes(x = year, y= total), size=1.25, color = exits)+
      geom_text(data = areas, aes(x = year, y = total, label = scales::comma(total)),
                position=position_dodge(0.8), vjust = -0.6, size = 5) +
      scale_fill_manual(values=par_cols) +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.15*max(areas$total)),
                         expand = c(0,0)) +
      ggtitle(title)+
      theme_csgjc_prob_parole() +
      labs(caption="\nSource: BJS Annual Parole Survey (2018)")
    
  })
  
  # prob parole bar and line chart
  output$barchart_bjs_prob <- renderPlot({
    
    df <- 
      df_prob_parole %>% 
      filter(state == input$state)
    df <- df %>% filter(adm_or_pop != "Population (EOY)")
    df <- df %>% mutate(metric = case_when(
      data == "total_pop_start" & type == "Probation" ~ "Probation Population",
      data == "total_entries" & type == "Probation" ~ "Probation Entries",
      data == "total_discharges" & type == "Probation" ~ "Probation Exits",
      
      data == "total_pop_start" & type == "Parole" ~ "Parole Population",
      data == "total_entries" & type == "Parole" ~ "Parole Entries",
      data == "total_discharges" & type == "Parole" ~ "Parole Exits"
    ))
    
    title <- paste0("Probation Population, Entries and Exits\n")
    
    entry <- df %>% filter(metric == "Probation Entries")
    exit <- df %>% filter(metric == "Probation Exits")
    areas <- df %>% dplyr::filter(metric == "Probation Population")
    
    ggplot()+
      geom_bar(data = areas, aes(x=year, y=total, fill=metric),
               stat="identity", width = 0.5) +
      geom_line(data = entry, aes(x = year, y= total), size=1.25, color = entries)+
      geom_line(data = exit, aes(x = year, y= total), size=1.25, color = exits)+
      geom_text(data = areas, aes(x = year, y = total, label = scales::comma(total)),
                position=position_dodge(0.8), vjust = -0.6, size = 5) +
      scale_fill_manual(values=prob_cols) +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.15*max(areas$total)),
                         expand = c(0,0)) +
      ggtitle(title)+
      theme_csgjc_prob_parole() +
      labs(caption="\nSource: BJS Annual Probation Survey (2018)")
    
  })
  
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
