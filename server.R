#######################################
# Project: MCLCShiny
# File: server.R
# Authors: Mari Roberts
# Date: December 8, 2021
# Description: 
#    Server
#######################################

library(RColorBrewer)
pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette
class(pal)

server <- function(input, output, session) { 
  
  #__________________________________________________________________________________________________________________________________________________________
  # 1) About
  #__________________________________________________________________________________________________________________________________________________________
  
  # no code needed
  
  #__________________________________________________________________________________________________________________________________________________________
  # 2) Dashboard
  #__________________________________________________________________________________________________________________________________________________________
  
  ##################################
  # Main header and paragraph
  ##################################
  
  # Print state name and adm or pop selected
  output$selected_state <- renderText({ 
    paste("Prison ", input$adm_or_pop, "Trends in ", input$state)
  })
  
  # Print state name and adm or pop selected
  output$selected_state_adm_pop <- renderText({ 
    paste("States across the country saw changes in their prison admissions and 
          populations due to supervision violations in 2020. But some states were 
          already experiencing reductions in violation admissions and population 
          prior to the pandemic. This snapshot shows available data for ", input$state, " from 2018 to 2020.")
  })
  
  ##################################
  # 1st plot titles
  ##################################
  
  # Print adm or pop selected
  output$adm_pop_title <- renderText({ 
    paste("Overall ", input$adm_or_pop, "by Type")
  })
  
  # Print adm or pop selected for sup viols
  output$viol_title <- renderText({ 
    paste("Supervision Violation ", input$adm_or_pop, "by Type")
  })
  
  ##############################################################################
  ##############################################################################
  # Bar chart about total admissions or population
  ##############################################################################
  ##############################################################################
  
  output$barchart <- renderPlot({
    
    df_totals <- 
      adm_pop_long %>% 
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "Other" | metric == "Supervision Violations")  
    
    totals <- df_totals %>%
      group_by(year) %>%
      summarise(total = sum(total))
    
    dodger = position_dodge(width = 0.9)
    
    ggplot(data = df_totals,
           aes_string(x = 'year', y = 'total', fill = 'metric')) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label=scales::comma(total)),
                position=dodger,
                size = 5.5,
                colour = "#000000",
                vjust = -0.5) +
      theme_csgjc_plot_legend() +
      theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      scale_fill_manual(values = c("#DEF0F6", "#E18731"),
                        name = "") +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.17*max(df_totals$total)),
                         expand = c(0,0)
      ) +
      coord_cartesian(clip = "off")
    
  })
  
  ##################################
  # Donut chart about supervision violations
  ##################################
  
  output$donutchart <- renderGirafe({ 
    
    # filter data depending on input
    df_donutchart <- 
      adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop,
             year == 2020) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "Other" | metric == "Supervision Violations") %>% 
      select(-year)
    
    # sum 
    df_donutchart$overall <- sum(df_donutchart$total)
    
    # create variables
    df_donutchart <- df_donutchart %>% group_by(metric) %>% 
      mutate(percentage = total / overall,
             hover_text = paste0(metric, ": ", total)) %>%
      mutate(percentage_label = paste0(round(100 * percentage, 0), "%")) %>% 
      select(-overall)
    
    # make dataframe
    df_donutchart <- as.data.frame(df_donutchart)
    
    # donut plot
    donut_plot <- ggplot(df_donutchart, aes_string(y = 'total', fill = 'metric')) +
      geom_bar_interactive(
        aes(x = 1, tooltip = hover_text),
        width = 0.5,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = df_donutchart[["percentage_label"]][df_donutchart[["metric"]] == "Supervision Violations"],
        size = 20,
        color = "#000000"
      ) +
      scale_fill_manual(values = c(Other = "#DEF0F6", `Supervision Violations` = "#E18731")) +
      coord_polar(theta = "y") +
      theme_void() +
      ggtitle("Supervision Violations \n In 2020") +
      theme(plot.title = element_text(size = 40,
                                      hjust = 0.5))
    
    ggiraph(ggobj = donut_plot)
    
  })
  
  ##################################
  # Bar chart about supervision violations
  ##################################
  
  output$barchart_supviols <- renderPlot({
    
    df_supviols <- 
      adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "Technical" | metric == "New Offense")  
    
    totals <- df_supviols %>%
      group_by(year) %>%
      summarise(total = sum(total))
    
    dodger = position_dodge(width = 0.9)
    
    ggplot(data = df_supviols,
           aes_string(x = 'year', y = 'total', fill = 'metric')) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label=scales::comma(total)),
                position=dodger,
                size = 5.5,
                colour = "#000000",
                vjust = -0.5) +
      theme_csgjc_plot_legend() +
      theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      scale_fill_manual(values = c("#DEF0F6", "#E18731"),
                        name = "") +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.17*max(df_supviols$total)),
                         expand = c(0,0)
      ) +
      coord_cartesian(clip = "off")
    
  })
  
  ##################################
  # Donut chart about supervision violations
  ##################################
  
  output$donutchart_supviols <- renderGirafe({ 
    
    # filter data depending on input
    df_donutchart_supviols <- 
      adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop,
             year == 2020) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "Technical" | metric == "New Offense") %>% 
      select(-year)
    
    # sum 
    df_donutchart_supviols$overall <- sum(df_donutchart_supviols$total)
    
    # create variables
    df_donutchart_supviols <- df_donutchart_supviols %>% group_by(metric) %>% 
      mutate(percentage = total / overall,
             hover_text = paste0(metric, ": ", total)) %>%
      mutate(percentage_label = paste0(round(100 * percentage, 0), "%")) %>% 
      select(-overall)
    
    # make dataframe
    df_donutchart_supviols <- as.data.frame(df_donutchart_supviols)
    
    # donut plot
    donut_plot_supviols <- ggplot(df_donutchart_supviols, aes_string(y = 'total', fill = 'metric')) +
      geom_bar_interactive(aes(x = 1, tooltip = hover_text),width = 0.5,stat = "identity",show.legend = FALSE) +
      annotate(geom = "text",x = 0,y = 0,label = df_donutchart_supviols[["percentage_label"]][df_donutchart_supviols[["metric"]] == "Technical"],size = 20,color = "#000000") +
      scale_fill_manual(values = c(`New Offense` = "#DEF0F6", Technical = "#E18731")) +
      coord_polar(theta = "y") +
      theme_void() +
      ggtitle("Technical Violations \n In 2020") +
      theme(plot.title = element_text(size = 40,hjust = 0.5))
    
    ggiraph(ggobj = donut_plot_supviols)
    
  })
  
  ##################################
  # Value boxes
  ##################################
  
  # filter data
  df_total_18 <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               year == "2019" &
               metric == "Total")
  })
  
  # Total 
  # Since 2018
  output$total_change_18 <- renderValueBox({
    valueBox(
      paste0(df_total_18()$change, "%"), subtitle = "2018-2019", color = "green")
  })
  
  # filter data
  df_total_19 <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               year == "2020" &
               metric == "Total")
  })
  
  # Total 
  # Since 2019
  output$total_change_19 <- renderValueBox({
    valueBox(
      paste0(df_total_19()$change, "%"), subtitle = "2019-2020", color = "red")
  })
  
  # Sentence about changes
  output$total_sentence_change <- renderText({ 
    paste0("Between 2018 and 2019, the number of prison ", df_total_18()$adm_or_pop_lc, " ",
           df_total_18()$change_type, "d ", df_total_18()$change,
           "%. In 2020, the number of prison ",  df_total_19()$adm_or_pop_lc, " ",
           df_total_19()$change_type, "d ", df_total_19()$change, "%.")
  })
  
  ###
  # Supervision Violation Admissions
  ###
  # filter data
  df_supviols_18 <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               year == "2019" &
               metric == "Supervision Violations")
  })
  
  # Supervision Violation 
  # Since 2018
  output$viol_change_18 <- renderValueBox({
    valueBox(
      paste0(df_supviols_18()$change, "%"), subtitle = "2018-2019", color = "green")
  })
  
  # filter data
  df_supviols_19 <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               year == "2020" &
               metric == "Supervision Violations")
  })
  
  # Supervision Violation 
  # Since 2019
  output$viol_change_19 <- renderValueBox({
    valueBox(
      paste0(df_supviols_19()$change, "%"), subtitle = "2019-2020", color = "red")
  })
  
  # Sentence about changes
  output$viol_sentence_change <- renderText({ 
    paste0("Between 2018 and 2019, the number of supervision violation ", df_supviols_18()$adm_or_pop_lc, " ",
           df_supviols_18()$change_type, "d ", df_supviols_18()$change,
           "%. In 2020, the number of supervision violation ",  df_supviols_19()$adm_or_pop_lc, " ",
           df_supviols_19()$change_type, "d ", df_supviols_19()$change, "%.")
  })
  
  ##################################
  # 2nd plot titles
  ##################################
  
  # Print adm or pop selected
  output$prob_title <- renderText({ 
    paste("Probation ", input$adm_or_pop, "by Type")
  })
  
  # Print adm or pop selected for sup viols
  output$parole_title <- renderText({ 
    paste("Parole ", input$adm_or_pop, "by Type")
  })
  
  ##############################################################################
  ##############################################################################
  # Probation and parole plots
  ##############################################################################
  ##############################################################################
  
  ############
  # Barplot for probation
  ############
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
    
    dodger = position_dodge(width = 0.9)
    
    ggplot(data = df_prob,
           aes_string(x = 'year', y = 'total', fill = 'metric')) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label=scales::comma(total)),
                position=dodger,
                size = 5.5,
                colour = "#000000",
                vjust = -0.5) +
      theme_csgjc_plot_legend() +
      theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      scale_fill_manual(values = c("#DEF0F6", "#E18731"),
                        name = "") +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.17*max(df_prob$total)),
                         expand = c(0,0)
      ) +
      coord_cartesian(clip = "off")
    
  })
  
  ############
  # Donut chart for probation
  ############
  output$donutchart_prob <- renderGirafe({ 
    
    # filter data depending on input
    df_donutchart_prob <- 
      adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               prob_vs_parole == "Probation" &
               year == 2020) %>% 
      filter(metric == "New Offense" | metric == "Technical") %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) 
    
    # sum 
    df_donutchart_prob$overall <- sum(df_donutchart_prob$total)
    
    # create variables
    df_donutchart_prob <- df_donutchart_prob %>% group_by(metric) %>% 
      mutate(percentage = total / overall,
             hover_text = paste0(metric, ": ", total)) %>%
      mutate(percentage_label = paste0(round(100 * percentage, 0), "%")) %>% 
      select(-overall)
    
    # make dataframe
    df_donutchart_prob <- as.data.frame(df_donutchart_prob)
    
    # donut plot
    donut_plot_prob <- ggplot(df_donutchart_prob, aes_string(y = 'total', fill = 'metric')) +
      geom_bar_interactive(
        aes(x = 1, tooltip = hover_text),
        width = 0.5,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = df_donutchart_prob[["percentage_label"]][df_donutchart_prob[["metric"]] == "Technical"],
        size = 20,
        color = "#000000"
      ) +
      scale_fill_manual(values = c(`New Offense` = "#DEF0F6", Technical = "#E18731")) +
      coord_polar(theta = "y") +
      theme_void() +
      ggtitle("Technical Violations \n In 2020") +
      theme(plot.title = element_text(size = 40,
                                      hjust = 0.5))
    
    ggiraph(ggobj = donut_plot_prob)
    
  })
  
  ############
  # Barchart for parole
  ############
  
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
    
    dodger = position_dodge(width = 0.9)
    
    ggplot(data = df_parole,
           aes_string(x = 'year', y = 'total', fill = 'metric')) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label=scales::comma(total)),
                position=dodger,
                size = 5.5,
                colour = "#000000",
                vjust = -0.5) +
      theme_csgjc_plot_legend() +
      theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      scale_fill_manual(values = c("#DEF0F6", "#E18731"),
                        name = "") +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.17*max(df_parole$total)),
                         expand = c(0,0)
      ) +
      coord_cartesian(clip = "off")

  })
  
  ############
  # Donut chart for parole
  ############
  
  output$donutchart_parole <- renderGirafe({ 
    
    # filter data depending on input
    df_donutchart_parole <- 
      adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               prob_vs_parole == "Parole" &
               year == 2020) %>% 
      filter(metric == "New Offense" | metric == "Technical") %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) 
    
    # sum 
    df_donutchart_parole$overall <- sum(df_donutchart_parole$total)
    
    # create variables
    df_donutchart_parole <- df_donutchart_parole %>% group_by(metric) %>% 
      mutate(percentage = total / overall,
             hover_text = paste0(metric, ": ", total)) %>%
      mutate(percentage_label = paste0(round(100 * percentage, 0), "%")) %>% 
      select(-overall)
    
    # make dataframe
    df_donutchart_parole <- as.data.frame(df_donutchart_parole)
    
    # donut plot
    donut_plot_parole <- ggplot(df_donutchart_parole, aes_string(y = 'total', fill = 'metric')) +
      geom_bar_interactive(
        aes(x = 1, tooltip = hover_text),
        width = 0.5,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = df_donutchart_parole[["percentage_label"]][df_donutchart_parole[["metric"]] == "Technical"],
        size = 20,
        color = "#000000"
      ) +
      scale_fill_manual(values = c(`New Offense` = "#DEF0F6", Technical = "#E18731")) +
      coord_polar(theta = "y") +
      theme_void() +
      ggtitle("Technical Violations \n In 2020") +
      theme(plot.title = element_text(size = 40,
                                      hjust = 0.5))
    
    ggiraph(ggobj = donut_plot_parole)
    
  })
  
  ##################################
  # Value boxes
  ##################################
  
  # Probation
  # Since 2018
  df_prob <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop &
             year == "2019" &
             metric == "Probation")
  })
  
  output$prob_change_18 <- renderValueBox({
    
    valueBox(paste0(df_prob()$change, "%"), subtitle = "2018-2019", color = "green")
    
  })
  
  # Probation
  # Since 2019
  df_prob_2 <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop &
             year == "2020" &
             metric == "Probation")
  })
  
  output$prob_change_19 <- renderValueBox({
    
    valueBox(paste0(df_prob_2()$change, "%"), subtitle = "2019-2020", color = "green")
    
  })
  
  # Sentence about probation changes
  output$prob_change_sentence <- renderText({ 
    paste0("Between 2018 and 2019, the number of probation violation ", df_prob()$adm_or_pop_lc, " ",
           df_prob()$change_type, "d ", df_prob()$change,
           "%. In 2020, the number of probation violation ",  df_prob_2()$adm_or_pop_lc, " ",
           df_prob_2()$change_type, "d ", df_prob_2()$change, "%.")
  })
  
  # Parole
  # Since 2018
  df_parole <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               year == "2019" &
               metric == "Parole")
  })
  
  output$parole_change_18 <- renderValueBox({
    
    valueBox(paste0(df_parole()$change, "%"), subtitle = "2018-2019", color = "green")
    
  })
  
  # Parole
  # Since 2019
  df_parole_2 <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               year == "2020" &
               metric == "Parole")
  })
  
  output$parole_change_19 <- renderValueBox({
    
    valueBox(paste0(df_parole_2()$change, "%"), subtitle = "2019-2020", color = "green")
    
  })
  
  # Sentence about Parole changes
  output$parole_change_sentence <- renderText({ 
    paste0("Between 2018 and 2019, the number of parole violation ", df_parole()$adm_or_pop_lc, " ",
           df_parole()$change_type, "d ", df_parole()$change,
           "%. In 2020, the number of parole violation ",  df_parole_2()$adm_or_pop_lc, " ",
           df_parole_2()$change_type, "d ", df_parole_2()$change, "%.")
  })
  
  #__________________________________________________________________________________________________________________________________________________________
  # 3) View Data
  #__________________________________________________________________________________________________________________________________________________________
  
  output$dt <- 
    DT::renderDataTable(
      datatable(mclc_datatable,
                filter = "top"),
      server = FALSE
    )
  
  output$filtered_row <- 
    renderPrint({
      input[["dt_rows_all"]]
    })
  
  output$download_filtered <- 
    downloadHandler(
      filename = function() {
        paste('mclc-filtered-', Sys.Date(), '.csv', sep='')
      },
      content = function(file){
        write.csv(mclc_datatable[input[["dt_rows_all"]], ],
                  file)
      }
    )
  
  #__________________________________________________________________________________________________________________________________________________________
  # Map - Counts
  #__________________________________________________________________________________________________________________________________________________________
  
  # print map title
  output$map_title_counts <- renderText({ 
    paste(input$data_map_counts, " ", input$adm_or_pop_map_counts, " in ", input$year_map_counts)
  })
  
  # create map
  output$map_counts <- renderLeaflet({
    
    # filter data
    df_map_counts <- mclc %>% 
      dplyr::filter(adm_or_pop == input$adm_or_pop_map_counts &
                    year == input$year_map_counts &
                    metric == input$data_map_counts)
    
    # merge data with shp file
    df_map_counts <- sp::merge(us_aea2, df_map_counts, by.x = 'NAME', by.y = "states", all = F)
    
    # get quantile breaks
    breaks_qt <- classIntervals(c(min(df_map_counts$total) - .00001, df_map_counts$total), 
                                n = 7, 
                                style = "quantile",
                                dataPrecision = 0)
    
    # round up to nearest 10th
    breaks_qt$brks <- round(breaks_qt$brks, digits = -1)
    
    # create a palette function
    pal_fun <- colorQuantile(palette = "Blues", domain = NULL, na.color = "#D3D3D3", n = 7)
    
    # # set colors manually:
    # pal_fun <- colorFactor(
    #   palette = c("#2A5B71", "#387A96", "#4698Bc", "#6BADC9", "#B5D6E4", "#DEF0F6",
    #               "#E9F4D6", "#A5D35C", "#8FC833", "#72A029", "#56781F"),
    #   domain = df_map_counts$total,
    #   na.color = "#D3D3D3" # light gray color for NA
    # )
    
    # pal_fun <- colorQuantile(
    #   palette = colorRampPalette(c('#DEF0F6', '#4698BC'))(length(df_map_counts$total)), 
    #   domain = df_map_counts$total,
    #   na.color = "#D3D3D3", # light gray color for NA
    #   n = 6)
    
    # add popup
    df_map_counts$popup_text <- 
      paste0('<strong>', df_map_counts$NAME, '</strong>',
             '<br/>', '<strong>','Count: ', '</strong>', df_map_counts$total, sep = "", ' ') %>% 
      lapply(htmltools::HTML)
    
    # # format labels
    # labels <- sprintf(
    #   "<strong>%s</strong><br/>%s",
    #   states$name, prettyNum(states$density, big.mark = ",")
    # ) %>% lapply(htmltools::HTML)
    
    # create leaflet map
    map_counts <- leaflet(data = df_map_counts,
                          options = leafletOptions(zoomControl = TRUE,
                                                   minZoom = 2, 
                                                   maxZoom = 4.75,
                                                   dragging = TRUE)) %>% 
      
      setView(lng = -96.25, lat = 29.50, zoom = 4.75) %>%
      
      addPolygons(fillColor = ~pal_fun(total), 
                  fillOpacity = 1, 
                  weight = 1, 
                  color = "#C4D9ED", 
                  popup = df_map_counts$popup_text,
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = "#4698BC")
      ) %>% 
      
      # addLegend(pal = pal_fun, 
      #           opacity = 1,
      #           values = df_map_counts$total)
      
      addLegend(position = "topright",
                colors = brewer.pal(7, "Blues"),
                opacity = 1,
                labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2, big.mark = ",")),
                title = "<strong>Count</strong>")
      
    map_counts
    
  })
  
  #__________________________________________________________________________________________________________________________________________________________
  # Map - Changes
  #__________________________________________________________________________________________________________________________________________________________
  
  # print map title
  output$map_title_change <- renderText({ 
    paste("Change in ", input$data_map_change, " ", input$adm_or_pop_map_change, "in ", input$year_map_change)
  })
 
  # create map
  output$map_change <- renderLeaflet({
    
    # filter data
    df_map_change <- mclc_change %>% 
      dplyr::filter(adm_or_pop == input$adm_or_pop_map_change &
                    year == input$year_map_change &
                    metric == input$data_map_change)
    
    # merge data with shp file
    df_map_change <- sp::merge(us_aea2, df_map_change, by.x = 'NAME', by.y = "states", all = F)
    
    # create a palette function
    pal_fun <- colorNumeric(palette = "Blues", domain = df_map_change$change, na.color = "#D3D3D3")
    
    # use the palette function created above to add the appropriate RGB value to our dataframe
    df_map_change$color <- pal_fun(df_map_change$change)
    
    # add popup
    df_map_change$popup_text <- 
      paste0('<strong>', df_map_change$NAME, '</strong>',
             '<br/>', '<strong>','Change: ', '</strong>', df_map_change$change, "%", sep = "", ' ') %>% 
      lapply(htmltools::HTML)
    
    # create leaflet map
    map_change <- leaflet(data = df_map_change,
                          options = leafletOptions(zoomControl = TRUE,
                                                   minZoom = 2, 
                                                   maxZoom = 4.75,
                                                   dragging = TRUE)) %>% 
      
      setView(lng = -96.25, lat = 29.50, zoom = 4.75) %>%
      
      addPolygons(fillColor = ~pal_fun(change), 
                  fillOpacity = 1, 
                  weight = 1, 
                  color = "#C4D9ED", 
                  popup = df_map_change$popup_text,
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = "#4698BC")
      ) %>% 
      
      addLegend(position = "topright",
                pal = pal_fun,
                opacity = 1,
                values = df_map_change$change,
                labFormat = labelFormat(suffix="%"),
                title = "<strong>% Change</strong>")
    
    map_change
    
  })
  
} #server
