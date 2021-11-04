server <- function(input, output, session) { 
  
  #_____________________________________________________________________________
  # 1) About
  #_____________________________________________________________________________
  
  # no code needed
  
  #_____________________________________________________________________________
  # 2) Dashboard
  #_____________________________________________________________________________
  
  # 2A-B) Subset data
  dataFilter <- reactive({
    adm_pop_long %>% 
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop &
             metric == "All Supervision")
  })
  
  # 2B) Print state name and adm or pop selected
  output$selected_state <- renderText({ 
    paste("Prison ", input$adm_or_pop, "Trends in ", input$state)
  })
  
  # 2B) Print state name and adm or pop selected
  output$selected_state_adm_pop <- renderText({ 
    paste("States across the country saw changes in their prison admissions and 
          populations due to supervision violations in 2020. But some states were 
          already experiencing reductions in violation admissions and population 
          prior to the pandemic. This snapshot shows available prison ", 
          input$adm_or_pop, "data for ", input$state, " from 2018 to 2020.")
  })
  
  #########
  # 2C) Bar chart
  #########
  output$barchart_2 <- renderPlot({
    
    ggplot(data = dataFilter(), 
           aes_string(x = 'year', y = 'total', fill = 'year')) + 
      # barchart
      geom_bar(stat = "identity", width = 0.70) +
      # title
      ggtitle("Total Supervision Violations from 2018 to 2020 \n") +
      # custom style
      theme_csgjc_plot() +
      # colors and legend
      scale_fill_manual(values = c(blue2, blue3, blue4),
                        name = "") +
      # y axis commas in labels
      scale_y_continuous(label = scales::comma) +
      # labels
      geom_text(aes(label = scales::comma(total)),
                vjust = -.5,
                size = 4.25) +
      # bar sizes
      theme(aspect.ratio = 0.75)
    
  }, width = 450, height = 400)
  
  #########
  # 2C) Area chart
  #########
  
  # create reactive data frame for new offense and technical supervision
  # make year a factor variable
  # for some reason, year is coming out as "2018.0" "2018.5" etc.
  dataFilter_2 <- reactive({
    adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "New Offense" | metric == "Technical") %>% 
      mutate(year = as.factor(year))
  })
  
  # create reactive data frame for new offense and technical supervision
  # just 2020 for labeling purposes
  dataFilter_2a <- reactive({
    adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "New Offense" | metric == "Technical") %>% 
      filter(year == "2020") %>% 
      mutate(order = ifelse(metric == "Technical", 1, 2)) %>% 
      arrange(order) %>% 
      mutate(cum = cumsum(total))
  })
  
  output$areachart_2 <- renderPlot({
    
    ggplot(dataFilter_2(), 
           aes(x=as.numeric(as.character(year)), 
               y=total, 
               fill = metric)) + 
      geom_area() + 
      # # add labels directly to plot
      # geom_text(data=dataFilter_2a(), aes(x=2020, y=cum, label=metric),
      #           size = 4.25,
      #           hjust = -.05, 
      #           # vjust = 5,
      #           position = position_stack(vjust = 0.5),
      #           check_overlap = TRUE
      #           ) +
      # # extend x axis so labels fit
      xlim(2018 - 0.1, 2020 + 0.1) +
      theme_csgjc_plot() +
      # title
      ggtitle("Supervision Violations by Type from 2018 to 2020 \n") +
      # custom style
      theme_csgjc_plot_legend() +
      # colors and legend
      scale_fill_manual(values = c(blue2, orange),
                        name = "") +
      # y and x axis labels
      scale_y_continuous(label = scales::comma) +
      # labels
      geom_text(aes(label = scales::comma(total)),
                position = position_stack(vjust = 0.5),
                check_overlap = TRUE,
                size = 4.25)
    # geom_text(data=dataFilter_2b(),aes(x = year,label=ifelse(year == min(year),total)),position = position_stack(vjust = 0.5), check_overlap = TRUE) +
    # geom_text(data=dataFilter_2b(),aes(x = year,label=ifelse(year == max(year),total)),position = position_stack(vjust = 0.5), check_overlap = TRUE)
    
  }, width = 450, height = 400)
  
  #_____________________________________________________________________________
  # 3) View Data
  #_____________________________________________________________________________
  
  output$table <- renderDT(mclc,
                           filter = "top",
                           options = list(
                           pageLength = 50
                           ))
  
  #_____________________________________________________________________________
  # 4) Map
  #_____________________________________________________________________________
  
  
  
}