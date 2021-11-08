server <- function(input, output, session) { 
  
  #_____________________________________________________________________________
  # 1) About
  #_____________________________________________________________________________
  
  # no code needed
  
  #_____________________________________________________________________________
  # 2) Dashboard
  #_____________________________________________________________________________
  
  #########
  # 2) Main header and paragraph
  #########
  
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
  
  #########
  # 2) Headers
  #########
  
  # Print adm or pop selected
  output$adm_pop_header <- renderText({ 
    paste("Total ", input$adm_or_pop)
  })
  
  # Print adm or pop selected for sup viols
  output$viol_header <- renderText({ 
    paste("Supervision Violation ", input$adm_or_pop)
  })
  
  #########
  # 2) Bar chart about total admissions or population
  #########
  
  # Subset data
  dataFilter <- reactive({
    adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "Supervision Violations" | metric == "Other")  
  })
  
  output$barchart <- renderPlot({
    
    ggplot(data = dataFilter(),
           aes_string(x = 'year', y = 'total', fill = 'metric')) +
      # barchart
      geom_bar(stat = "identity", position = "stack", width = 0.70) +
      # title
      ggtitle("\n") +
      # custom style
      theme_csgjc_plot_legend() +
      # colors and legend
      scale_fill_manual(values = c("#DEF0F6", "#B05D24"),
                        name = "") +
      # y axis commas in labels
      scale_y_continuous(label = scales::comma) +
      # labels
      geom_text(aes(label = scales::comma(total)), 
                position = position_stack(0.5),
                size = 4.25,
                colour = ifelse(dataFilter()$metric == "Other", "black", "white")) +
      # bar sizes
      theme(aspect.ratio = 0.75)

  }, width = 450, height = 400)
  
  #########
  # 2) Bar chart about supervision violations
  #########
  
  # Subset data
  dataFilter_2 <- reactive({
    adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "Technical" | metric == "New Offense")  
  })
  
  output$barchart_2 <- renderPlot({
    
    ggplot(data = dataFilter_2(),
           aes_string(x = 'year', y = 'total', fill = 'metric')) +
      # barchart
      geom_bar(stat = "identity", position = "stack", width = 0.70) +
      # title
      ggtitle("\n") +
      # custom style
      theme_csgjc_plot_legend() +
      # colors and legend
      scale_fill_manual(values = c("#DEF0F6", "#B05D24"),
                        name = "") +
      # y axis commas in labels
      scale_y_continuous(label = scales::comma) +
      # labels
      geom_text(aes(label = scales::comma(total)), 
                position = position_stack(0.5),
                size = 4.25,
                colour = ifelse(dataFilter()$metric == "Other", "black", "white")) +
      # bar sizes
      theme(aspect.ratio = 0.75)
    
  }, width = 450, height = 400)
  
  #########
  # 2) Value boxes
  #########

  ###
  # Total 
  ###
  # filter data
  dataFilter_2b <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop &
             year == "2019" &
             data == "total_admissions")
  })

  # Total 
  # Since 2018
  output$total_change_18 <- renderValueBox({
    valueBox(
      paste0(dataFilter_2b()$change, "%"), subtitle = "Since 2018")
  })

  # filter data
  dataFilter_2c <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop &
             year == "2020" &
             data == "total_admissions")
  })

  # Total 
  # Since 2019
  output$total_change_19 <- renderValueBox({
    valueBox(
      paste0(dataFilter_2c()$change, "%"), subtitle = "Since 2019")
  })

  # 2) Print adm or pop selected
  output$total_sentence_change <- renderText({ 
    paste0("Since 2018, the number of prison ", dataFilter_2b()$adm_or_pop_lc, " ",
           dataFilter_2b()$change_type, "d ", dataFilter_2b()$change,
           "%. In 2020, the number of prison ",  dataFilter_2c()$adm_or_pop_lc, " ",
           dataFilter_2c()$change_type, "d ", dataFilter_2c()$change, "%.")
  })
  
  ###
  # Supervision Violation Admissions
  ###
  # filter data
  dataFilter_2d <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               year == "2019" &
               data == "total_violation_admissions")
  })
  
  # Supervision Violation 
  # Since 2018
  output$viol_change_18 <- renderValueBox({
    valueBox(
      paste0(dataFilter_2d()$change, "%"), subtitle = "Since 2018")
  })
  
  # filter data
  dataFilter_2e <- reactive({
    adm_pop_long %>%
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               year == "2020" &
               data == "total_violation_admissions")
  })
  
  # Supervision Violation 
  # Since 2019
  output$viol_change_19 <- renderValueBox({
    valueBox(
      paste0(dataFilter_2e()$change, "%"), subtitle = "Since 2019")
  })
  
  # 2) Print adm or pop selected
  output$viol_sentence_change <- renderText({ 
    paste0("Since 2018, the number of prison ", dataFilter_2d()$adm_or_pop_lc, " ",
           dataFilter_2d()$change_type, "d ", dataFilter_2d()$change,
           "%. In 2020, the number of prison ",  dataFilter_2e()$adm_or_pop_lc, " ",
           dataFilter_2e()$change_type, "d ", dataFilter_2e()$change, "%.")
  })
  
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