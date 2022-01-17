server <- function(input, output, session) {
  
  #-------------------------------------------------------------------------------
  # View and download data
  #-------------------------------------------------------------------------------

  output$table_out <- DT::renderDataTable(
    datatable(data = mclc_datatable, 
              extensions = 'Buttons',
              filter = "top",
              selection = 'single', 
              options = list( 
                 dom = "Blfrtip", 
                 buttons = 
                   list("copy", list(
                     extend = "collection", buttons = c("csv", "excel", "pdf"), 
                     text = "Download")), # end of buttons customization
                 # customize the length menu
                 lengthMenu = list(c(25, 50, 100, -1), # declare values
                                   c(25, 50, 100, "All") # declare titles
                 ), # end of lengthMenu customization
                 pageLength = 25
               ) # end of options
    ) 
  )
  
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
    
    title <- paste0("Prison ", input$adm_or_pop,"\n")
    
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
      geom_text(size = 4.5, aes(x = year + 0.15,label=ifelse(year == min(year),scales::comma(total), NA), fill = NULL),     data = totals_tech, position = position_stack(vjust = 1.22), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(label=ifelse(year != min(year) & year != max(year),scales::comma(total), NA), fill = NULL), data = totals_tech, position = position_stack(vjust = 1.22), check_overlap = TRUE) +
      geom_text(size = 4.5, aes(x = year - 0.15,label=ifelse(year == max(year),scales::comma(total), NA), fill = NULL),     data = totals_tech, position = position_stack(vjust = 1.26), check_overlap = TRUE) +
      # colors
      scale_fill_manual(values = c(total_co, viol_co, tech_co), 
                        labels = c("Total", "Supervision Violation", "Technical Violation"), 
                        breaks = c("Other", "Supervision Violations", "Technical"),
                        name = "") +  
      scale_x_continuous(breaks = c(1,2,3), labels = c("            2018", "2019", "2020             ")) 
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
    
  })
  
  # prob parole bar and line chart
  output$barchart_parole <- renderPlot({
    
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
      geom_line(data = entry, aes(x = year, y= total), size=1.25, color = "#7B898F")+
      geom_line(data = exit, aes(x = year, y= total), size=1.25, color = "#FA9F8D")+
      geom_text(data = areas, aes(x = year, y = total, label = scales::comma(total)),
                position=position_dodge(0.8), vjust = -0.6, size = 5) +
      scale_fill_manual(values=par_cols) +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.15*max(areas$total)),
                         expand = c(0,0)) +
      ggtitle(title)+
      theme_csgjc_prob_parole()
    
  })
  
  # prob parole bar and line chart
  output$barchart_prob <- renderPlot({
    
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
      geom_line(data = entry, aes(x = year, y= total), size=1.25, color = "#7B898F")+
      geom_line(data = exit, aes(x = year, y= total), size=1.25, color = "#FA9F8D")+
      geom_text(data = areas, aes(x = year, y = total, label = scales::comma(total)),
                position=position_dodge(0.8), vjust = -0.6, size = 5) +
      scale_fill_manual(values=prob_cols) +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.15*max(areas$total)),
                         expand = c(0,0)) +
      ggtitle(title)+
      theme_csgjc_prob_parole()
    
  })
  
  #######################################View data for state

  
  
}
