#######################################
# Project: MCLCShiny
# File: server.R
# Authors: Mari Roberts
# Date: November 11, 2021
# Description: 
#    Server
#######################################

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
  # Headers
  #########
  
  # # Print adm or pop selected
  # output$adm_pop_header <- renderText({ 
  #   paste("Overall ", input$adm_or_pop)
  # })
  # 
  # # Print adm or pop selected for sup viols
  # output$viol_header <- renderText({ 
  #   paste("Supervision Violation ", input$adm_or_pop)
  # })
  
  #########
  # Plot titles
  #########
  
  # Print adm or pop selected
  output$adm_pop_title <- renderText({ 
    paste("Overall ", input$adm_or_pop, "by Type")
  })
  
  # Print adm or pop selected for sup viols
  output$viol_title <- renderText({ 
    paste("Supervision Violation ", input$adm_or_pop, "by Type")
  })
  
  #########
  # Bar chart about total admissions or population
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
  
  # get max y limit value
  ymax <- reactive({
    adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "Supervision Violations" | metric == "Other") %>% 
      ungroup() %>% 
      top_n(1)
  })
  
  output$barchart <- renderPlot({
    
    ggplot(dataFilter(), 
           aes_string(fill='metric', y='total', x='year')) + 
      geom_bar(position="dodge", stat="identity", width = .5) + 
      # labels
      geom_text(aes(label=scales::comma(total)),
                position=position_dodge(0.5),
                size = 4.25,
                vjust = -0.5,
                colour = "#000000") +
      # ggtitle("\n") +
      # custom style
      theme_csgjc_plot_legend() +
      # colors and legend
      scale_fill_manual(values = c("#1C3D4B", "#4698BC"),
                        name = "") +
      # y axis commas in labels
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.04*max(dataFilter()$total)),
                         expand = c(0,0)
      ) +
      coord_cartesian(clip = "off")
    
  }, width = 450, height = 350)
  
  #########
  # Bar chart about supervision violations
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
      geom_bar(stat = "identity", position = "dodge", width = .5) +
      # ggtitle("\n") +
      # custom style
      theme_csgjc_plot_legend() +
      # colors and legend
      scale_fill_manual(values = c("#B5D6E4", "#6BADC9"),
                        name = "") +
      # labels
      geom_text(aes(label = scales::comma(total)), 
                position = position_dodge(width = 0.5),
                vjust = -0.25,
                size = 4.25,
                colour = "#000000") +
      # y axis commas in labels
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.04*max(dataFilter_2()$total)),
                         expand = c(0,0)
      ) +
      coord_cartesian(clip = "off")
    
  }, width = 450, height = 350)
  
  #########
  # Value boxes
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
      paste0(dataFilter_2b()$change, "%"), subtitle = "Since 2018", color = "green")
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
      paste0(dataFilter_2c()$change, "%"), subtitle = "Since 2019", color = "red")
  })
  
  # Sentence about changes
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
      paste0(dataFilter_2d()$change, "%"), subtitle = "Since 2018", color = "green")
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
      paste0(dataFilter_2e()$change, "%"), subtitle = "Since 2019", color = "red")
  })
  
  # Sentence about changes
  output$viol_sentence_change <- renderText({ 
    paste0("Since 2018, the number of supervision violation ", dataFilter_2d()$adm_or_pop_lc, " ",
           dataFilter_2d()$change_type, "d ", dataFilter_2d()$change,
           "%. In 2020, the number of supervision violation ",  dataFilter_2e()$adm_or_pop_lc, " ",
           dataFilter_2e()$change_type, "d ", dataFilter_2e()$change, "%.")
  })
  
  #_____________________________________________________________________________
  # 3) View Data
  #_____________________________________________________________________________
  
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
  
  #_____________________________________________________________________________
  # 4) Map
  #_____________________________________________________________________________
  
  # print map title
  output$map_title <- renderText({ 
    paste("Change in ", input$data_map, " ", input$adm_or_pop_map, "in ", input$year_map)
  })
  
  # create map
  output$map <- renderLeaflet({
    
    df_map <- mclc_change %>% 
          dplyr::filter(adm_or_pop == input$adm_or_pop_map &
                        year == input$year_map &
                        metric == input$data_map)
    df_map <- sp::merge(us_aea2, df_map, by.x = 'NAME', by.y = "states", all = F)
    
    # create a palette function
    palette <- colorNumeric(palette = "Blues", domain = df_map$change, na.color = "#D3D3D3")
    
    # use the palette function created above to add the appropriate RGB value to our dataframe
    df_map$color <- palette(df_map$change)
    
    # add popup
    df_map$popup_text <- 
      paste0('<strong>', df_map$NAME, '</strong>',
             '<br/>', '<strong>','Change: ', '</strong>', df_map$change,"%", sep = "", ' ') %>% 
      lapply(htmltools::HTML)
    
    # create leaflet map
    map_1 <- leaflet(data = df_map,
                     options = leafletOptions(zoomControl = FALSE,
                                              minZoom = 4.5, 
                                              maxZoom = 4.5,
                                              dragging = FALSE)) %>% 
      
      setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
      
      addPolygons(fillColor = df_map$color, 
                  fillOpacity = 1, 
                  weight = 1, 
                  color = "#C4D9ED", 
                  popup = df_map$popup_text,
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = "#FFFFFF")
                  ) %>% 
      
      addLegend(position = "bottomright",
                pal = palette,
                opacity = 0.7,
                values = df_map$change,
                labFormat = labelFormat(suffix="%"),
                title = "<strong>% Change</strong>")
    map_1

  })
  
} #server
