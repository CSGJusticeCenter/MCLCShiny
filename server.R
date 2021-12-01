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
  
  # filter data
  dataFilter_4 <- reactive({
    mclc.df <- mclc_change %>% 
      dplyr::filter(adm_or_pop == input$adm_or_pop_map &
                    year == input$year_map &
                    metric == input$data_map)
    mclc.df <- sp::merge(us_aea2, mclc.df, by.x = 'NAME', by.y = "states", all=F)
  })
  
  # # set colors manually:
  # paletteNum <- colorFactor(
  #   palette = c("#2A5B71", "#387A96", "#4698Bc", "#6BADC9", "#B5D6E4", "#DAEAF2",
  #               "#E9F4D6", "#A5D35C", "#8FC833", "#72A029", "#56781F"),
  #   domain = mclc_change$change,
  #   na.color = "#D3D3D3" # light gray color for NA
  # )
  
  # color palette
  paletteNum <- colorNumeric(palette = "Blues", domain = NULL)
  
  # leaflet map
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     minZoom = 4.5, 
                                     maxZoom = 4.5,
                                     dragging = FALSE)) %>%  
      
      # map template
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(opacity = 0)) %>%
      
      # set view to US
      setView(lng = -96.25, lat = 39.50, zoom = 4.5) %>%
      
      addPolygons(data = dataFilter_4(),
                  
                  # colors
                  opacity = 1.0,
                  color = 'white',
                  weight = 1,
                  fillOpacity = 0.9, 
                  smoothFactor = 0.3,
                  fillColor = ~paletteNum(dataFilter_4()$change),
                  
                  # highlight options
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = "#355DA1"
                  )) 
    
  }) #renderLeaflet
  
} #server
