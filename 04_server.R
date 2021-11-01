#######################################
# Project: MCLCShiny
# File: server.R
# Authors: Mari Roberts
# Date: September 27, 2021
# Description: 
#    Server for R Shiny app
#######################################
server <- function(input, output, session) {
  
  ###############
  # 1) About____________________________________________________________________
  ###############
  
  # no code needed
  
  ###############
  # 2) Dashboard________________________________________________________________
  ###############
  
  # 2A-C) Subset data
  dataFilter <- reactive({
    adm_pop_long %>% 
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop &
             metric == input$metric)
  })
  
  # 2D) Print state name selected
  output$selected_state <- renderText({ 
    input$state
  })
  
  # 2E) Print graph title
  output$selected_var <- renderText({ 
    paste(input$metric, " Violations from 2018 to 2020")
  })
  
  # 2E) Bar chart
  output$barchart <- renderPlot({
    
    ggplot(data = dataFilter(), 
           aes_string(x = 'year', 
                      y = 'total', 
                      fill = 'year')) + 
      
      # barchart
      geom_bar(stat = "identity", width = 0.75) +
      
      # style
      theme_minimal() + 
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.5, 
                                      face = "bold",
                                      size = 18,
                                      colour = "#000000"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 12, 
                                       colour = "#000000"),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
      ) +
      
      # colors and legend
      scale_fill_manual(values = c(blue2, blue3, blue4),
                        name = "") +
      scale_y_continuous(label = scales::comma) +
      # labels
      # geom_text(aes(label = stat(y), group = year), stat = 'summary', fun = sum, 
      #           vjust = -.5,
      #           size = 4) +
      geom_text(aes(label = scales::comma(total)),
                vjust = -.5,
                size = 4.25) +
      
      # bar sizes
      theme(aspect.ratio = 1)
    
  }, width = 450, height = 450)
  
  ###############
  # 3) View Data________________________________________________________________
  ###############
  
  output$table <- renderDT(mclc,
                           filter = "top",
                           options = list(
                           pageLength = 50
                           ))
  
  ###############
  # 4) Map______________________________________________________________________
  ###############
  
  #####
  # 4a) Selected state
  #####
  
  updateSelectizeInput(session, "stateInput", choices = mclc.df$NAME,
                       server = TRUE)
  
  # selected state
  selectedState <- reactive({
    mclc.df[mclc.df$NAME == input$stateInput, ] 
  })
  
  # # selected metric
  # selectedData <- reactive({
  #   mclc.df[mclc.df$NAME == input$dataInput, ] 
  # })
  
  #####
  # 4b) Selected year
  #####
  selectedYear <- reactive({switch(input$yearInput, 
                                   "2018"=mclc.df$total_admissions_2018, 
                                   "2019"=mclc.df$total_admissions_2019, 
                                   "2020"=mclc.df$total_admissions_2020)
  })

  # color palette
  pal2 <- colorNumeric(palette = "Blues", domain=NULL)
  
  # leaflet map
  output$map <- renderLeaflet({
    leaflet(mclc.df, 
            # remove zoom
            options = leafletOptions(doubleClickZoom= FALSE)) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      setView(lng = -98.583, lat = 39.833, zoom = 3.5) %>%
      # add popups
      addPolygons(data = mclc.df ,fillColor = ~pal2(selectedYear()),
                  popup = paste0("<strong>State: </strong>", 
                                 mclc.df$NAME),
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 1)
    
  })
  
  # popup based on state
  observeEvent(input$stateInput, {
    state_popup <- paste0("<strong>State: </strong>", 
                          selectedState()$NAME
                          # "<br><strong>2018: </strong>",
                          # selectedYear()$total_admissions_2018,
                          # "<br><strong>2019: </strong>",
                          # selectedYear()$total_admissions_2019,
                          # "<br><strong>2020: </strong>",
                          # selectedYear()$total_admissions_2020
                          )
    
  # selected state turns orange
  leafletProxy("map", data = selectedState()) %>%
      clearGroup(c("st.ate")) %>%
      addPolygons(group ="st.ate",fillColor = "orange",
                  popup = state_popup,
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 5)
  })
  
  ###############
  # 5) Dashboard________________________________________________________________
  ###############
  
  # 5A-B) Subset data
  dataFilter <- reactive({
    adm_pop_long %>% 
      filter(states == input$state_2 &
               adm_or_pop == input$adm_or_pop_2 &
               metric == "Total")
  })
  
  # 5C) Print state name selected
  output$selected_state_2 <- renderText({ 
    input$state_2
  })
  
  # 5D) Print graph title
  output$selected_adm_pop_2 <- renderText({ 
    paste(input$adm_or_pop_2, "")
  })
  
  #########
  # 5D) Bar chart
  #########
  output$barchart_2 <- renderPlot({
    
    ggplot(data = dataFilter(), 
           aes_string(x = 'year', 
                      y = 'total', 
                      fill = 'year')) + 
      
      # barchart
      geom_bar(stat = "identity", width = 0.70) +
      
      ggtitle("Total Supervision Violations from 2018 to 2020 \n") +
      
      # style
      theme_minimal() + 
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.5, 
                                      # face = "bold",
                                      size = 18,
                                      colour = "#000000"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 12, 
                                       colour = "#000000"),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
      ) +
      
      # colors and legend
      scale_fill_manual(values = c(blue2, blue3, blue4),
                        name = "") +
      scale_y_continuous(label = scales::comma) +
      # labels
      # geom_text(aes(label = stat(y), group = year), stat = 'summary', fun = sum, 
      #           vjust = -.5,
      #           size = 4) +
      geom_text(aes(label = scales::comma(total)),
                vjust = -.5,
                size = 4.25) +
      
      # bar sizes
      theme(aspect.ratio = 0.75)
    
  }, width = 450, height = 400)
  
  #########
  # 5E) Area chart
  #########
  
  # subset data
  dataFilter_2 <- reactive({
    adm_pop_long %>% 
      filter(states == input$state_2 &
             adm_or_pop == input$adm_or_pop_2 &
             metric == "Total")
  })
  
  output$areachart_2 <- renderPlot({
    

    
  }, width = 450, height = 400)
}