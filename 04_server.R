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
  # 1) About
  ###############
  
  # no code needed
  
  ###############
  # 2) Dashboard
  ###############
  
  # 2)A-C Subset data
  dataFilter <- reactive({
    adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop &
               metric == input$metric)
  })
  
  # 2)A Print name of state selected in 
  output$state_choice <- renderText({
    input$state 
  })
  
  # 2)D Barchart plot
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
                                      size = 16),
            axis.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
      ) +
      
      # colors and legend
      scale_fill_manual(values = c(blue2, blue3, blue4),
                        name = "") +
      
      # labels
      geom_text(aes(label = stat(y), group = year), stat = 'summary', fun = sum, 
                vjust = -.5,
                size = 4) +
      
      # bar sizes
      theme(aspect.ratio = 1)
    
  }, width = 450, height = 450)
  
  
  ###############
  # 3) View Data
  ###############
  
  output$table <- renderDT(mclc,
                           filter = "top",
                           options = list(
                           pageLength = 50
                           ))
  
  ###############
  # 4) Map
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
  
  output$map <- renderLeaflet({
    leaflet(mclc.df) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addPolygons(data = mclc.df ,fillColor = ~pal2(selectedYear()),
                  popup = paste0("<strong>State: </strong>", 
                                 mclc.df$NAME),
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 1)
    
  })
  
  observeEvent(input$stateInput, {
    state_popup <- paste0("<strong>State: </strong>", 
                          selectedState()$NAME, 
                          "<br><strong>% of smoking adults in 2015: </strong>",
                          selectedState()$total_admissions_2018,
                          "<br><strong>% of smoking adults in 2016: </strong>",
                          selectedState()$total_admissions_2019,
                          "<br><strong>% of smoking adults in 2017: </strong>",
                          selectedState()$total_admissions_2020)
    
    leafletProxy("map", data = selectedState()) %>%
      clearGroup(c("st.ate")) %>%
      addPolygons(group ="st.ate",fillColor = "orange",
                  popup = state_popup,
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 5)
  })
      
}