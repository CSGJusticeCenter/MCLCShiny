#######################################
# Project: MCLCShiny
# File: regional_map.R
# Authors: Mari Roberts
# Date: September 27, 2021
# Description: 
#    Creates 50 state interactive map
#######################################

# UI
ui <- shinyUI(fluidPage(theme = shinytheme("united"),
                        titlePanel(HTML("<h1><center><font size=14> Regional Map </font></center></h1>")), 
                        sidebarLayout(
                          sidebarPanel(
                            
                            ##############
                            ##############
                            selectizeInput(
                              "stateInput", 'State', choices = "", multiple = FALSE,
                              options = list(
                                placeholder = 'Please select a state from below')
                            ),
                            ##############
                            ##############
                            
                            
                            ##############
                            ##############
                            # selectInput("dataInput", label = h3("Data"),
                            #             choices = c("Total Admissions",
                            #                         "Supervision Violation Admissions",
                            #                         "Probation Violation Admissions",
                            #                         "New Offense Probation Violation Admissions",
                            #                         "Technical Probation Violation Admissions",
                            #                         "Parole Violation Admissions",
                            #                         "New Offense Parole Violation Admissions",
                            #                         "Technical Parole Violation Admissions",
                            #                         "Total Population",
                            #                         "Supervision Violation Population",
                            #                         "Probation Violation Population",
                            #                         "New Offense Probation Violation Population",
                            #                         "Technical Probation Violation Population",
                            #                         "Parole Violation Population",
                            #                         "New Offense Parole Violation Population",
                            #                         "Technical Parole Violation Population"
                            #                         )),
                            ##############
                            ##############
                            
                            ##############
                            ##############
                            selectInput("yearInput", label = h3("Year"),
                                        choices = c("2018",
                                                    "2019",
                                                    "2020"))
                            ##############
                            ##############
                            
                            ),
                          mainPanel(
                            
                            ##############
                            ##############
                            leafletOutput(outputId = 'map', 
                                          height = 800) 
                            ##############
                            ##############
                            
                          ))
))


# SERVER
server <- shinyServer(function(input, output, session) {
  
  # selected state
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
  
  # selected year
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
  
})

# Run app! 
shinyApp(ui = ui, server = server)
