ui <- fluidPage(fluidRow(
                column(width = 4,
                       
                       selectInput(inputId = "adm_or_pop", label = "Admissions/Population", 
                                   choices = unique(adm_pop_long$adm_or_pop)),
                       selectInput(inputId = "data", label = "Data", 
                                   choices = unique(adm_pop_long$metric)),
                       sliderInput("year", "Year",
                                   min = 2018, max = 2020,
                                   value = 2020),
                       textOutput("header")
                ), #column
                column(width = 8,
                       
                       leafletOutput(outputId = "map", height = 600)
                       
                       )
          ) #fluidRow
) #fluidPage


server <- function(input, output, session) { 
  
  # header based on inputs
  output$header <- renderText({ 
    paste(input$data, " ", input$adm_or_pop, " in ", input$state, " in ", input$year)
  })
  
  # Subset data
  dataFilter <- reactive({
    adm_pop_long %>% 
      filter(adm_or_pop == input$adm_or_pop,
             year == input$year,
             metric == input$data)
  })
  
}



# run app
shinyApp(ui = ui, server = server)
