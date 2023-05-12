library(shiny)
library(highcharter)
library(dplyr)

ui <- fluidPage(
  tags$script(src = "https://code.highcharts.com/mapdata/countries/us/us-all.js"),

  navbarPage(
    "Hex Map",
    id = "navbarID",
    tabPanel("Hex Map",
             highchartOutput("hcmap")),
    tabPanel("State",
             selectInput("state_report", "Select State", choices = NULL))
  )
)

server <- function(input, output, session) {
  # reactive values
  selected_state_map <- reactiveVal(NULL)

  # sample data
  data_4_map <- download_map_data("countries/us/us-all") %>%
    get_data_from_map() %>%
    select(`hc-key`) %>%
    mutate(value = round(100 * runif(nrow(.)), 2))

  # map clicking
  click_js <- JS("function(event) {
    var stateName = event.point.name;
    Shiny.onInputChange('selected_state_map', stateName);
    $('#tabs a[href=\"#tabs-2\"]').tab('show');
  }")

  # map
  output$hcmap <- renderHighchart({
    hcmap(map = "countries/us/us-all",
          data =  data_4_map,
          value = "value",
          joinBy = "hc-key",
          name = "Pop",
          download_map_data = FALSE) %>%
      hc_colorAxis(stops = color_stops()) %>%
      hc_plotOptions(series = list(events = list(click = click_js)))
  })

  # redirect to the State tab and update selected state
  observeEvent(input$selected_state_map, {
    selected_state_map(input$selected_state_map)
    updateNavbarPage(session, "navbarID", selected = "State")
  })

  # update selectInput choices based on selected state
  observeEvent(selected_state_map(), {
    updateSelectInput(session, "state_report", selected = selected_state_map(),
                      choices = ifelse(is.null(selected_state_map()), NULL, selected_state_map()))
  })

}

shinyApp(ui, server)
