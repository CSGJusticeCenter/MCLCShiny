library(shiny)
library(highcharter)
library(dplyr)

ui <- fluidPage(

  navbarPage(
    "Hex Map",
    id = "navbarID",
    tabPanel("nationalmaps",
             highchartOutput("hex_map")),
    tabPanel("statereports",
             selectInput("state_report", "Select State", choices = NULL))
  )
)

server <- function(input, output, session) {

  # Change URL depending on tab selection in navbar
  observeEvent(input$navbarID, {

    newURL <- paste0(
      session$clientData$url_protocol,
      "//",
      session$clientData$url_hostname,
      ":",
      session$clientData$url_port,
      session$clientData$url_pathname,
      "#",
      input$navbarID
    )
    updateQueryString(newURL, mode = "replace", session)
  })
  observe({
    currentTab <- sub("#", "", session$clientData$url_hash)
    if(!is.null(currentTab)){
      updateTabItems(session, "navbarID", selected = currentTab)
    }
  })

  # reactive values
  selected_state_map <- reactiveVal(NULL)

  # sample data
  data_4_map <- reactive({
    df1 <- mclc_explorer %>%
      filter(adm_or_pop == "Admissions",
             year       == "2019 - 2020",
             metric     == "Total")
  })


  # map clicking
  click_js <- JS("function(event) {
    var state = event.point.state_abb;
    Shiny.setInputValue('selected_state_map', state);
    $('#navbarID a[href=\"#statereports\"]').tab('show');
  }")

  foundational_map <- reactive({
    # Get minimum and maximum value
    min_map <- min(data_4_map()$change, na.rm = TRUE)
    max_map <- max(data_4_map()$change, na.rm = TRUE)

    # Get absolute value for comparison
    min_map_abs <- abs(min_map)
    max_map_abs <- abs(max_map)

    # Get neg or pos sign for min and max
    min_map_type <- ifelse(min_map >= 0, "positive", "negative")
    max_map_type <- ifelse(max_map >= 0, "positive", "negative")

    # Generate tile map
    # Has diverging scales when there are neg and pos values which centers the color gradient at zero
    # Has a gradient scale when both the min and max are both negative or both positive

    # Determine the new min and max so that zero is centered
    # For example, If the highest positive value is 20 than the negative value is -20

    NEW_MAX <- case_when(
      max_map_abs > min_map_abs ~ max_map_abs,
      max_map_abs < min_map_abs ~ min_map_abs,
      max_map_abs == min_map_abs ~ max_map_abs)
    NEW_MIN <- case_when(
      min_map_abs > max_map_abs ~ min_map_abs,
      min_map_abs < max_map_abs ~ max_map_abs,
      min_map_abs == max_map_abs ~ min_map_abs)
    NEW_MAX <- ifelse(max_map_type == "negative", -abs(NEW_MAX), abs(NEW_MAX))
    NEW_MIN <- ifelse(min_map_type == "negative", -abs(NEW_MIN), abs(NEW_MIN))

      highchart() %>%

        hc_chart(
          spacingTop = 1,
          spacingRight = 1,
          spacingBottom = 1,
          spacingLeft = 1) %>%

        hc_add_series_map(
          map = hex_gj,
          df = data_4_map(),
          joinBy = "state_abb",
          value = "change",
          dataLabels = list(enabled = TRUE, format = "{point.datalabel}",
                            style = list(fontSize = "14px",
                                         fontWeight = "regular",
                                         fontFamily = "Graphik",
                                         textOutline = 0)),
          nullColor = "#e8e8e8") %>%

        hc_legend(align = "right",
                  verticalAlign = "bottom",
                  layout = "vertical",
                  symbolHeight = 200,
                  symbolWidth = 25,
                  x = -25,
                  y = 0) %>%


        hc_plotOptions(series = list(animation = FALSE,
                                     cursor = "pointer",
                                     borderWidth = 3))


  })

  # map
  output$hex_map <- renderHighchart({
    foundational_map()%>%
      hc_plotOptions(series = list(events = list(click = click_js)))
  })

  # redirect to the State tab and update selected state
  observeEvent(input$selected_state_map, {
    selected_state_map(input$selected_state_map)
    updateNavbarPage(session, "navbarID", selected = "statereports")
  })

  # update selectInput choices based on selected state
  observeEvent(selected_state_map(), {
    updateSelectInput(session, "state_report", selected = selected_state_map(),
                      choices = ifelse(is.null(selected_state_map()), NULL, selected_state_map()))
  })

}

shinyApp(ui, server)
