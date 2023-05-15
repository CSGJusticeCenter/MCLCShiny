#######################################
# Project: MCLCShiny
# File: server.R
# Authors: Mari Roberts
# Date last updated: May 3, 2023 (MAR)
# Description:
#    Server for shiny app
#######################################


#######################################
# Project: MCLCShiny
# File: ui.R
# Authors: Mari Roberts
# Date last updated: May 3, 2023 (MAR)
# Description:
#    User interface for shiny app
#######################################


ui <- fluidPage(

  navbarPage(id = "navbarID",


             # Map Explorer Page
             tabPanel("nationalmaps", id = "nationalmaps",


                          fluidRow(column(width = 3),
                                   column(width = 6,
                                   selectInput('data_map',
                                               div(style = "font-weight: bold",
                                                   "Select Metric"),
                                               choices = c("Total",
                                                           "New Offense Violation",
                                                           "Supervision Violation",
                                                           "Probation Violation",
                                                           "Parole Violation",
                                                           "Technical Violation"),
                                               multiple = FALSE)),
                                   column(width = 3)
                          ), # end fluidRow
                      br(),

                          #######
                          # Hex map
                          #######

                          fluidRow(column(width = 1),
                                   column(width = 10,
                                          align = "center",
                                          div(id = "hex-map",
                                              highchartOutput("hex_map",
                                                              height = 550,
                                                              width = "100%"))),
                                   column(width = 1)),
                          br(), br()
             ), # end tabPanel

             ##############################################################################################################################

             # State Reports Page
             tabPanel("statereports", id = "statereports",

                      #######
                      # Dropdown and download buttons
                      #######
                          fluidRow(column(width = 3),
                                   column(width = 6,
                                          fluidRow(column(width = 2),

                                                   # Drop Down - Select State
                                                   column(width = 4, align = "center",
                                                          class = "input-col",
                                                          labeled_input('input-btn', "",
                                                                        div(id = 'state-selector',
                                                                            selectInput('state_report',
                                                                                        div(style = "font-weight: bold",
                                                                                            "Select State"),
                                                                                        choices = unique(adm_pop_long$state),
                                                                                        multiple = FALSE)))),

                                                   # Drop Down - Select Admissions or Population
                                                   column(width = 4, align = "center", class = "input-col",
                                                          labeled_input('input-btn', "",

                                                                        div(id = "type-selector",
                                                                            selectInput('adm_pop_report',
                                                                                        div(style = "font-weight: bold",
                                                                                            "Select Type"),
                                                                                        choices = c("Admissions",
                                                                                                    "Population"),
                                                                                        selected = "Admissions",
                                                                                        multiple = FALSE)))),
                                                   column(width = 2))),
                                   column(width = 3)

                          ), # fluidRow
                      br()

             ))
)

server <- function(input, output, session) {


  #################################################################################
  #################################################################################

  # Redirect to state page depending on state selected in hex map

  # Reactive values for state selected on map
  selected_state_map <- reactiveVal(NULL)

  # Map clicking
  click_js <- JS("function(event) {
    var state = event.point.name;
    Shiny.onInputChange('selected_state_map', state);
    $('#tabs a[href=\"#tabs-2\"]').tab('show');
  }")

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


  #################################################################################
  #################################################################################

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

  ##############################################################################################################################
  # MAP EXPLORER
  ##############################################################################################################################

  #######
  # Hex map
  #######

  df1 <-  reactive({mclc_explorer %>%
    filter(adm_or_pop == input$adm_or_pop_map,
           year       == input$year_map,
           metric     == input$data_map)})

  # Select foundational hex map and store it as a reactive expression
  # Charts were created in highchart.R
  # This is necessary to download the map
  foundational_map <- reactive({

    # Get minimum and maximum value
    min_map <- min(df1()$change, na.rm = TRUE)
    max_map <- max(df1()$change, na.rm = TRUE)

    # Get absolute value for comparison
    min_map_abs <- abs(min_map)
    max_map_abs <- abs(max_map)

    # Get neg or pos sign for min and max
    min_map_type <- ifelse(min_map >= 0, "positive", "negative")
    max_map_type <- ifelse(max_map >= 0, "positive", "negative")

    # Determine the new min and max where all values are negative
    NEW_MAX <- max_map
    NEW_MIN <- min_map

    highchart() %>%

      hc_chart(
        spacingTop = 1,
        spacingRight = 1,
        spacingBottom = 1,
        spacingLeft = 1) %>%

      hc_add_series_map(
        map = hex_gj,
        df = df1(),
        joinBy = "state_abb",
        value = "change",
        dataLabels = list(enabled = TRUE, format = "{point.datalabel}",
                          style = list(fontSize = "14px",
                                       fontWeight = "regular",
                                       fontFamily = "Graphik",
                                       textOutline = 0)),
        nullColor = "#e8e8e8") %>%

      hc_colorAxis(min = NEW_MIN,
                   max = NEW_MAX,
                   stops = color_stops(4, c(darkblue, regblue, lightblue, white)),
                   labels = list(format = "{value}%",
                                 style = list(fontSize = "14px"))) %>%

      hc_legend(align = "right",
                verticalAlign = "bottom",
                layout = "vertical",
                symbolHeight = 200,
                symbolWidth = 25,
                x = -25,
                y = 0) %>%

      hc_plotOptions(series = list(events = list(click = click_js)))

  })

  # output hex map
  output$hex_map <- renderHighchart({
    foundational_map()
  })

  ##############################################################################################################################
  # State Reports
  ##############################################################################################################################

  #######
  # State page title
  #######

  # Title of state based on user input
  output$selected_state <- renderText({
    if (input$adm_pop_report == "Admissions") {
      paste("Prison Admission Trends in", input$state_report)
    } else if (input$adm_pop_report == "Population") {
      paste("Prison Population Trends in", input$state_report)
    } else {
      ""
    }
  })

}

# launch shiny app
shinyApp(ui = ui, server = server)
