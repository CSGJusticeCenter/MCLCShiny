#######################################
# Project: MCLCShiny
# File: ui.R
# Authors: Mari Roberts
# Date last updated: May 3, 2023 (MAR)
# Description:
#    User interface for shiny app
#######################################


ui <- fluidPage(

  # Add the shinyjs library for enabling caching
  shinyjs::useShinyjs(),

  useConductor(),
  navbarPage(id = "navbarID",

             # Formats light blue header
             tags$style(type = "text/css",
                        ".container-fluid {padding-left:0px; padding-right:0px;}"),
             tags$style(type = "text/css",
                        ".navbar {margin-bottom: .5px;}"),
             tags$style(type = "text/css",
                        ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}"),

             # Hide errors on user-end
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: visible; content: ''; }"),

             # App title
             title = "MCLC Dashboard",

             # English
             tags$html(lang="en"),

             ##############################################################################################################################

             # Map Explorer Page
             tabPanel("nationalmaps",

                      div(id = "header",

                          #######
                          # Dropdown and download buttons
                          #######

                          fluidRow(column(width = 3),
                                   column(width = 6,
                                          fluidRow(# Select Metric
                                            column(width = 3,
                                                   align = "center",
                                                   class = "input-col",
                                                   labeled_input('input-btn', "",
                                                                 selectInput('data_map',
                                                                             div(style = "font-weight: bold",
                                                                                 "Select Metric"),
                                                                             choices = c("Total",
                                                                                         "New Offense Violation",
                                                                                         "Supervision Violation",
                                                                                         "Probation Violation",
                                                                                         "Parole Violation",
                                                                                         "Technical Violation"),
                                                                             multiple = FALSE))),
                                            # Select Adm or Pop
                                            column(width = 3,
                                                   align = "center",
                                                   class = "input-col",
                                                   labeled_input('input-btn', "",
                                                                 selectInput('adm_or_pop_map',
                                                                             div(style = "font-weight: bold",
                                                                                 "Select Metric Type"),
                                                                             choices = c("Admissions",
                                                                                         "Population"),
                                                                             multiple = FALSE))),
                                            # Select Year Change
                                            column(width = 3,
                                                   align = "center",
                                                   class = "input-col",
                                                   labeled_input('input-btn', "",
                                                                 selectInput('year_map',
                                                                             div(style = "font-weight: bold",
                                                                                 "Select Year Change"),
                                                                             choices = c('2018 - 2019, 1 year' = "2018 - 2019",
                                                                                         '2019 - 2020, 1 year' = "2019 - 2020",
                                                                                         '2020 - 2021, 1 year' = "2020 - 2021",
                                                                                         '2018 - 2021, 4 years' = "2018 - 2021"),
                                                                             selected = "2018 - 2021",
                                                                             multiple = FALSE))),
                                            # Download Map
                                            column(width = 3,
                                                   align = "center",
                                                   class = "input-col",
                                                   labeled_input('save-map-btn', "",
                                                                 downloadButton(outputId = 'save_map',
                                                                                "Download Map",
                                                                                class = "download-btn-lg")))
                                          ) # end fluidRow
                                   ),
                                   column(width = 3)
                          ) # end fluidRow
                      ), # end div header
                      br(),

                      div(id = "app-body",

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
                          br(), br(),

                          #######
                          # Hex map table
                          #######

                          fluidRow(column(width = 1),
                                   column(width = 10,
                                          align = "center",
                                          div(id = "selected-map-table",
                                              textOutput("selected_map_table"))),
                                   column(width = 1)),
                          br(),
                          tags$head(tags$style(HTML("thead{color: #004270; font-size: 16px}"))),
                          fluidRow(column(width = 1),
                                   column(width = 10, align = "left",
                                          div(id = "table-map",
                                              reactableOutput("table_map"))),
                                   column(width = 1)),
                          br(), br()

                      ) # end div
             ), # end tabPanel

             ##############################################################################################################################

             # State Reports Page
             tabPanel("statereports",

                      #######
                      # Dropdown and download buttons
                      #######

                      div(id = "app-header",
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

                          ) # fluidRow
                      ), # end div header

                      br()

  ))
)

# launch shiny app
shinyApp(ui = ui, server = server)
