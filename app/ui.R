
ui <- fluidPage(
  
  # GLOBAL START (HEADER) ######################################################
  
  includeCSS("www/theme.css"), # include custom CSS
  navbarPage(
    id = "navbarID",
    
    # Formats light blue header
    tags$style(type = "text/css",".container-fluid {padding-left:0px; padding-right:0px;}"),
    tags$style(type = "text/css",".navbar {margin-bottom: .5px;}"),
    tags$style(type = "text/css",".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}"),
    
    # Hide errors on user-end
    # tags$style(type="text/css",
    #   ".shiny-output-error { visibility: hidden; }",
    #   ".shiny-output-error:before { visibility: visible; content: ''; }"),

    # App title - Accessible
    # tags$head(tags$title("More Community, Less Confinement Dashboard")),
    title = "More Community, Less Confinement Dashboard",
    
    # English - Accessible
    tags$html(lang="en"),
             
    # NATL TRENDS HEX MAP  #######################################################
    tabPanel("nationaltrends",
      # header with dropdowns and download button ------------------------------
      div(
        id = "header",
        fluidRow(
          column(width = 3),
          column(width = 6,fluidRow(
            # Select Metric
            column(
              width = 3,
              align = "center",
              class = "input-col",
              labeled_input('input-btn', "",
                selectInput('data_map',
                  label = "Select Metric",
                  choices = c("Total",
                              "New Offense Violation",
                              "Supervision Violation",
                              "Probation Violation",
                              "Parole Violation",
                              "Technical Violation"),
                  multiple = FALSE
                )
              )
            ),
            # Select Adm or Pop
            column(
              width = 3,
              align = "center",
              class = "input-col",
              labeled_input('input-btn', "",
                selectInput('adm_or_pop_map',
                  label = "Select Metric Type",
                  choices = c("Admissions", "Population"),
                  multiple = FALSE
                )
              )
            ),
            # Select Year Change
            column(
              width = 3,
              align = "center",
              class = "input-col",
              labeled_input('input-btn', "",
                selectInput('year_map',
                  label = "Select Year Change",
                  choices = c('2018 - 2019, 1 year' = "2018 - 2019",
                              '2019 - 2020, 1 year' = "2019 - 2020",
                              '2020 - 2021, 1 year' = "2020 - 2021",
                              '2018 - 2023, 6 years' = "2018 - 2023"),
                  selected = "2018 - 2021",
                  multiple = FALSE
                )
              )
            ),
            # Download Map
            column(
              width = 3,
              align = "center",
              class = "input-col",
              labeled_input('save-map-btn', "",
                downloadButton(outputId = 'save_map',
                  label = "Download Map",
                  class = "download-btn-lg"
                )
              )
            )
          )), # end fluidRow<column 
          column(width = 3)
        ) # end fluidRow with columns 
      ), # end div header
      br(), # end header with dropdowns and download button 
      
      # body with hex map and table  ------------------------------
      div(id = "app-body",
        # HEx MAP 
        fluidRow(column(
          width = 12,
          align = "center",
          div(class = "hidden-xs hidden-sm",
            #id = "hex-map",
            highchartOutput("hex_map", height = 600, width = "100%") |> withSpinner()
          )
        )),
        br(), br(),
        # HEX MAP TABLE
        fluidRow(column(
          width = 12,
          align = "center",
          div(
            id = "selected-map-table",
            textOutput("selected_map_table")
          )
        )),
        br(),
        tags$head(tags$style(HTML("thead{color: #004270; font-size: 16px}"))),
        fluidRow(column(
          width = 12, 
          align = "left",
          div(
            id = "reactable-table",
            reactableOutput("table_map")
          )
        )),
        br(), br()
      ) # end div app-body 
    ), # end tabPanel END NATL TRENDS HEX MAP 

     ############################################################################################################################## State Reports

     # State Reports Page
     tabPanel("statedashboard",

              #######
              # Dropdown and download buttons
              #######

              div(id = "app-header",
                  fluidRow(column(width = 3),
                           column(width = 6,
                                  fluidRow(column(width = 2),

                                           # Drop Down - Select State
                                           column(width = 4,
                                                  align = "center",
                                                  class = "input-col",
                                                  labeled_input('input-btn', "",
                                                  div(id = 'state-selector',
                                                       selectInput('state_report',
                                                                   label = "Select State",
                                                                   choices = state.name,
                                                                   multiple = FALSE)))),

                                           # Drop Down - Select Admissions or Population
                                           column(width = 4,
                                                  align = "center",
                                                  class = "input-col",
                                                  labeled_input('input-btn', "",
                                                  div(id = "type-selector",
                                                       selectInput('adm_pop_report',
                                                                   label = "Select Type",
                                                                   choices = c("Admissions", "Population"),
                                                                   selected = "Admissions",
                                                                   multiple = FALSE)))),
                                           column(width = 2))),
                           column(width = 3)

                           ) # fluidRow
              ), # end div header

              br(),

              div(id = "app-body",

                  # State title
                  fluidRow(column(width = 1),
                           column(width = 10,
                                  div(id = "selected-state",
                                      textOutput("selected_state"),
                                      "aria-label" = "Selected State")),
                           column(width = 1)),
                  br(),

                  #######
                  # Value boxes
                  #######
                  fluidRow(column(width = 1),
                           column(width = 10,
                                  fluidRow(
                                    column(width = 3,
                                           valueBoxOutput("total_change",
                                                          width = "100%")
                                           ),
                                    column(width = 3,
                                           valueBoxOutput("sup_change",
                                                          width = "100%") |> withSpinner()
                                           ),
                                    column(width = 3,
                                           valueBoxOutput("tech_change",
                                                          width = "100%") |> withSpinner()
                                           ),
                                    column(width = 3,
                                           valueBoxOutput("new_off_change",
                                                          width = "100%")
                                           )
                                    )
                                  ),
                           column(width = 1)),
                  br(), br(),

                  #######
                  # Panels for Overview, Parole, Probation, and Race/Ethnic Disparities
                  #######

                  fluidRow(column(width = 1),
                           column(width = 10,

                                  tabsetPanel(selected = "1",
                                              type = "tabs",
                                              id = "tabsetpanel",

                                    tabPanel(value="1","Overview",

                                             br(),

                                             # Area graph and download button
                                             fluidRow(column(width = 5,
                                                             align = "center",
                                                             uiOutput("state_area") |> withSpinner()),
                                                      column(width = 1, align = "center",
                                                             uiOutput("state_area_button")),
                                             # Supervision violation graph and download button
                                                      column(width = 5, align = "center",
                                                             uiOutput("state_nt") |> withSpinner()),
                                                      column(width = 1, align = "center",
                                                             uiOutput("state_nt_button"))),
                                             br(), br(), br(),

                                             # State overview table
                                             fluidRow(column(width = 12,
                                                             align = "center",
                                                             div(id = "reactable-table",
                                                                 reactableOutput("state_table") |> withSpinner()
                                                             ))),

                                             br(),

                                             # States notes
                                             div(id = "state-note-section",
                                                 fluidRow(column(width = 12,
                                                                 align = "center",
                                                                 div(id = "selected-state-note-title",
                                                                     "State Notes"))),
                                                 br(), br(),

                                                 fluidRow(column(width = 1),
                                                          column(width = 5,
                                                                 div(id = "selected-state-note-subtitle",
                                                                     "Probation Metrics"),
                                                                 div(id = "selected-state-note",
                                                                     htmlOutput("state_probation_notes"),
                                                                     htmlOutput("state_probation_asterisks_notes"))),
                                                          column(width = 5,
                                                                 div(id = "selected-state-note-subtitle",
                                                                     "Parole/Post-Incarceration Metrics"),
                                                                 div(id = "selected-state-note",
                                                                     htmlOutput("state_parole_notes"),
                                                                     htmlOutput("state_parole_asterisks_notes"))),
                                                          column(width = 1)),

                                                 fluidRow(column(width = 1),
                                                          column(width = 10,
                                                                 align = "left",
                                                                 div(id = "selected-state-note",
                                                                     htmlOutput("state_additional_notes"))),
                                                          column(width = 1)),
                                             ),

                                             br(), br()

                                    ), # end tabPanel

                                    tabPanel(value="2","Parole",

                                             br(),
                                             # Parole graph and download button depending on data availability
                                             uiOutput("parole_nt"),
                                             br(), br(), br(),
                                             # Parole reactable table
                                             fluidRow(column(width = 12,
                                                             align = "center",
                                                             div(id = "reactable-table",
                                                                 reactableOutput("parole_table") |> withSpinner()
                                                             ))),
                                             br(), br()

                                    ), # end tabPanel

                                    tabPanel(value="3","Probation",

                                             br(),
                                             # Probation graph and download button depending on data availability
                                             uiOutput("probation_nt"),
                                             br(), br(), br(),
                                             # Probation reactable table
                                             fluidRow(column(width = 12,
                                                             align = "center",
                                                             div(id = "reactable-table",
                                                                 reactableOutput("probation_table") |> withSpinner()
                                                             ))),
                                             br(), br()

                                    ), # end tabPanel

                                  ) # end tabsetPanel
                           ), # end column
                           column(width = 1)
                  ), # end fluidRow

                  br(), br()

              ) # end div

     ), # end tabPanel

     ############################################################################################################################## Download Data

     # Download data page
     tabPanel("downloaddata",

              #######
              # Dropdowns
              #######

              div(id = "app-header",

                  fluidRow(column(width = 3),
                           column(width = 6,

                                  fluidRow(# Select State(s)
                                           column(width = 3, align = "center", class = "input-col",
                                                  labeled_input('input-btn', "",
                                                                pickerInput(inputId = 'download_state',
                                                                            width = "100%",
                                                                            choices = NULL,
                                                                            selected = NULL,
                                                                            multiple = TRUE,
                                                                            div(style = "font-weight: bold", "Select State(s)"),
                                                                            options = list(`actions-box` = TRUE,
                                                                                           style = "picker-style")))),

                                           # Select Metric(s)
                                           column(width = 3, align = "center", class = "input-col",
                                                  labeled_input('input-btn', "",
                                                                pickerInput(inputId = 'download_metric',
                                                                            width = "100%",
                                                                            choices = NULL,
                                                                            selected = NULL,
                                                                            multiple = TRUE,
                                                                            div(style = "font-weight: bold", "Select Metric(s)"),
                                                                            options = list(`actions-box` = TRUE,
                                                                                           style = "picker-style")))),
                                           # Select Year(s)
                                           column(width = 3, align = "center", class = "input-col",
                                                  labeled_input('input-btn', "",
                                                                pickerInput(inputId = 'download_year',
                                                                            width = "100%",
                                                                            choices = NULL,
                                                                            selected = NULL,
                                                                            multiple = TRUE,
                                                                            div(style = "font-weight: bold", "Select Year(s)"),
                                                                            options = list(`actions-box` = TRUE,
                                                                                           style = "picker-style")))),
                                           # Download Data
                                           column(width = 3, align = "center", class = "input-col",
                                                                downloadButton(outputId = 'save_data', "Download Data",
                                                                               class = "download-btn-lg")))),
                           column(width = 3)

                  ) # fluidRow
              ), # end div header

              br(),

              #######
              # Download table
              #######

              div(id = "app-body",

                  fluidRow(column(width = 2),
                           column(width = 8, div(id = "download-title",
                                                 "Download Data")),
                           column(width = 2)),

                  br(),

                  fluidRow(column(width = 2),
                           column(width = 8, div(id = "download-info",
                                                 "To understand the impact of community supervision
                                                 (i.e., probation, parole, post-release supervision) on state prison populations,
                                                 The Council of State Governments (CSG) Justice Center surveyed corrections
                                                 leaders in all 50 states. This project was supported by Arnold Ventures and
                                                 produced in partnership with the Correctional Leaders Association (CLA).
                                                 The resulting data spans 4 years—from 2018 to 2021—and demonstrate how the number
                                                 of people sent to prison for supervision violations changed.")),
                           column(width = 2)),
                  br(),

                  fluidRow(column(width = 2),
                           column(width = 8,
                                  align = "center",
                                  div(id = "selected-download-table",
                                      reactableOutput("selected_download_table"))),
                           column(width = 2)),

                  br(), br()

              ) # end div

     ) # end tabPanel

# GLOBAL END (FOOTER) ######################################################
)) # end navbarPage<fluidPage
