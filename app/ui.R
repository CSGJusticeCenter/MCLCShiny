
ui <- fluidPage(
  
  # GLOBAL START (HEADER) ######################################################
  theme = "theme.css", 
  # includeCSS("www/theme.css"), # include custom CSS
  navbarPage(
    id = "navbarID",
    
    # Accessibility tags 
    tags$head(tags$title("Supervision Violations and Their Impact on Incarceration")),
    tags$html(lang="en"),
    
    #title = "Supervision Violations and Their Impact on Incarceration", # title for navigaton bar, navbar is hidden so not applicable 
    
    # Formats light blue header
    tags$style(type = "text/css",".container-fluid {padding-left:0px; padding-right:0px;}"),
    tags$style(type = "text/css",".navbar {margin-bottom: .5px;}"),
    tags$style(type = "text/css",".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}"),
    
    # Hide errors on user-end
    # tags$style(type="text/css",
    #   ".shiny-output-error { visibility: hidden; }",
    #   ".shiny-output-error:before { visibility: visible; content: ''; }"),
             
    # NATL TRENDS HEX MAP  #####################################################
    tabPanel("nationaltrends",
      # natl trends header -----------------------------------------------------
      div(
        id = "header",
        fluidRow(
          column(width = 3),
          column(width = 6,fluidRow(
            column(# Select Metric
              width = 3,
              align = "center",
              class = "input-col",
              labeled_input('input-btn', "",
                selectInput('data_map',
                  label = "Select Metric",
                  choices = metric_opts,
                  multiple = FALSE
                )
              )
            ),
            column(# Select Type (Adm or Pop)
              width = 3,
              align = "center",
              class = "input-col",
              labeled_input('input-btn', "",
                selectInput('adm_or_pop_map',
                  label = "Select Metric Type",
                  choices = type_opts,
                  multiple = FALSE
                )
              )
            ),
            column(# Select Year Change
              width = 3,
              align = "center",
              class = "input-col",
              labeled_input('input-btn', "",
                selectInput('year_map',
                  label = "Select Year Change",
                  choices = yrchg_opts,
                  selected = "2018 - 2023",
                  multiple = FALSE
                )
              )
            ),
            column( # Download Map
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
      
      # natl trends app body ---------------------------------------------------
      div(id = "app-body",
        fluidRow(column( # HEx MAP 
          width = 12,
          align = "center",
          div(class = "hidden-xs hidden-sm",
            #id = "hex-map",
            highchartOutput("hex_map", height = 600, width = "100%") |> withSpinner()
          ) # end div 
        )), # end column<fluidRow HEX MAP 
        br(), br(),
        fluidRow(column( # HEX MAP TABLE
          width = 12,
          align = "center",
          div(
            id = "selected-map-table",
            textOutput("selected_map_table")
          ) # end div 
        )), # end column<fluidRow HEX MAP TABLE 
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
    
    
    # STATE DASHBOARDS #########################################################
    
    tabPanel("statedashboard",
      # state dashboard header -------------------------------------------------
      div(id = "app-header",
        fluidRow(
          column(width = 3),
          column(width = 6,
            fluidRow(
              column(width = 2),
              column( # SELECT STATE 
                width = 4,
                align = "center",
                class = "input-col",
                labeled_input('input-btn', "",
                  div(
                    id = 'state-selector',
                    selectInput('state_report',
                      label = "Select State",
                      choices = state.name,
                      multiple = FALSE
                    ) # end select input 
                  ) # end div 
                ) # end labeled_input
              ), # end column: SELECT STATE 
              column( # SELECT TYPE (ADM OR POP) 
                width = 4,
                align = "center",
                class = "input-col",
                labeled_input('input-btn', "",
                  div(
                    id = "type-selector",
                    selectInput('adm_pop_report',
                      label = "Select Type",
                      choices = type_opts,
                      selected = "Admissions",
                      multiple = FALSE
                    ) # end select input 
                  ) # end div 
                ) # end labeled_input
              ), # end column: SELECT TYPE (ADM OR POP) 
              column(width = 2)
            ) # end fluid row 
          ), #end column width 6 
          column(width = 3)
        ) # fluidRow
      ), # end div app-header
      br(),
      # state header app body -------------------------------------------------
      div(id = "app-body",
        # 0 state title --------------------------------------------------------
        fluidRow(
          column(width = 1),
          column(
            width = 10,
            div(id = "selected-state",
            textOutput("selected_state"), "aria-label" = "Selected State")
          ),
          column(width = 1)
        ), # end fluidRow title 
        br(),
        
        # 0 value boxes --------------------------------------------------------
        fluidRow(
          column(width = 1),
          column(
            width = 10,
            fluidRow( # TODO: add spinners to all boxes? 
              column(width = 3, valueBoxOutput("total_change",  width = "100%")),
              column(width = 3, valueBoxOutput("sup_change",    width = "100%") |> withSpinner()),
              column(width = 3, valueBoxOutput("tech_change",   width = "100%") |> withSpinner()),
              column(width = 3, valueBoxOutput("new_off_change",width = "100%"))
            )
          ),
          column(width = 1)
        ), #end fluidRow value boxes 
        br(), br(),
        
        # START PANELS (STATE TABS) --------------------------------------------
        fluidRow(
          column(width = 1),
          column(width = 10,
            tabsetPanel( # STATE TABS 
              selected = "1", 
              type = "tabs", 
              id = "tabsetpanel",
              # 1 overview -----------------------------------------------------
              tabPanel(
                value="1",
                "Overview",
                br(),
                fluidRow( # SIDE BY SIDE PLOTS 
                  column( # area chart  
                    width = 5,
                    align = "center",
                    uiOutput("state_area") |> withSpinner()
                  ),
                  column( # area chart download button 
                    width = 1, 
                    align = "center",
                    uiOutput("state_area_button")
                  ),
                  column( # supervision bar chart 
                    width = 5, 
                    align = "center",
                    uiOutput("state_nt") |> withSpinner()
                  ),
                  column( # supervision bar chart download button 
                    width = 1, 
                    align = "center",
                    uiOutput("state_nt_button")
                  )
                ), # end fluidrow END SIDE BY SIDE PLOTS 
                br(), br(), br(),
                fluidRow(column( # State overview table
                  width = 12,
                  align = "center",
                  div(id = "reactable-table", reactableOutput("state_table") |> withSpinner())
                )), # end column<fluidRow state overview table 
                br(), 
                # STATE NOTES 
                div(id = "state-note-section",
                  fluidRow(column( # state notes title 
                    width = 12,
                    align = "center",
                    div(id = "selected-state-note-title", "State Notes")
                  )), # end column<fluidRow state notes title 
                  br(), br(),
                  fluidRow(
                    column(width = 1),
                    column( # probation title, notes (check boxes) and asterisks notes  
                      width = 5, 
                      div(id = "selected-state-note-subtitle", "Probation Metrics"),
                      div(
                        id = "selected-state-note", 
                        htmlOutput("state_probation_notes"), 
                        htmlOutput("state_probation_asterisks_notes")
                        )
                    ), # end column: probation title, notes (check boxes) and asterisks notes 
                    column( # parole title, notes (check boxes) and asterisks notes
                      width = 5, 
                      div(id = "selected-state-note-subtitle", "Parole/Post-Incarceration Metrics"),
                      div(
                        id = "selected-state-note", 
                        htmlOutput("state_parole_notes"),
                        htmlOutput("state_parole_asterisks_notes")
                      )
                    ), # end column: parole title, notes (check boxes) and asterisks notes
                    column(width = 1)
                  ), #end fluidRow
                  fluidRow( # addl notes for states 
                    column(width = 1),
                    column( 
                      width = 10,
                      align = "left",
                      div(id = "selected-state-note", htmlOutput("state_additional_notes"))
                    ), 
                    column(width = 1)
                  ), # end fluidRow: addl notes for states  
                  fluidRow( # standard text for states - prompting questions 
                    column(width = 1),
                    column( 
                      width = 10,
                      align = "left",
                      br(), 
                      div(id = "selected-state-note-subtitle", standard_state_note_header),
                      div(id = "selected-state-note", HTML(standard_state_note_text))
                    ), 
                    column(width = 1)
                  ) # end fluidRow: standard text for states - prompting questions  
                ), #end div: STATE NOTES  
                br(), br()
              ), # end tabPanel: overview 
              
              # 2 Parole -------------------------------------------------------
              tabPanel(
                value="2",
                "Parole",
                br(),
                # Parole graph and download button depending on data availability
                uiOutput("parole_nt"),
                br(), br(), br(),
                fluidRow(column( # Parole reactable table
                  width = 12,
                  align = "center",
                  div(id = "reactable-table", reactableOutput("parole_table") |> withSpinner() )
                )), # end column<fluidRow: parole reactable table 
                br(), br()
              ), # end tabPanel: parole 
              
              # 3 Probation ----------------------------------------------------
              tabPanel(
                value="3",
                "Probation",
                br(),
                # Probation graph and download button depending on data availability
                uiOutput("probation_nt"),
                br(), br(), br(),
                fluidRow(column( # Probation reactable table
                  width = 12,
                  align = "center",
                  div(id = "reactable-table", reactableOutput("probation_table") |> withSpinner() )
                )), #end column<fluidRow: Probation reactable table
                br(), br()
              ), # end tabPanel
            ) # end tabsetPanel: STATE TABS 
          ), # end column
        column(width = 1)
      ), # end fluidRow
      
      # END PANELS (STATE TABS) --------------------------------------------
      br(), br()
      ) # end div
    ), # end tabPanel: STATE TABS 
    
    # DOWNLOAD DATA ############################################################
    tabPanel("downloaddata",
      # download data header ---------------------------------------------------
      div(id = "app-header",
        fluidRow(
          column(width = 3),
          column(width = 6,
            fluidRow(
              column( # SELECT STATE(S)
                width = 3, 
                align = "center",
                class = "input-col",
                labeled_input('input-btn', "",
                  pickerInput(inputId = 'download_state',
                    width = "100%",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    div(style = "font-weight: bold", "Select State(s)"),
                    options = list(`actions-box` = TRUE, style = "picker-style")
                  )
                )
              ), # end column: SELECT STATES
              column( # SELECT METRIC(S)
                width = 3, 
                align = "center", 
                class = "input-col",
                labeled_input('input-btn', "",
                  pickerInput(inputId = 'download_metric',
                    width = "100%",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    div(style = "font-weight: bold", "Select Metric(s)"),
                    options = list(`actions-box` = TRUE, style = "picker-style")
                  )
                )
              ), # end column: SELECT METRIC(S)
              column( # SELECT YEAR(S)
                width = 3, 
                align = "center", 
                class = "input-col",
                labeled_input('input-btn', "",
                  pickerInput(inputId = 'download_year',
                    width = "100%",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    div(style = "font-weight: bold", "Select Year(s)"), 
                    options = list(`actions-box` = TRUE, style = "picker-style")
                  )
                )
              ), # end column: SELECT YEAR(S)
              column( # DOWNLOAD DATA BUTTON 
                width = 3, 
                align = "center", 
                class = "input-col",
                downloadButton(outputId = 'save_data', "Download Data", class = "download-btn-lg")
              ) # end column: DOWNLOAD DATA BUTTON 
            ) #end fluidRow 
          ), # end column 
          column(width = 3)
        ) # fluidRow
      ), # end div header
      br(),
      
      # download data app body ---------------------------------------------------
      div(id = "app-body",
        fluidRow(
          column(width = 2),
          column(width = 8, div(id = "download-title", "Download Data")),
          column(width = 2)
        ),
        br(),
        fluidRow(
          column(width = 2),
          column(
            width = 8, 
            div(
              id = "download-info",
              "To understand the impact of community supervision
              (i.e., probation, parole, post-release supervision) on state prison populations,
              The Council of State Governments (CSG) Justice Center surveyed corrections
              leaders in all 50 states. This project was supported by Arnold Ventures and
              produced in partnership with the Correctional Leaders Association (CLA).
              resulting data spans 6 years—from 2018 to 2023—and demonstrate how the number
              of people sent to prison for supervision violations changed."
            )
          ),
          column(width = 2)
        ),
        br(),
        fluidRow( # download table 
          column(width = 2),
          column(width = 8, 
                 align = "center",
                 div(id = "selected-download-table", reactableOutput("selected_download_table"))
          ),
          column(width = 2)
        ), # end fluidRow: download table 
        br(), br()
      ) # end div: app body 
    ) # end tabPanel: download data 

# GLOBAL END (FOOTER) ######################################################
)) # end navbarPage<fluidPage
