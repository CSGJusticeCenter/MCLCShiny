
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
    #   ".shiny-output-error:before { visibility: visible; content: ''; }"),x
             
    # NATL TRENDS HEX MAP  #####################################################
    tabPanel("nationaltrends",
      # natl trends header -----------------------------------------------------
      div(
        class = "header", 
        # fluidRow("x") == <div class="row">x</div> == div(class == "row")
        div(class = "row fixwidth", # this is REQUIRED to remove extra spacing
          column(# Select Metric
            width = 3,
            align = "center",
            class = "input-col",
            labeled_input('input-btn', "",
              selectInput('data_map',
                label = "Select Metric",
                choices = metric_opts,
                multiple = FALSE
              ) # selectInput
            ) # labeled_input 
          ), # column: select metric 
          column(# Select Type (Adm or Pop)
            width = 3,
            align = "center",
            class = "input-col",
            labeled_input('input-btn', "",
              selectInput('adm_or_pop_map',
                label = "Select Metric Type",
                choices = type_opts,
                multiple = FALSE
              ) # selectInput 
            ) # labeled_input
          ), #column: select type 
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
              ) # selectInput
            ) # labeled_input
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
        ) # end div (row fixwidth) with columns 
      ), # end div header
      br(), # end header with dropdowns and download button 
      
      # natl trends app body ---------------------------------------------------
      div(class = "row app-body", 
        div( # HEx MAP 
          class = "row",
          align = "center",
          div(class = "hidden-xs hidden-sm", #hex map will be hidden on xs and sm widow (mobile) 
            #id = "hex-map",
            highchartOutput("hex_map", height = 600, width = "100%") |> svii_spinner()
          ) # end div 
        ), # end column HEX MAP 
        div( # HEx MAP NOTE
          class = "row fixwidth",
          align = "left",
          div(style = "font-family: Graphik;", HTML(hex_map_note)) # obj created in dataframes.R 
        ), # end column HEX MAP NOTE
        div( # TABLE TITLE
          class = "row",
          align = "center",
          div(
            id = "selected-map-table",
            br(),  
            textOutput("selected_map_table")
          ) # end div 
        ), # end column TABLE TITLE 
        br(),
        # tags$head(tags$style(HTML("thead{color: #004270; font-size: 16px}"))),
        div(# HEX MAP TABLE 
          class = "row fixwidth", 
          align = "center",
          div(
            id = "reactable-table",
            reactableOutput("table_map")
          )
        ),
        br(), br()
      ) # end div app-body 
    ), # end tabPanel END NATL TRENDS HEX MAP 
    
    
    # STATE DASHBOARDS #########################################################
    
    tabPanel("statedashboard",
      # state dashboard header -------------------------------------------------
      div(class = "header",
        div(class = "row fixwidth", # this is REQUIRED to remove extra spacing
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
                ) # select input 
              ) # div 
            ) # labeled_input
          ), # column: SELECT STATE 
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
                ) # select input 
              ) # div 
            ) # labeled_input
          ), # column: SELECT TYPE (ADM OR POP) 
          column(width = 2)
        ) # div row fixwidth 
      ), # div header
      br(),
      # state header app body -------------------------------------------------
      div(class = "row app-body", 
      div(class = "row fixwidth", 
        # 0 state title --------------------------------------------------------
        div(id = "selected-state", textOutput("selected_state"), "aria-label" = "Selected State"), 
        br(),
        
        # 0 value boxes --------------------------------------------------------
        fluidRow( 
          column(width = 3, valueBoxOutput("total_change",  width = "100%") |> svii_spinner()),
          column(width = 3, valueBoxOutput("sup_change",    width = "100%") |> svii_spinner()),
          column(width = 3, valueBoxOutput("tech_change",   width = "100%") |> svii_spinner()),
          column(width = 3, valueBoxOutput("new_off_change",width = "100%") |> svii_spinner())
        ), 
        br(), br(),
        
        # START PANELS (STATE TABS) --------------------------------------------
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
              column( #1a area chart ----------------------------------------  
                width = 5,
                align = "center",
                uiOutput("state_area") |> svii_spinner()
              ),
              column( # area chart download button 
                width = 1, 
                align = "center",
                uiOutput("state_area_button")
              ),
              column( #1b supervision bar chart --------------------------------
                width = 5, 
                align = "center",
                uiOutput("state_nt") |> svii_spinner()
              ),
              column( # supervision bar chart download button 
                width = 1, 
                align = "center",
                uiOutput("state_nt_button")
              )
            ), # fluidrow END SIDE BY SIDE PLOTS 
            br(), br(), br(),
            fluidRow(column( # State overview table
              width = 12,
              align = "center",
              div(id = "reactable-table", reactableOutput("state_table") |> svii_spinner())
            )), # column<fluidRow state overview table 
            br(), 
            # 1c key questions to consider notes ------------------------------------------------------
            div(id = "state-note-section",
                fluidRow(column( # state notes title 
                  width = 12,
                  align = "center",
                  div(id = "selected-state-note-title", standard_state_note_header) # obj created in dataframes.R 
                )), # column<fluidRow state notes title 
                br(), br(),
                fluidRow( # standard text for states - prompting questions 
                  column(width = 1),
                  column( 
                    width = 10,
                    align = "left",
                    div(id = "selected-state-note", HTML(standard_state_note_text)), # obj created in dataframes.R 
                    br(), br(), 
                  ), 
                  column(width = 1)
                ) # end fluidRow: standard text for states - prompting questions  
            ), #end div: KEY QUESTIONS TO CONSIDER 
            # 1d state notes ------------------------------------------------------
            div(id = "state-note-section",
              fluidRow(column( # state notes title 
                width = 12,
                align = "center",
                div(id = "selected-state-note-title", "State Notes")
              )), # column<fluidRow state notes title 
              br(), br(),
              fluidRow(
                column(width = 1),
                column( # parole title, notes (check boxes) and asterisks notes
                  width = 5, 
                  div(id = "selected-state-note-subtitle", "Parole/Post-Incarceration Metrics"),
                  div(
                    id = "selected-state-note", 
                    htmlOutput("state_parole_notes"),
                    htmlOutput("state_parole_asterisks_notes")
                  )
                ), # end column: parole title, notes (check boxes) and asterisks notes
                column( # probation title, notes (check boxes) and asterisks notes  
                  width = 5, 
                  div(id = "selected-state-note-subtitle", "Probation Metrics"),
                  div(
                    id = "selected-state-note", 
                    htmlOutput("state_probation_notes"), 
                    htmlOutput("state_probation_asterisks_notes")
                    )
                ), # column: probation title, notes (check boxes) and asterisks notes 
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
              div(id = "reactable-table", reactableOutput("parole_table") |> svii_spinner() )
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
              div(id = "reactable-table", reactableOutput("probation_table") |> svii_spinner() )
            )), # column<fluidRow: Probation reactable table
            br(), br()
          ), # tabPanel
          
          # 4 Demographics ----------------------------------------------------
          tabPanel(
            value="4",
            "Demographics",
            br(),
            fluidRow(column( 
              width = 12,
              align = "center",
              radioButtons(
                inputId = "demo_groupcat", 
                # label = div("Select a demographic group category:", style = "margin-bottom: 5px;"), 
                label = NULL, 
                choiceNames = list(
                  tags$span(class = "demo-radio-cat", "Race and Ethnicity"), 
                  tags$span(class = "demo-radio-cat", "Sex or Gender")
                ), 
                choiceValues = list("race_ethnicity", "sex_gender"), 
                # choices = list(
                #   "Race and Ethnicity" = "race_ethnicity", 
                #   "Sex or Gender" = "sex_gender"
                # ), 
                inline = TRUE
              ), 
              htmlOutput("demo_pretext0"), 
              htmlOutput("demo_pretext1"), 
              div(id = "reactable-table", reactableOutput("demo_table_1")), 
              htmlOutput("demo_pretext2"), 
              div(id = "reactable-table", reactableOutput("demo_table_2")), 
              htmlOutput("demo_pretext3"), 
              div(id = "reactable-table", reactableOutput("demo_table_3")),
              br(), 
              div(id = "selected-state-note-title", "Notes"), 
              htmlOutput("demo_posttext"), 
            )), # column<fluidRow: Probation reactable table
            br(), br()
          ), # tabPanel
        ) # tabsetPanel: STATE TABS 
      ), # div row fix width 
      ) # div row app-body 
    ), # tabPanel: STATE TABS 
    

# GLOBAL END (FOOTER) ######################################################
)) # end navbarPage<fluidPage
