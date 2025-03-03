#######################################
# Project: MCLCShiny
# File: ui.R
# Authors: Mari Roberts
# Date last updated: August 21, 2023 (MAR)
# Description:
#    User interface for shiny app
#######################################

source("colors.R")
source("dataframes.R")
source("functions.R")
source("modals.R")
source("guides.R")

ui <- fluidPage(

  # include custom CSS
  includeCSS("www/theme.css"),
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

             # App title - Accessible
             # tags$head(tags$title("More Community, Less Confinement Dashboard")),
             title = "More Community, Less Confinement Dashboard",

             # English - Accessible
             tags$html(lang="en"),

     ############################################################################################################################## MAP

     # Map Explorer Page
     tabPanel("nationaltrends",

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
                                                                            label = "Select Metric",
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
                                                                            label = "Select Metric Type",
                                                                            choices = c("Admissions",
                                                                                        "Population"),
                                                                            multiple = FALSE))),
                                           # Select Year Change
                                           column(width = 3,
                                                  align = "center",
                                                  class = "input-col",
                                                  labeled_input('input-btn', "",
                                                                selectInput('year_map',
                                                                            label = "Select Year Change",
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
                                                                               label = "Download Map",
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

                  fluidRow(
                    column(width = 12,
                           align = "center",
                           div(class = "hidden-xs hidden-sm",
                               #id = "hex-map",
                               highchartOutput("hex_map",
                                               height = 600,
                                               width = "100%") %>% withSpinner()
                               )),
                  ),
                  br(), br(),

                  #######
                  # Hex map table
                  #######

                  fluidRow(
                    column(width = 12,
                           align = "center",
                           div(id = "selected-map-table",
                               textOutput("selected_map_table"))),
                  ),
                  br(),
                  tags$head(tags$style(HTML("thead{color: #004270; font-size: 16px}"))),
                  fluidRow(
                    column(width = 12, align = "left",
                           div(id = "reactable-table",
                               reactableOutput("table_map"))),
                  ),
                  br(), br()

              ) # end div


     ), # end tabPanel

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
                                                                   choices = unique(adm_pop_long$state),
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
                                                          width = "100%") %>% withSpinner()
                                           ),
                                    column(width = 3,
                                           valueBoxOutput("tech_change",
                                                          width = "100%") %>% withSpinner()
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
                                                             uiOutput("state_area") %>% withSpinner()),
                                                      column(width = 1, align = "center",
                                                             uiOutput("state_area_button")),
                                             # Supervision violation graph and download button
                                                      column(width = 5, align = "center",
                                                             uiOutput("state_nt") %>% withSpinner()),
                                                      column(width = 1, align = "center",
                                                             uiOutput("state_nt_button"))),
                                             br(), br(), br(),

                                             # State overview table
                                             fluidRow(column(width = 12,
                                                             align = "center",
                                                             div(id = "reactable-table",
                                                                 reactableOutput("state_table") %>% withSpinner()
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
                                                                 reactableOutput("parole_table") %>% withSpinner()
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
                                                                 reactableOutput("probation_table") %>% withSpinner()
                                                             ))),
                                             br(), br()

                                    ), # end tabPanel

                                    tabPanel(value="4","Race and Ethnicity",
                                             br(),
                                             fluidRow(column(width = 12,
                                                             align = "center",
                                               div(id = "denominator-picker",
                                                   class = "retitle",
                                                   style = "margin-bottom: 12px;",
                                                   pickerInput('pop_denom',
                                                               label = NULL,
                                                               width = "fit",
                                                               choices = c("Total Disparities" = "CEN",
                                                                           "Portion of Disparities Attributable to Parole Revocations" = "BJS"),
                                                               options = list(style = "re-picker",
                                                                              class = "retitle"),
                                                               selected = "CEN",
                                                               inline = TRUE),

                                                   # Add tooltip depending on pop_denom selection/tabindex = 0 for screenreader
                                                   tagAppendAttributes(uiOutput("redefinition_tooltip",
                                                                                inline = TRUE), tabindex = 0)

                                                )
                                             )),
                                             fluidRow(column(width = 2),
                                                      column(width = 8,
                                                             align = "center",
                                                             div(id = "re-explanation", textOutput("retitleend")),
                                                             br(), br()),
                                                      column(width = 2)),
                                             fluidRow(
                                               column(width = 2),
                                               column(width = 8, align = "center", id = "infopanel-id",
                                                 htmlOutput("infogheader"),
                                                 conditionalPanel(condition = "output.showinfogpanel",
                                                    div(imageOutput("infogblack", height = "100%", ), style = "margin-bottom: 0.5em;"),
                                                    imageOutput("infoghisp", height = "100%"), br(),
                                                    div(id = "re-note", textOutput("renote")), br(),
                                                    htmlOutput("howitscalculated"),
                                                 ), # end conditional panel
                                                 div(id = "showtables-id",
                                                     checkboxInput("showtables", "Show Additional Data Tables", value = FALSE),
                                                     align = "left"),
                                                 conditionalPanel(condition = "output.showtablepanel",
                                                    htmlOutput("table_rri_header")   ,
                                                    htmlOutput("table_rri")          ,
                                                    htmlOutput("table_rate_header")  ,
                                                    htmlOutput("table_rate")         ,
                                                    htmlOutput("table_revcnt_header"),
                                                    htmlOutput("table_revcnt")       ,
                                                    br(),
                                                    div(html("&#10033; Asterisk indicates that counts of readmissions to prison from parole are less than 5. In these instances, the actual count values are suppressed, counts are shown with a value of 5, and rates are calculated using a count value of 5.")
                                                        , class = "retxt", align = "left", style = "font-size: 0.95em !important;")
                                                 ), #end conditional Panel
                                               ), #end column width=8
                                               column(width = 2),
                                               tags$button(
                                                 class = "floating-button",
                                                 `aria-label` = "info button",
                                                 alt = "This button calls the information modal and app guide",
                                                 id = "guide-button",
                                                 onclick = "Shiny.setInputValue(\"show_guide\", true, {priority: \"event\"})",
                                                 icon("info", class = "centered-icon", id = "centered-icon")
                                                 ),
                                                 tags$script(
                                                       HTML(
                                                              "
                                                              var observedElement = document.body;
                                                              var prevClassState = observedElement.classList.contains('modal-open')
                                                              var observer = new MutationObserver(function(mutations) {
                                                                     mutations.forEach(function(mutation) {
                                                                            if (mutation.attributeName == 'class') {
                                                                                   var currentClassState = mutation.target.classList.contains('modal-open');
                                                                                   if(prevClassState !== currentClassState) {
                                                                                          prevClassState = currentClassState;
                                                                                          if(currentClassState) {
                                                                                                 document.body.setAttribute('aria-hidden', false)
                                                                                                 document.getElementsByClassName('modal-content')[0].setAttribute('aria-hidden', false)
                                                                                                 document.getElementsByClassName('modal-body')[0].setAttribute('aria-hidden', false)
                                                                                                 document.getElementsByClassName('tab-content')[0].setAttribute('aria-hidden', true)
                                                                                          } else {
                                                                                                 document.body.setAttribute('aria-hidden', false)
                                                                                                 document.getElementsByClassName('tab-content')[0].setAttribute('aria-hidden', false)
                                                                                          }
                                                                                   }
                                                                            }
                                                                     })
                                                              })
                                                              observer.observe(observedElement, {attributes: true})
                                                              "
                                                       )
                                                 )
                                             ), #end fluidRow
                                             br(),
                                             fluidRow(column(width = 2),
                                                      column(width = 8,
                                                             imageOutput("RE_infographic.img")),
                                                      column(width = 2)),
                                             br()
                                    ) # end tabPanel
                                    #### END  RACE/ETHNICITY TAB

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
))
