source("data_libraries.R")
source("functions.R")

ui <- dashboardPage(dashboardHeader(title = "MCLC"),
                    sidebar = dashboardSidebar(
                      sidebarMenu(id = "tabs",
                                  menuItem(text = "Map Explorer",  tabName = "Map_Explorer"),
                                  menuItem(text = "State Reports", tabName = "State_Reports"),
                                  menuItem(text = "Download Data", tabName = "Download_Data")
                      )
                    ),
                    body = dashboardBody(

                      # change to custom theme found in data_libraries.R
                      customTheme,

                      tabItems(

#--------------------------------------------------------------------------------------------------------------
# Map Page
#--------------------------------------------------------------------------------------------------------------

                        tabItem(tabName = "Map_Explorer",

                                fluidPage(########
                                          # Side panel
                                          ########
                                          br(),
                                          wellPanel(tags$style(type="text/css", '#leftPanel {width:200px; float:left;}'), id = "leftPanel",
                                                    selectInput("data_map_counts",       "Data",  choices = c("Total", "Supervision Violations",
                                                                                                              "Technical", "New Offense",
                                                                                                              "Parole", "Probation")),
                                                    selectInput("adm_or_pop_map_counts", "Type",  choices = unique(mclc_explorer$adm_or_pop)),
                                                    selectInput("year_map_counts",       "Years", choices = unique(mclc_explorer$year)),
                                                    downloadButton(outputId = "save_map",  label = "Download Map"),
                                                    downloadButton(outputId = "save_data", label = "Download Data")
                                          ),
                                          ########
                                          # Map
                                          ########
                                          mainPanel(
                                            fluidRow(column(width = 12,
                                                            align = "left",
                                                            br(),
                                                            textOutput("selected_map"),
                                                            tags$head(tags$style("#selected_map{font-size: 20px;
                                                                                                font-style: bold;
                                                                                                font-family: sans-serif;}")))),
                                            fluidRow(column(width = 12,
                                                            align = "center",
                                                            plotOutput("reactive_map", height = 600))),
                                            fluidRow(column(width = 12,
                                                            align = "center",
                                                            reactableOutput("table_map_counts"))),
                                            br()
                                          ) #mainPanel
                                ) #fluidPage
                        ), #tabItem

#--------------------------------------------------------------------------------------------------------------
# State Reports
#--------------------------------------------------------------------------------------------------------------

                        tabItem(tabName = "State_Reports",
                                fluidPage(
                                  br(),
                                  wellPanel(tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'), id = "leftPanel",
                                            selectInput("state", "State",      choices = unique(adm_pop_long$state)),
                                            radioButtons("adm_or_pop", "Type", choices = unique(adm_pop_long$adm_or_pop))),

                                  mainPanel(######
                                            # State title
                                            ######
                                            fluidRow(column(width = 12,
                                                            align = "left",
                                                            br(),
                                                            textOutput("selected_state"),
                                                            tags$head(tags$style("#selected_state{font-size: 20px;
                                                                                                  font-style: bold;
                                                                                                  font-family: sans-serif;}")),
                                                            br())),

                                            ############
                                            # Value boxes
                                            ############
                                            tags$style(".small-box.bg-black       {background-color: #3C3C3C !important; color: #FFFFFF !important; }"),
                                            tags$style(".small-box.bg-green       {background-color: #fcccac !important; color: #3C3C3C !important; }"),
                                            tags$style(".small-box.bg-orange      {background-color: #fc9c54 !important; color: #FFFFFF !important; }"),
                                            tags$style(".small-box.bg-blue        {background-color: #2B4570 !important; color: #FFFFFF !important; }"),

                                            tags$style(".small-box                {border: 1px; border-style: solid; border-color: #FFFFFF !important; border-radius: 1px; padding: 0.1em; }"),

                                            fluidRow(
                                              column(width = 3,
                                                     valueBoxOutput("total_change", width = 125)),
                                              column(width = 3,
                                                     valueBoxOutput("sup_change",   width = 125)),
                                              column(width = 3,
                                                     valueBoxOutput("tech_change",  width = 125)),
                                              column(width = 3,
                                                     valueBoxOutput("rev_rate",     width = 125))
                                            ), #fluidRow
                                            br(),

                                            tabsetPanel(
                                              ###################
                                              # Overview of state
                                              ###################
                                              tabPanel(value="1","Overview",
                                                       br(),
                                                       ############
                                                       # Plots
                                                       ############
                                                       fluidRow(
                                                         column(width = 7,
                                                                plotlyOutput("totals_chart",         height = 375)),
                                                         column(width = 5,
                                                                plotlyOutput("sup_viols_type_chart", height = 375))
                                                       ), #fluidRow
                                                       br(),
                                                       ############
                                                       # Table under graphs
                                                       ############
                                                       fluidRow(
                                                         column(width = 12,
                                                                align = "center",
                                                                reactableOutput("state_table"))),
                                                       br()
                                              ),
                                              ###################
                                              # Parole
                                              ###################
                                              tabPanel("Parole",
                                                       br(),
                                                       ############
                                                       # Plots
                                                       ############
                                                       fluidRow(column(width = 6,
                                                                       plotlyOutput("areachart_parole", height = 300)),
                                                                column(width = 6,
                                                                       plotlyOutput("barchart_parole",  height = 300))),
                                                       br(),
                                                       ############
                                                       # Table under graphs
                                                       ############
                                                       fluidRow(column(width = 12,
                                                                       align = 'center',
                                                                       reactableOutput("parole_table"))),
                                                       br()
                                              ),
                                              ###################
                                              # Probation
                                              ###################
                                              tabPanel("Probation",
                                                       br(),
                                                       ############
                                                       # Plots
                                                       ############
                                                       fluidRow(column(width = 6,
                                                                       plotlyOutput("areachart_prob", height = 300)),
                                                                column(width = 6,
                                                                       plotlyOutput("barchart_prob",  height = 300))),
                                                       br(),
                                                       ############
                                                       # Table under graphs
                                                       ############
                                                       fluidRow(column(width = 12,
                                                                       align = 'center',
                                                                       reactableOutput("prob_table")))
                                              ), #tabPanel
                                              id = "tb2") #tabsetPanel
                                  ) #mainPanel
                                ) #fluidPage
                        ), #tabItem

#--------------------------------------------------------------------------------------------------------------
# Download Data
#--------------------------------------------------------------------------------------------------------------

                        tabItem(tabName = "Download_Data",

                                fluidPage(br(),
                                          wellPanel(tags$style(type="text/css", '#leftPanel { width:250px; float:left;}'), id = "leftPanel",
                                                    selectInput(inputId = "dataset",
                                                                label = "Dataset",
                                                                choices = c("More Community, Less Confinement (CSG)", "Annual Probation Survey and Annual Parole Survey (BJS)")),
                                                    pickerInput(inputId = 'year_table',  label = 'Year(s)',  choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
                                                    pickerInput(inputId = 'state_table', label = 'State(s)', choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))
                                          ), # wellPanel
                                          mainPanel(
                                            fluidRow(column(width = 12,
                                                            h3("Download Data"),
                                                            br(),
                                                            textOutput("selected_data"),
                                                            tags$head(tags$style("#selected_data{font-size: 20px;
                                                                                                 font-style: bold;
                                                                                                 font-family: sans-serif;}")),
                                                            br(),
                                                            textOutput("selected_data_info"),
                                                            br(),
                                                            br())
                                            ),
                                            fluidRow(
                                              column(width = 12,
                                                     DT::dataTableOutput("main_table"))
                                              )
                                          ) #mainPanel
                                ) #fluidPage
                        ) #tabItem
                      ) #tabItems
                    ), #dashboardBody
                    tags$head(tags$style(HTML('* {font-family: "Franklin Gothic Book"};')))
                    # tags$head(
                    #   includeCSS("www/custom.css")
                    # )
) #dashboardPage
