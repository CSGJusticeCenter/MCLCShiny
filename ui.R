source("data_libraries.R")
source("functions.R")

ui <- dashboardPage(dashboardHeader(title = "MCLC"), 
                    sidebar = dashboardSidebar(
                      sidebarMenu(id = "tabs",
                                  menuItem(text = "Map Explorer",tabName = "Map_Explorer",icon = icon("map-pin")),
                                  menuItem(text = "Map Explorer 2",tabName = "Map_Explorer_2",icon = icon("map-pin")),
                                  menuItem(text = "State Reports",tabName = "State_Reports",icon = icon("search-location")),
                                  menuItem(text = "Download Data",tabName = "Download_Data",icon = icon("table"))
                      )
                    ), #dashboardSidebar
                    body = dashboardBody(
                      
                      # change to custom theme
                      customTheme,
                      
                      tabItems(
                        #-------------------------------------------------------
                        # National Page
                        #-------------------------------------------------------
                        
                        tabItem(tabName = "Map_Explorer",
                                
                                fluidPage(wellPanel(tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'), id = "leftPanel",
                                                    selectInput("data_map_counts", "Data",        choices = unique(adm_pop_long$metric)),
                                                    selectInput("adm_or_pop_map_counts", "Type",  choices = unique(adm_pop_long$adm_or_pop)),
                                                    
                                                    radioButtons("choice_map_counts", "Value",    choices = c("Count", "Change from Previous Year"), selected = "Count"),
                                                    conditionalPanel(
                                                      condition = "input.choice_map_counts == 'Count'",
                                                      selectInput("year_map_counts", "Year", choices = c(2018, 2019, 2020))
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.choice_map_counts == 'Change from Previous Year'",
                                                      selectInput("year_map_counts2", "Year", choices = c(2019, 2020))
                                                    )
                                          ),
                                          mainPanel(  
                                            fluidRow(plotOutput("map_counts", width = "100%")),
                                            br(), br(),
                                            fluidRow(
                                              column(width = 2),
                                              column(width = 8, DT::dataTableOutput("table_map_counts"))),
                                              column(width = 2)
                                            # fluidRow(reactableOutput("table_map_counts"))
                                          )
                                )
                        ), #tabItem
                        
                        #-------------------------------------------------------
                        # State Reports
                        #-------------------------------------------------------
                        
                        tabItem(tabName = "State_Reports",
                                fluidPage(
                                  # headerPanel("header 2"),
                                  # titlePanel(h2("title 2")),
                                  br(),
                                  wellPanel(tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'), id = "leftPanel",
                                            selectInput("state", "State", choices = unique(adm_pop_long$states)),
                                            radioButtons("adm_or_pop", "Type",   choices = unique(adm_pop_long$adm_or_pop))
                                            # radioButtons("year", "Year",       choices = unique(adm_pop_long$year))
                                  ),
                                  
                                  mainPanel(
                                    
                                    ######
                                    # State title
                                    ######
                                    textOutput("selected_state"),
                                    tags$head(tags$style("#selected_state{font-size: 24px;
                                                                          font-style: bold;}")),
                                    # br(),
                                    # textOutput("selected_state_adm_pop"),
                                    # tags$head(tags$style("#selected_state_adm_pop{font-size: 14px
                                    #                                               font-style: regular;}")),
                                    br(),
                                    
                                    ############
                                    # Value boxes
                                    ############
                                    tags$style(".small-box.bg-green {background-color: #3C3C3C !important; color: #FFFFFF !important; }"),
                                    tags$style(".small-box.bg-red   {background-color: #3C3C3C !important; color: #FFFFFF !important; }"),
                                    tags$style(".small-box          {border: 1px; border-style: solid; border-color: #3C3C3C !important; border-radius: 1px; padding: 0.1em; }"),
                                    
                                    fluidRow(
                                      column(width = 4,
                                             valueBoxOutput("total_change", width = 125)),
                                      column(width = 4,
                                             valueBoxOutput("sup_change", width = 125)),
                                      column(width = 4,
                                             valueBoxOutput("tech_change", width = 125)
                                      )
                                    ), #fluidRow
                                    
                                    br(),
                                    
                                    tabsetPanel(
                                      ###################
                                      # Overview of state report
                                      ###################
                                      tabPanel(value="1","Overview", 
                                               br(),
                                               ############
                                               # Plots
                                               ############
                                               fluidRow(
                                                 column(width = 7, 
                                                        plotlyOutput("totals_chart", height = 375)),
                                                 column(width = 5,
                                                        plotlyOutput("sup_viols_type_chart", height = 375))
                                               ), #fluidRow
                                               br(), 
                                               ############
                                               # Tables
                                               ############
                                               fluidRow(
                                                 column(width = 12,
                                                        reactableOutput("state_table")
                                                        )                                               ),
                                               br()
                                      ),
                                      ###################
                                      # Parole
                                      ###################
                                      tabPanel("Parole", 
                                               br(),
                                               fluidRow(
                                                 column(width = 6,
                                                        plotlyOutput("areachart_parole", height = 300)),
                                                 column(width = 6,
                                                        plotlyOutput("barchart_parole", height = 300)),
                                               ),
                                               br(),
                                               fluidRow(
                                                 column(width = 6,
                                                        #plotlyOutput("barchart_bjs_parole", height = 300)
                                                        ),
                                                 column(width = 6)
                                               ) #fluidRow
                                      ),
                                      ###################
                                      # Probation
                                      ###################
                                      tabPanel("Probation", 
                                               br(),
                                               fluidRow(
                                                 column(width = 6,
                                                        plotlyOutput("areachart_prob", height = 300)),
                                                 column(width = 6,
                                                        plotlyOutput("barchart_prob", height = 300))
                                               ),
                                               br(),
                                               fluidRow(
                                                 column(width = 6,
                                                        #plotlyOutput("barchart_bjs_prob", height = 300)
                                                 ),
                                                 column(width = 6)
                                               ) #fluidRow
                                      ),
                                      id = "tb2")
                                  ) #mainPanel
                                ) #fluidPage
                        ), #tabItem 
                        
                        #-------------------------------------------------------
                        # 2nd Map Option
                        #-------------------------------------------------------
                        
                        tabItem(tabName = "Map_Explorer_2",
                                fluidPage(

                                  # headerPanel("header 3"),
                                  # titlePanel(h2("title 3")),

                                  mainPanel(

                                    br(),
                                    leafletOutput("leaflet_map", height = 350, width = 800),
                                    tags$style(HTML(".leaflet-container { background: #FFFFFF;}")),
                                    tags$style(type = "text/css", "#leaflet_map {height: calc(100vh - 53px) !important;}"),

                                  ) #mainPanel
                                ) #fluidPage
                        ), #tabItem
                        
                        #-------------------------------------------------------
                        # Download Data
                        #-------------------------------------------------------
                        tabItem(tabName = "Download_Data",
                                
                                fluidPage(wellPanel(tags$style(type="text/css", '#leftPanel { width:250px; float:left;}'), id = "leftPanel",
                                                    selectInput(inputId = "dataset",
                                                                label = "Dataset",
                                                                choices = c("More Community, Less Confinement (CSG)", "Bureau of Justice Statistics")),
                                                    
                                                    conditionalPanel(
                                                      condition = "input.dataset == 'More Community, Less Confinement (CSG)'",
                                                      checkboxGroupInput("year_table", "Year", choices = unique(csg$year), selected = "2018"),
                                                      selectizeInput("state_table", "State(s)", choices = unique(csg$state), multiple = TRUE, selected = "Alabama")
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.dataset == 'Bureau of Justice Statistics'",
                                                      checkboxGroupInput("year_table2", "Year", choices = unique(df_prob_parole$year), selected = "2014"),
                                                      selectizeInput("state_table2", "State(s)", choices = unique(df_prob_parole$state), multiple = TRUE, selected = "Alabama")
                                                    )
                                          ), # wellPanel
                                          mainPanel(  
                                            br(),
                                            fluidRow(
                                              column(width = 2),
                                              column(width = 8, DT::dataTableOutput("main_table"))),
                                              column(width = 2)
                                          ) #mainPanel
                                ) #fluidPage
                        ) #tabItem
                      ) #tabItems
                    ), #dashboardBody
                    tags$head(tags$style(HTML('* {font-family: "Arial"};')))
) #dashboardPage