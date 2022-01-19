ui <- dashboardPage(dashboardHeader(title = "MCLC"), 
                    sidebar = dashboardSidebar(
                      sidebarMenu(id = "tabs",
                                  menuItem(text = "Map Explorer",tabName = "Map_Explorer",icon = icon("map-pin")),
                                  menuItem(text = "State Reports",tabName = "State_Reports",icon = icon("search-location")),
                                  menuItem(text = "View Data",tabName = "View_Data",icon = icon("table"))
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
                                            fluidRow(plotOutput("map_counts")),
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
                                    tabsetPanel(
                                      ###################
                                      # Overview of state report
                                      tabPanel(value="1","Overview", 
                                               br(),
                                               fluidRow(
                                                        # box(plotOutput("areachart", height = 250)),
                                                        # box(plotOutput("barchart",  height = 250))
                                                 column(width = 7, 
                                                        plotOutput("areachart", height = 310)),
                                                 column(width = 5,
                                                        plotOutput("barchart",  height = 300))
                                               ) #fluidRow
                                      ),
                                      ###################
                                      # Parole
                                      ###################
                                      tabPanel("Parole", 
                                               br(),
                                               fluidRow(
                                                 column(width = 1),
                                                 column(width = 7,
                                                        plotOutput("barchart_parole", height = 300, width = 450))
                                               ) #fluidRow
                                      ),
                                      ###################
                                      # Probation
                                      ###################
                                      tabPanel("Probation", 
                                               br(),
                                               fluidRow(
                                                 column(width = 1),
                                                 column(width = 7,
                                                        plotOutput("barchart_prob", height = 300, width = 450))
                                               ) #fluidRow
                                      ),
                                      id = "tb2")
                                  ) #mainPanel
                                ) #fluidPage
                        ), #tabItem 
                        
                        #-------------------------------------------------------
                        # View Data
                        #-------------------------------------------------------
                        
                        tabItem(tabName = "View_Data",
                                fluidPage(

                                  # headerPanel("header 3"),
                                  # titlePanel(h2("title 3")),

                                  mainPanel(

                                    br(),
                                    "TEXT"

                                  ) #mainPanel
                                ) #fluidPage
                        ) #tabItem
                      ) #tabItems
                    ) #dashboardBody
) #dashboardPage