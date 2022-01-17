library(shiny)
library(shinythemes)
library(wordcloud2)
library(shinydashboard)
library(dashboardthemes)

ui <- dashboardPage(dashboardHeader(title = "MCLC"), 
                    sidebar = dashboardSidebar(
                      sidebarMenu(id = "tabs",
                                  menuItem(text = "National",tabName = "National",icon = icon("chart-bar")),
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
                        
                        tabItem(tabName = "About",
                                
                                fluidPage(theme = shinytheme("united"), 
                                          
                                          headerPanel("header for title 1"),
                                          titlePanel(h3("title for category 1")),
                                          
                                          wellPanel(tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'),
                                                    id = "leftPanel",
                                                    conditionalPanel(condition="input.tb1=='1'",
                                                                     textInput("sc_number", h5("Enter a Number:"), 10)
                                                    ),
                                                    conditionalPanel(condition="input.tb1=='2'",
                                                                     textInput("string_1", h5("Enter String:"), "string here")
                                                    ),
                                                    br(),
                                                    selectInput("group_text_1", "Select Groups",
                                                                choices = c("gr1","gr2","gr3"),
                                                                selected = "gr1",
                                                                multiple = TRUE),
                                                    br()
                                          ),
                                          mainPanel(                          
                                            tabsetPanel(
                                              tabPanel(value="1", "Tab #1", hr(), DT::dataTableOutput("sc_table_number")),
                                              tabPanel(value="2", "Tab #2" , hr(), DT::dataTableOutput("sc_table_date")),
                                              id = "tb1")
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
                                            selectInput("state", "Select State", choices = unique(adm_pop_long$states)),
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
                                    DT::dataTableOutput("table_out")

                                  ) #mainPanel
                                ) #fluidPage
                        ) #tabItem
                      ) #tabItems
                    ) #dashboardBody
) #dashboardPage