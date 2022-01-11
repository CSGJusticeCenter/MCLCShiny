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
                        
                        tabItem(tabName = "National",
                                
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
                                                    br(),
                                                    actionButton(inputId = "GoButton_1", label = "Go",  icon("refresh"))
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
                                    tags$head(tags$style("#selected_state{color: #000000;
                                                                          font-size: 24px;
                                                                          font-style: bold;
                                                    }")),
                                    br(),
                                    
                                    
                                    tabsetPanel(
                                      ###################
                                      # Overview of state report
                                      ###################
                                      tabPanel(value="1","Overview", 
                                               br(),
                                               
                                               
                                      ),
                                      ###################
                                      # How your state compares
                                      ###################
                                      tabPanel("Compare", 
                                               br(),
                                               "How Your State Compares"
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
                                  titlePanel(h2("title 3")),
                                  
                                  mainPanel(                          
                                    
                                    "Text"
                                  
                                  ) #mainPanel
                                ) #fluidPage
                        ) #tabItem 
                      ) #tabItems
                    ) #dashboardBody
) #dashboardPage

server <- function(input, output, session) {
  
  
  #-------------------------------------------------------------------------------
  # State Reports
  #-------------------------------------------------------------------------------
  
  # Print state name and adm or pop selected
  output$selected_state <- renderText({ 
    paste("Prison ", input$adm_or_pop, "Trends in ", input$state)
  })  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
