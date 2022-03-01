source("data_libraries.R")
source("functions.R")

ui <- dashboardPage(dashboardHeader(title = "MCLC"), 
                    sidebar = dashboardSidebar(
                      sidebarMenu(id = "tabs",
                                  menuItem(text = "Map Explorer",  tabName = "Map_Explorer",icon = icon("map-pin")),
                                  menuItem(text = "State Reports", tabName = "State_Reports",icon = icon("search-location")),
                                  menuItem(text = "Download Data", tabName = "Download_Data",icon = icon("table"))
                                  ) #sidebarMenu
                    ), #dashboardSidebar
                    body = dashboardBody(
                      
                      # change to custom theme
                      customTheme,
                      
                      tabItems(
                        
                        #-------------------------------------------------------
                        # Map Page
                        #-------------------------------------------------------
                        
                        tabItem(tabName = "Map_Explorer",
                                
                                fluidPage(########
                                          # Side panel
                                          ########
                                          br(),
                                          wellPanel(tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'), id = "leftPanel",
                                                    selectInput("data_map_counts", "Data",        choices = unique(adm_pop_long$metric)),
                                                    selectInput("adm_or_pop_map_counts", "Type",  choices = unique(adm_pop_long$adm_or_pop)),
                                                    
                                                    radioButtons("choice_map_counts", "Value",    choices = c("Count", "Change from Previous Year"), selected = "Count"),
                                                    conditionalPanel(
                                                      condition = "input.choice_map_counts == 'Count'",
                                                      selectInput("year_map_counts", "Year", choices = c(2018, 2019, 2020))),
                                                    conditionalPanel(
                                                      condition = "input.choice_map_counts == 'Change from Previous Year'",
                                                      selectInput("year_map_counts2", "Year", choices = c(2019, 2020)))
                                          ), #wellPanel
                                          ########
                                          # Map
                                          ########
                                          mainPanel(
                                            fluidRow(column(width = 12,
                                                            align = "center",
                                                            br(),
                                                            textOutput("selected_map"),
                                                            tags$head(tags$style("#selected_map{font-size: 20px;
                                                                                         font-style: bold;}")),
                                                            br()
                                                            )),
                                            fluidRow(leafletOutput("leaflet_map"),
                                                     tags$style(HTML(".leaflet-container { background: #FFFFFF;}"))
                                                     #tags$style(type = "text/css", "#leaflet_map {height: calc(100vh - 53px) !important;}")
                                                     ),
                                            br(),
                                            fluidRow(column(width = 12,
                                                     align = "center",
                                                     reactableOutput("table_map_counts")))
                                          ) #mainPanel
                              ) #fluidPage
                        ), #tabItem
                        
                        #-------------------------------------------------------
                        # State Reports
                        #-------------------------------------------------------
                        
                        tabItem(tabName = "State_Reports",
                                fluidPage(
                                  br(),
                                  wellPanel(tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'), id = "leftPanel",
                                            selectInput("state", "State", choices = unique(adm_pop_long$states)),
                                            radioButtons("adm_or_pop", "Type",   choices = unique(adm_pop_long$adm_or_pop))),
                                  
                                  mainPanel(######
                                            # State title
                                            ######
                                            fluidRow(column(width = 12,
                                                            align = "center",
                                                            br(),
                                                            textOutput("selected_state"),
                                                            tags$head(tags$style("#selected_state{font-size: 24px;
                                                                                  font-style: bold;}")),
                                                            br())),
        
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
                                      # Overview of state 
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
                                               # Table under graphs
                                               ############
                                               fluidRow(
                                                 column(width = 12,
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
                                               fluidRow(
                                                 column(width = 6,
                                                        plotlyOutput("areachart_parole", height = 300)),
                                                 column(width = 6,
                                                        plotlyOutput("barchart_parole", height = 300)),),
                                               br(), 
                                               ############
                                               # Table under graphs
                                               ############
                                               fluidRow(
                                                 column(width = 12,
                                                        reactableOutput("parole_table"))),
                                               br(),
                                               ############
                                               # BJS plot
                                               ############
                                               fluidRow(column(width = 6,
                                                               plotlyOutput("barchart_bjs_parole", height = 300))),
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
                                                               plotlyOutput("barchart_prob", height = 300))),
                                               br(), 
                                               ############
                                               # Table under graphs
                                               ############
                                               fluidRow(column(width = 12,
                                                               align = 'center',
                                                               reactableOutput("prob_table"))),
                                               br(),
                                               ############
                                               # BJS plot
                                               ############
                                               fluidRow(column(width = 6,
                                                               plotlyOutput("barchart_bjs_prob", height = 300))),
                                               br()
                                      ), #tabPanel
                                      id = "tb2") #tabsetPanel
                                  ) #mainPanel
                                ) #fluidPage
                        ), #tabItem 
                        
                        #-------------------------------------------------------
                        # Download Data
                        #-------------------------------------------------------
                        tabItem(tabName = "Download_Data",
                                
                                fluidPage(br(),
                                          wellPanel(tags$style(type="text/css", '#leftPanel { width:250px; float:left;}'), id = "leftPanel",
                                                    selectInput(inputId = "dataset",
                                                                label = "Dataset",
                                                                choices = c("More Community, Less Confinement (CSG)", "Annual Probation Survey and Annual Parole Survey (BJS)")),
                                                    
                                                    conditionalPanel(
                                                      condition = "input.dataset == 'More Community, Less Confinement (CSG)'",
                                                      checkboxGroupInput("year_table", "Year", choices = unique(csg$year), selected = "2018"),
                                                      pickerInput("download_table","State(s)", choices = unique(csg$state), options = list(`actions-box` = TRUE), multiple = T)
                                                      # selectizeInput("download_table", "State(s)", choices = unique(csg$state), multiple = TRUE, selected = "Alabama")
                                              
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.dataset == 'Annual Probation Survey and Annual Parole Survey (BJS)'",
                                                      checkboxGroupInput("year_table2", "Year", choices = unique(bjs_prob$year), selected = "2014"),
                                                      pickerInput("download_table2","State(s)", choices = unique(bjs_prob$state), options = list(`actions-box` = TRUE), multiple = T)
                                                      # selectizeInput("download_table2", "State(s)", choices = unique(bjs_prob$state), multiple = TRUE, selected = "Alabama")
                                                    )
                                          ), # wellPanel
                                          mainPanel(  
                                           br(),
                                           fluidRow(column(width = 1),
                                                    column(width = 11,
                                                           h2("Download Data"),
                                                           br(),
                                                           textOutput("selected_data"),
                                                           tags$head(tags$style("#selected_data{font-size: 20px;font-style: bold;}")),
                                                           br(),
                                                           textOutput("selected_data_info"),
                                                           br(), br())
                                           ),
                                            fluidRow(
                                              column(width = 1),
                                              column(width = 11, 
                                                     align = "center", 
                                                     DT::dataTableOutput("main_table")))
                                          ) #mainPanel
                                ) #fluidPage
                        ) #tabItem
                      ) #tabItems
                    ), #dashboardBody
                    tags$head(tags$style(HTML('* {font-family: "Arial"};')))
) #dashboardPage