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
                                    tags$head(tags$style("#selected_state{font-size: 24px;
                                                                          font-style: bold;}")),
                                    br(),
                                    textOutput("selected_state_adm_pop"),
                                    tags$head(tags$style("#selected_state_adm_pop{font-size: 14px
                                                                                  font-style: regular;}")),
                                    br(),
                                    tabsetPanel(
                                      ###################
                                      # Overview of state report
                                      ###################
                                      tabPanel(value="1","Overview", 
                                               br(),
                                               fluidRow(
                                               # total
                                               box(plotOutput("barchart", height = 250)),
                                               # box(plotlyOutput("barchart")),
                                               box(plotOutput("areachart",height = 250))
                                               ) #fluidRow
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
    paste("Trends in ", input$state)
  })  
  
  # Print state name and adm or pop selected
  output$selected_state_adm_pop <- renderText({ 
    paste("This snapshot shows available data for ", input$state, " from 2018 to 2020.")
  })
  
  # Totals areachart
  output$areachart <- renderPlot({ 
    
    df_totals <- 
      adm_pop_long %>% 
      filter(states == input$state &
               adm_or_pop == input$adm_or_pop) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "Other" | metric == "Supervision Violations")  
    
    totals <- df_totals %>%
      group_by(year) %>%
      summarise(total = sum(total))
    
    dodger = position_dodge(width = 0.9)
    
    title <- paste0(input$adm_or_pop, " in " ,input$state, " by Type\n")
    
    ggplot(df_totals, aes(x=year, y=total, fill=metric)) + 
      geom_area()+
      theme_csgjc_areaplot() +
      geom_text(size = 5.5, aes(label=ifelse(year != min(year) & year != max(year),total1, NA)),position = position_stack(vjust = 0.5), check_overlap = TRUE) +
      geom_text(size = 5.5, aes(x = year + 0.18,label=ifelse(year == min(year),total1, NA)),position = position_stack(vjust = 0.5), check_overlap = TRUE) +
      geom_text(size = 5.5, aes(x = year - 0.18,label=ifelse(year == max(year),total1, NA)),position = position_stack(vjust = 0.5), check_overlap = TRUE) +
      scale_fill_manual(values = c("#DEF0F6","#E18731"), 
                        labels = c("Other", "Supervision Violations"), 
                        breaks = c("other_admissions", "admissions_for_violations"),
                        name = "") +  
      scale_x_continuous(breaks = c(2018,2019,2020), labels = c("2018", "2019", "2020")) 
  })
  
  # Totals barchart
  output$barchart <- renderPlot({
    
    df_totals <- 
      adm_pop_long %>% 
      filter(states == input$state &
             adm_or_pop == input$adm_or_pop) %>% 
      group_by(metric, year) %>% 
      summarise(total = sum(total)) %>% 
      filter(metric == "Other" | metric == "Supervision Violations")  
    
    totals <- df_totals %>%
      group_by(year) %>%
      summarise(total = sum(total))
    
    dodger = position_dodge(width = 0.9)
    
    title <- paste0(input$adm_or_pop, " in " ,input$state, " by Type\n")
    
    ggplot(data = df_totals,
           aes_string(x = 'year', y = 'total', fill = 'metric')) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label=scales::comma(total)),
                position=dodger,
                size = 4.5,
                colour = "#000000",
                vjust = -0.5) +
      ggtitle(title) +
      theme_csgjc_plot_legend() +
      theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      scale_fill_manual(values = c("#DEF0F6", "#E18731"),
                        name = "") +
      scale_y_continuous(label = scales::comma,
                         limits = c(0, 1.17*max(df_totals$total)),
                         expand = c(0,0)
      ) +
      coord_cartesian(clip = "off")
    
  })
  
}

shinyApp(ui = ui, server = server)
