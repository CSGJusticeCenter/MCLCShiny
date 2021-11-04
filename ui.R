
# define UI
ui <- fluidPage(
  
  # theme
  # update theme https://medium.com/analytics-vidhya/building-custom-r-shiny-ui-66d446ef4dad
  theme = shinytheme("flatly"),
  
  navbarPage(
    
    #_________________________________________________________________________________________
    # 1) About
    #_________________________________________________________________________________________
    
    "More Community, Less Confinement",
    tabPanel("About", 
             
             h1("More Community, Less Confinement"),
             h2("A State-by-State Analysis on How Supervision Violations Impacted Prison Populations During the Pandemic"),
             p("This 50-state revocation dashboard explores how supervision violations impacted prison populations during—and prior to—the pandemic. The project was conducted in partnership with the Correctional Leaders Association with support from Arnold Ventures."),
             p("View the national report, 50 state reports, and our research methodology.")
             
    ),#tabPanel
    
    #_________________________________________________________________________________________
    # 2) Dashboard
    #_________________________________________________________________________________________
    
    tabPanel("Dashboard",

             ########
             # 2A) side bar with drop downs and instructions
             # eventually would like to have the option to hide the panel????
             # https://stackoverflow.com/questions/42159804/how-to-collapse-sidebarpanel-in-shiny-app
             ########
             fluidRow(
                      column(width = 6, 
                             wellPanel(
                             #1) Select state
                             selectInput(inputId = "state", label = "Select State", choices = unique(adm_pop_long$states))
                             ) #wellPanel
                      ),#column
                      
                      column(width = 6, 
                             wellPanel(
                             #2) Select adm or pop for plot
                             selectInput(inputId = "adm_or_pop", label="Admissions or Population", choices = unique(adm_pop_long$adm_or_pop))
                             ) #wellPanel
                      ) #column
             ), #fluidRow
             
             ########
             # 2B state title
             ########
             fluidRow(column(width = 1,
                             ""),
                      align = "center",
                      column(width = 10,
                             #1) Text output
                             textOutput("selected_state"),
                             tags$head(tags$style("#selected_state{color: #000000;
                                                                     font-size: 42px;
                                                                     font-style: bold;
                                                    }")),
                             # add spacing
                             br(),
                             
                             #2) Paragraph about data
                             # need to make adm and pop lowercase??????
                             textOutput("selected_state_adm_pop"),
                             tags$head(tags$style("#selected_state_adm_pop{color: #000000;
                                                                           font-size: 16px;
                                                    }")),
                             
                             # add spacing
                             br(),
                             br()
                             
                             ), #column
                      column(width = 1,
                             "")
             ), #fluidRow
             
             ########
             # 2C Plots 
             ########
             fluidRow(column(width = 6, 
                             # Bar chart 
                             plotOutput("barchart_2")
                             ),
                      
                      column(width = 6, 
                             # Area chart 
                             plotOutput("areachart_2")
                             )
                      
             ), #fluidRow
             
             ########
             # 2D Changes from 2018 and 2019
             ########
             
             
             
    ), #tabPanel
    
    #_________________________________________________________________________________________
    # 3) View Data
    #_________________________________________________________________________________________
    
    tabPanel("View Data",
             
             titlePanel("View Data"),
             
             # create table
             DTOutput('table')
             
    ), #tabPanel
    
    #_________________________________________________________________________________________
    # 4) Map
    #_________________________________________________________________________________________
    
    tabPanel("Map") #tabPanel
    
  ) #navbarPage
) #fluidPage
