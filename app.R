#######################################
# Project: MCLCShiny
# File: app.R
# Authors: Mari Roberts
# Date: September 27, 2021
# Description: 
#    User interface for R Shiny app
#######################################

##################################

# User Interface

##################################

# Define UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage(
                  
                  "More Community, Less Confinement",
                  tabPanel("About", 
                           
                           h1("More Community, Less Confinement"),
                           h2("A State-by-State Analysis on How Supervision Violations Impacted Prison Populations During the Pandemic"),
                           p("This 50-state revocation dashboard explores how supervision violations impacted prison populations during—and prior to—the pandemic. The project was conducted in partnership with the Correctional Leaders Association with support from Arnold Ventures."),
                           p("View the national report, 50 state reports, and our research methodology.")
                           
                           ),#tabPanel
                  
                  tabPanel("Data Table", 
                           
                             titlePanel("Data Table"),

                             # Create a new Row in the UI for selectInputs
                             fluidRow(
                               column(4,
                                      selectInput("states",
                                                  "states:",
                                                  c("All",
                                                    unique(as.character(mclc$states))))
                               ),
                               column(4,
                                      selectInput("year",
                                                  "year:",
                                                  c("All",
                                                    unique(as.character(mclc$year))))
                               ),
                               column(4,
                                      selectInput("metric",
                                                  "metric:",
                                                  c("All",
                                                    unique(as.character(mclc$metric))))
                               )
                             ),
                             # Create a new row for the table.
                             DT::dataTableOutput("table")
                           
                           ),#tabPanel
                  
                  tabPanel("Map", 
                           
                           "This panel is intentionally left blank"
                           
                           ) #tabPanel
                  
                ) #navbarPage
) #fluidPage

# Define server function  
server <- function(input, output) {

    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      data <- mclc
      if (input$states != "All") {
        data <- data[data$states == input$states,]
      }
      if (input$metric != "All") {
        data <- data[data$metric == input$metric,]
      }
      if (input$year != "All") {
        data <- data[data$year == input$year,]
      }
      data
    }))
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
