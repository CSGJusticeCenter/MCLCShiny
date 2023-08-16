
library(shiny)
library(shinyBS)

ui <- fluidPage(
        pickerInput(inputId = "dropdown1",
                    label = NULL,
                    choices = list("Disparities", "Cumulative Disparities")),
        bsTooltip(id = "dropdown1",
                  title = "<li>Disparities: this is a definition </li><br> <li>Cumulative Disparities: this is another definition and it is even better</li>",
                  placement = "right",
                  trigger = "hover")
)

server <- function(input, output, session) {
}

shinyApp(ui, server)


ui <- fluidPage(
  selectInput(inputId = "dropdown1",
              label = NULL,
              choices = list("Disparities", "Cumulative Disparities")),
  bsTooltip(id = "dropdown1",
            title = "<li>Disparities: this is a definition </li><br> <li>Cumulative Disparities: this is another definition and it is even better</li>",
            placement = "right",
            trigger = "hover")
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
# library(shiny)
# library(shinyBS)
#
# ui <- fluidPage(
#
#   selectInput(inputId = "dropdown1",
#               label = "",
#               choices = c("Disparities", "Cumulative Disparities")),
#   bsTooltip(id = "dropdown1",
#             title = "This is a tooltip",
#             placement = "right",
#             trigger = "hover")
#
# )
#
# server <- function(input, output, session) {
#
# }
#
# shinyApp(ui, server)

# library(shiny)
# library(shinyBS)
#
# ui <- fluidPage(
#   selectInput(inputId = "dropdown1",
#               label = "",
#               choices = c("Disparities", "Cumulative Disparities")),
#   uiOutput("tooltip")
# )
#
# server <- function(input, output, session) {
#
#   disparities_definition <- reactiveVal()
#   observe({
#     disparities_definition(
#       ifelse(input$dropdown1 == "Disparities",
#              "This is a disparities definition",
#              "This is a cumulative disparities definition"
#       ))
#   })
#
#   output$tooltip <- renderUI({
#     bsTooltip(id = "dropdown1",
#               title = disparities_definition(),
#               placement = "right",
#               trigger = "hover")
#   })
#
# }
#
# shinyApp(ui, server)

library(shiny)
library(shinyBS)

ui <- fluidPage(
  fluidRow(
    column(4,selectInput(inputId = "dropdown1",
                         label = "",
                         choices = c("Disparities", "Cumulative Disparities"))),
    column(5,htmlOutput("tooltip"))
  )
)

server <- function(input, output, session) {

  TIP <- reactiveValues()
  observe({
    TIP$a <- ifelse(input$dropdown1 =="Disparities",
                    "This is Disparities",
                    "This is Cumulative Disparities")
  })

  output$tooltip <- renderUI({
    tags$span("",
              tipify(icon("info-circle", lib = "font-awesome", style = "color: #004270"),
                     TIP$a)
    )
  })
}

shinyApp(ui, server)

# library(shiny)
# library(shinyBS)
#
# ui <- fluidPage(
#   div(style = "display: flex;",
#
#       selectInput("test",
#                   "Select Input",
#                   choices = 1:3),
#
#       pickerInput(
#         inputId = "somevalue",
#         label = "A label",
#         choices = c("a", "b")
#       )
#   )
# )
#
# server <- function(input, output, session) {
#
# }
#
# shinyApp(ui, server)

# output$redefinition <- renderUI({
#   tippy(
#     icon(name = "info-circle",
#          lib = "font-awesome",
#          style = "font-size: 0.5em; color: #004270"),
#     tooltip = disparities_definition(),
#     interactive = TRUE,
#     theme = "light"
#   )
# })
# output$redefinition <- renderUI({
#   tippy(
#     icon(name = "info-circle", lib = "font-awesome", style = "font-size: 0.5em; color: #004270"),
#     tooltip = paste0("<tooltip role='tooltip'
#                         aria-label='Definitions for disparities and cumulative disparities.'
#                         style='font-family: Graphik;
#                                font-size: 2em;'>",
#                      disparities_definition(),
#                      "</tooltip>"),
#     interactive = TRUE,
#     placement = "right",
#     theme = "light"
#   )
