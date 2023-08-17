
# div(id = "tooltip",
#     class = "retitle",
#     role = "tooltip",
#     uiOutput("redefinition", inline = TRUE)  # add tooltip depending on pop_denom selection
# ),

# makeDropdownTooltip <- function(dropdownId, buttonLabel, tooltipContent) {
#   script <- tags$script(HTML(paste0("
#     $(document).ready(function() {
#       var dropdown = $('#", dropdownId, "');
#
#       var buttonID = 'button_' + Math.floor(Math.random()*1000);
#       var button = $('<button>')
#         .attr('id', buttonID)
#         .attr('type', 'button')
#         .addClass('btn action-button btn-inverse btn-xs')
#         .text('", buttonLabel, "');
#
#       dropdown.parent().append(button);
#
#       button.tooltip({
#         placement: 'bottom',
#         trigger: 'hover',
#         title: '", tooltipContent, "'
#       });
#     });
#   ")))
#   htmltools::attachDependencies(script, shinyBS:::shinyBSDep)
# }

# output$redefinition_tooltip <- renderUI({
#   if (input$pop_denom == "BJS") {
#     makeDropdownTooltip(
#       dropdownId = "pop_denom",
#       buttonLabel = "?",
#       tooltipContent = "This is a tooltip for BJS"
#     )
#   } else {
#     makeDropdownTooltip(
#       dropdownId = "pop_denom",
#       buttonLabel = "?",
#       tooltipContent = "This is a tooltip for Other"
#     )
#   }
# })


library(shiny)
library(shinyBS)

extendedCheckboxGroup <- function(..., extensions = list()) {
  cbg <- checkboxGroupInput(...)
  nExtensions <- length(extensions)
  nChoices <- length(cbg$children[[2]]$children[[1]])

  if (nExtensions > 0 && nChoices > 0) {
    lapply(1:min(nExtensions, nChoices), function(i) {
      # For each Extension, add the element as a child (to one of the checkboxes)
      cbg$children[[2]]$children[[1]][[i]]$children[[2]] <<- extensions[[i]]
    })
  }
  cbg
}

bsButtonRight <- function(...) {
  btn <- bsButton(...)
  # Directly inject the style into the shiny element.
  btn$attribs$style <- "float: right;"
  btn
}

server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')

    output$rendered <-   renderUI({
      extendedCheckboxGroup("qualdim", label = "Checkbox", choiceNames  = c("cb1", "cb2"), choiceValues = c("check1", "check2"), selected = c("check2"),
                            extensions = list(
                              tipify(bsButtonRight("pB1", "?", style = "inverse", size = "extra-small"),
                                     "Here, I can place some help"),
                              tipify(bsButtonRight("pB2", "?", style = "inverse", size = "extra-small"),
                                     "Here, I can place some other help")
                            ))
    })
  })
}

ui <- fluidPage(
  shinyjs::useShinyjs(),

  tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),

  # useShinyBS

  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100),
      uiOutput("rendered")
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

shinyApp(ui = ui, server = server)
