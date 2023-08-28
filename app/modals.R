# re_modal <- function() {
#   showModal(
#     modalDialog(
#       title = "Race/Ethnicity Data",
#       tags$div("This tab uses data from BJS, PUMS, and NCRP and is not collected through the MCLC survey. Please read the following explanation closely. After closing this display you will be guided through the interface and data.",
#         style = "font-size: 1.5em; font-weight: bold; padding: 10px; margin: 0; background: #DDE3E9;"),
#       tags$br(),
#       tags$br(),
#       tags$p("Racial disparities start in the community", style = 'text-align: center; font-size: 2em; font-weight: bold;'),
#       tags$p("... and can be exacerbated as individuals move through the criminal justice sytem", style = 'text-align: center; font-size: 2em; font-weight: bold;'),
#       tags$img(src="different_types_of_disparities_sub.png", class = "centered-img"),
#       tags$br(),
#       tags$br(),
#       tags$div(
#         tags$span(
#           tags$u("Disparities in prison readmissions from parole"),
#           style = 'font-weight: bold;'),
#           "describes the disparities that occur when people are readmitted to prison (",
#         tags$span("red", style = "color: red; font-weight: bold;"),
#         ") arrow only",
#         style = 'text-align: left; font-size: 1.5em;'
#         ),
#       tags$div(
#         tags$span(
#           tags$u("Absolute disparities in prison readmissions from parole"),
#           style = 'font-weight: bold;'),
#         "examines the disparities that originate in the community and compound across the criminal justice system (",
#         tags$span("blue", style = 'color: blue; font-weight: bold;'),
#         "and",
#         tags$span("red", style = 'color: red; font-weight: bold;'),
#         "arrows)",
#         style = 'text-align: left; font-size: 1.5em;'),
#       easyClose = FALSE,
#       footer = actionButton("close_modal", label = "Explore")
#     )
#   )
# }

re_modal <- function() {
  showModal(modalDialog(
    title = "YouTube Video",
    tags$iframe(width = "560", height = "315", src = "LINK", frameborder = "0", allowfullscreen = NA),
    easyClose = FALSE,
    footer = actionButton("close_modal", label = "Explore")
  ))
}
