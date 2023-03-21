# TODO Finish me...
# tooltiptext <- function(text, ttext, class = NULL, tclass = NULL) {
#    stopifnot("text must be a string." = is.character(text))
#    stopifnot("text must be a string." = is.character(ttext))
#    if (!is.null(class)) stopifnot("text must be a string." = is.character(class))
#    if (!is.null(class)) stopifnot("text must be a string." = is.character(tclass))

#    .class <- ifelse(class, class, 'tooltiptext')
#    .tclass <- ifelse(tclass, tclass, 'tooltip')

#     tags$div(
#         class = .class, 
#         text,
#         tags$span(
#             class = .tclass,
#             ttext
#             ),
#         # Add CSS
#         tags$style(
#             HTML(
#                 sprintf(
#                 "
#                 <style>
#                     .%s {
#                         position: relative;
#                         display: inline-block;
#                         border-bottom: 1px dotted black;
#                     }
#                      .%s .%s {
#                         visibility: hidden;
#                         width: 120px;
#                         background-color: black;
#                         color: #fff;
#                         text-align: center;
#                         border-radius: 6px;
#                         padding: 5px 0;

#                         position: absolute;
#                         z-index: 1;
#                     }

#                     .%s:hover .%s {
#                         visibility: visible;
#                     }
#                 </style>
#                 ",
#                 .tclass, .tclass, .class, .tclass, .class
#                 )
#             )
#         )
#     ) 
# }

# tooltiptext(text = "foo", ttext = "bar")

first_guide <- Conductor$
    new(exitOnEsc = TRUE, keyboardNavigation = TRUE)$
    step(el = "#state-selector",
        title = "Select a state",
        text = "Choose which state’s race/ethnicity data to view using this drop-down menu.")$
    #pick a population
    step(el = "#type-selector",
         title = "Select Metric Type",
         text = "Next, choose to view race/ethnicity data in your state selection in prison <i>Admissions</i> or the prison <i>Population</i> using this drop-down menu.")$
    # pick a denominator
    step(el = "#denominator-picker",
         title = "Disparities",
         text = "Choose to view racial and ethnic <i>disparities</i> or <i>cumulative disparities</i> in your state and metric type selected by using this drop-down menu.",
         position = 'top')$
    # focus on infograph
    step(el = "#infopanel-id",
         title = "Notice...",
         text = "The racial and ethnic disparities or cumulative disparities in your state and metric type selected.",
         id = "ip1")$#, on_next = "let test = document.getElementsByClassName('driver-close-btn')[0]")$
    step(el = "#infopanel-id",
         title = "Notice...",
         text = "The racial and ethnic disparities or cumulative disparities in your state and metric type selected.</br></br> In some state, the data to calculate the disparities in parole revocations are not available.",
         id = "ip2")$
    # show additional data 
    step(el = "#showtables-id",
         title = "Show Additional Data",
         text = "Check the box to uncover additional metrics such as Relative Rate Indices, Rates of Readmissions to Prison from Parole, and Counts of Readmissions to Prison from Parole.")$
     # show the guide call button
     step(el = "#guide-button",
          title = "Get Help",
          text = "To call back the help dialog box, click here.",
          buttons = list(
               list(
                    action = "next",
                    text = "Finish"
               )
          ))
