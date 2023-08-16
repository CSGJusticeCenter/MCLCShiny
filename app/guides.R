first_guide <- Conductor$
    new(
      exitOnEsc = TRUE,
      keyboardNavigation = TRUE,
      onComplete = "document.body.scrollTop = document.documentElement.scrollTop = 0;"
    )$
    step(
      el = "#state-selector",
      title = "Select a state",
      text = "<span class = 'guidetext'>
      Choose which state’s race/ethnicity data to view using this drop-down menu.
      </span>",
      cancelIcon = list(enabled = TRUE, label = "Exit the app guide"),
      buttons = list(list(action = "next",text = "Next"))
    )$
    #pick a population
    step(
      el = "#type-selector",
      title = "Select Metric Type",
      text = "<span class = 'guidetext'>
      Next, choose to view race/ethnicity data
      in your state selection in prison <i>Admissions</i> or the
      prison <i>Population</i> using this drop-down menu.
      </span>",
      cancelIcon = list(enabled = TRUE, label = "Exit the app guide"),
    )$
    # pick a denominator
    step(
      el = "#denominator-picker",
      title = "Disparities",
      text = "<span class = 'guidetext'>
      Choose to view racial and ethnic <i><b>disparities</i></b> or <i><b>cumulative
      disparities</b></i> in your state and metric type selected by using this
      drop-down menu.
       <br><br><b><i>Disparities</b></i> in parole returns examines the disparities between the prison re-admissions and re-incarceration rates for individuals who had been previously released on parole among different racial and ethnic groups. The rate numerator is the number of people who were admitted to prison from parole from a race or ethnic group compared to the number of people serving sentences of parole in the same race or ethnic group.
       <br><br><b><i>Cumulative disparities</b></i> for parole returns represents the accumulation of disparities between race and ethnic groups for people who have returned to prison from parole. This metric compares the admissions and populations of individuals who returned to prison from parole from each racial and ethnic group to their representation in the community.
      </span>",
      cancelIcon = list(enabled = TRUE, label = "Exit the app guide"),
      position = 'bottom'
    )$
    # focus on infograph
    step(
      el = "#infopanel-id",
      title = "Notice...",
      text = "<span class = 'guidetext'>
      The racial and ethnic disparities or cumulative disparities in your state
      and metric type selected.
      </span>",
      cancelIcon = list(enabled = TRUE, label = "Exit the app guide"),
      id = "ip1"
    )$
    step(
      el = "#infopanel-id",
      title = "Notice...",
      text = "<span class = 'guidetext'>
      The racial and ethnic disparities or cumulative disparities in your state
      and metric type selected.</br></br> In some states, the data to calculate
      the disparities in parole revocations are not available.
      </span>",
      cancelIcon = list(enabled = TRUE, label = "Exit the app guide"),
      id = "ip2"
    )$
    # show additional data
    step(
      el = "#showtables-id",
      title = "Show Additional Data",
      text = "<span class = 'guidetext'>
      Check the box to uncover additional metrics such as Relative Rate Indices,
      Rates of Readmissions to Prison from Parole, and Counts of Readmissions to
      Prison from Parole.
      </span>",
      cancelIcon = list(enabled = TRUE, label = "Exit the app guide"),
    )$
     # show the guide call button
    step(
      el = "#guide-button",
      title = "Get Help",
      text = "<span class = 'guidetext'>
      To call back the help dialog box, click here.
      </span>",
      cancelIcon = list(enabled = TRUE, label = "Exit the app guide"),
      buttons = list(list(action = "next", text = "Finish"))
    )

## NOTES
# if using X box to exit (cancelIcon = list(enabled = TRUE, label = 'Exit app guide'))
# the tile of the guide box is shifted to the left
# this can be compensated by adding text-indent: 5px to the shepherd-title css
