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
      Choose which state’s race and ethnicity data to view using this drop-down menu.
      </span>",
      cancelIcon = list(enabled = TRUE, label = "Exit the app guide"),
      buttons = list(list(action = "next",text = "Next"))
    )$
    #pick a population
    step(
      el = "#type-selector",
      title = "Select Metric Type",
      text = "<span class = 'guidetext'>
      Next, choose to view race and ethnicity data
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
      Choose to view racial and ethnic <i><b>total disparities</i></b> or <i><b>portion of disparities attributable to parole revocations</b></i> in your state and metric type selected by using this
      drop-down menu.
       <br><br><b><i>Total disparities</b></i> examine the accumulation of disparities between racial and ethnic groups for people who had their parole revoked. This metric compares the admissions and populations of people who entered prison for parole violations from a racial or ethnic group compared to the representation in the community in the same racial or ethnic group.
       <br><br><b><i>Portion of disparities attributable to parole revocations</b></i> examines the disparities in parole revocations among different racial and ethnic groups. This metric compares the admissions and populations of people who entered prison for parole violations from a racial or ethnic group compared to the representation of people on parole in the same racial or ethnic group.
      </span>",
      cancelIcon = list(enabled = TRUE, label = "Exit the app guide"),
      position = 'right'
    )$
    # focus on infograph
    step(
      el = "#infopanel-id",
      title = "Notice...",
      text = "<span class = 'guidetext'>
      The racial and ethnic total disparities or portion of disparities attributable to parole revocations in your state
      and metric type selected.
      </span>",
      cancelIcon = list(enabled = TRUE, label = "Exit the app guide"),
      id = "ip1"
    )$
    step(
      el = "#infopanel-id",
      title = "Notice...",
      text = "<span class = 'guidetext'>
      The racial and ethnic total disparities or portion of disparities attributable to parole revocations in your state
      and metric type selected.</br></br> In some states, the data to calculate the disparities in readmissions to prison from parole are not available.
      </span>",
      cancelIcon = list(enabled = TRUE, label = "Exit the app guide"),
      id = "ip2"
    )$
    # show additional data
    step(
      el = "#showtables-id",
      title = "Show Additional Data",
      text = "<span class = 'guidetext'>
      Check the box to uncover additional metrics such as Relative Rate Indices, rates of Admissions for Parole Revocations, and the number of Admissions from Parole.
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
