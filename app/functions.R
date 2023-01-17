#######################################
# Project: MCLCShiny
# File: functions.R
# Authors: Mari Roberts
# Date last updated: October 25, 2022
# Description:
#    Defines custom functions
#######################################

##########################
# Highchart
##########################

# Highcharts theme for hex map
hc_theme_map_jc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    chart = list(
      marginTop = 75,
      style = list(fontFamily = "Graphik")
    ),
    #title = list(style = list(fontFamily = "Graphik", fontSize = "24px")),
    #subtitle = list(style = list(fontFamily = "Graphik", fontSize = "16px")),
    caption = list(align = "right", y = 15),
    xAxis = list(
      labels = list(
        style = list(fontSize = "15px"),
        staggerLines = 2
      ),
      gridLineColor = "transparent"
    ),
    plotOptions = list(
      series = list(states = list(inactive = list(opacity = 1))),
      line = list(marker = list(enabled = TRUE)),
      spline = list(marker = list(enabled = TRUE)),
      area = list(marker = list(enabled = TRUE)),
      areaspline = list(marker = list(enabled = TRUE))
    )
  )
)

# Highcharts theme for plots
hc_theme_jc <- hc_theme(colors = c("#D25E2D", "#EDB799", "#C7E8F5", "#236ca7", "#D6C246", "#dcdcdc"),
                        chart = list(style = list(fontFamily = "Graphik", color = "#666666")),
                        title = list(align = "center", style = list(fontFamily = "Graphik", fontWeight = "bold", fontSize = "24px")),
                        #subtitle = list(align = "center", style = list(fontFamily = "Graphik", fontSize = "16px")),
                        legend = list(align = "center", verticalAlign = "top"),
                        xAxis = list(gridLineColor = "transparent",
                                     lineColor = "transparent",
                                     minorGridLineColor = "transparent",
                                     tickColor = "transparent"),
                        yAxis = list(labels = list(enabled = TRUE),
                                     gridLineColor = "transparent",
                                     lineColor = "transparent",
                                     majorGridLineColor = "transparent",
                                     minorGridLineColor = "transparent",
                                     tickColor = "transparent"),
                        plotOptions = list(line = list(marker = list(enabled = FALSE)),
                                           spline = list(marker = list(enabled = FALSE)),
                                           area = list(marker = list(enabled = FALSE)),
                                           areaspline = list(marker = list(enabled = FALSE)),
                                           arearange = list(marker = list(enabled = FALSE)),
                                           bubble = list(maxSize = "10%")))


# Highcharts download buttons
hc_setup <- function(x) {
    highcharter::hc_add_dependency(x, name = "plugins/series-label.js") %>%
    highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
    highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
    highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
    highcharter::hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    highcharter::hc_exporting(enabled = TRUE)
}

# Highchart area chart for state page
fnc_highchart_state_areachart <- function(df, title_name){
  highchart() %>%

    hc_chart(type="area") %>%
    hc_add_series(data = subset(df, metric == "Total"), name = "Total", type = "area", hcaes(x = year, y = total), color = total_co) %>%
    hc_add_series(data = subset(df, metric == "Supervision Violation"), name = "Supervision Violation", type = "area", hcaes(x = year, y = total), color = viol_co) %>%
    hc_add_series(data = subset(df, metric == "Technical Violation"), name = "Technical Violation", type = "area", hcaes(x = year, y = total), color = tech_co) %>%
    hc_add_series(data = subset(df, metric == "New Offense Violation"), name = "New Offense Violation", type = "area", hcaes(x = year, y = total), color = new_o_co) %>%

    hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020, 2021)) %>%
    hc_yAxis(title = "", labels=list(format="{value:,.0f}")) %>%

    hc_title(
      text = title_name,
      align = "center",
      style = list(fontWeight = "bold", fontSize = "16px", useHTML = TRUE)
    ) %>%

    hc_add_theme(hc_theme_jc) %>%

    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%

    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = 'TEXT.',
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TEXT."))
    )
}


# Supervision violation highchart bar chart for state page
fnc_highchart_state_barchart <- function(df, title_name){
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = df$metric) %>%
    hc_add_series(data = subset(df, metric == "Technical Violation"), name = "Technical Violation", type = "column", hcaes(x = year, y = total), color = tech_co) %>%
    hc_add_series(data = subset(df, metric == "New Offense Violation"), name = "New Offense Violation", type = "column", hcaes(x = year, y = total), color = new_o_co) %>%

    hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020, 2021)) %>%
    hc_yAxis(title = "", labels=list(format="{value:,.0f}")) %>%

    hc_title(
      text = title_name,
      align = "center",
      style = list(fontWeight = "bold", fontSize = "16px", useHTML = TRUE)
    ) %>%

    hc_add_theme(hc_theme_jc) %>%

    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%

    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = 'TEXT.',
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TEXT."))
    )
}

fnc_highchart_map <- function(df, map_filename){

  # Get minimum and maximum value
  min_map <- min(df$change, na.rm = TRUE)
  max_map <- max(df$change, na.rm = TRUE)

  # Get absolute value for comparison
  min_map_abs <- abs(min_map)
  max_map_abs <- abs(max_map)

  # Get neg or pos sign for min and max
  min_map_type <- ifelse(min_map >= 0, "positive", "negative")
  max_map_type <- ifelse(max_map >= 0, "positive", "negative")

  # Generate tile map
  # Has diverging scales when there are neg and pos values which centers the color gradient at zero
  # Has a gradient scale when both the min and max are both negative or both positive

  # Determine the new min and max so that zero is centered
  # For example, If the highest positive value is 20 than the negative value is -20
  if (min_map_type != max_map_type) {

    NEW_MAX <- case_when(
      max_map_abs > min_map_abs ~ max_map_abs,
      max_map_abs < min_map_abs ~ min_map_abs,
      max_map_abs == min_map_abs ~ max_map_abs)
    NEW_MIN <- case_when(
      min_map_abs > max_map_abs ~ min_map_abs,
      min_map_abs < max_map_abs ~ max_map_abs,
      min_map_abs == max_map_abs ~ min_map_abs)
    NEW_MAX <- ifelse(max_map_type == "negative", -abs(NEW_MAX), abs(NEW_MAX))
    NEW_MIN <- ifelse(min_map_type == "negative", -abs(NEW_MIN), abs(NEW_MIN))

    highchart() %>%

      hc_add_series_map(
        map = hex_gj,
        df = df,
        joinBy = "state_abb",
        value = "change",
        dataLabels = list(enabled = TRUE, format = "{point.datalabel}",
                          style = list(fontSize = "14px",
                                       fontWeight = "regular",
                                       fontFamily = "Graphik",
                                       textOutline = 0)),
        nullColor = "#e8e8e8") %>%

      hc_colorAxis(min = NEW_MIN,
                   max = NEW_MAX,
                   stops = color_stops(7, c(darkorange, orange, lightorange, white, lightblue, regblue, darkblue)),
                   labels = list(format = "{value}%",
                                 style = list(fontSize = "14px"))
      ) %>%

      hc_legend(align = "right",
                verticalAlign = "bottom",
                layout = "vertical",
                #padding = 10,
                symbolHeight = 200,
                symbolWidth = 25,
                x = -25,
                y = 0

      ) %>%

      hc_add_theme(hc_theme_map_jc) %>%

      hc_xAxis(title = "") %>%
      hc_yAxis(title = "") %>%
      hc_title(
        text = paste0("Change in ", unique(df$metric), " ", unique(df$adm_or_pop), " from ", unique(df$year)),
        align = "center",
        style = list(#fontFamily = "Graphik-Bold", # works in view but not in export
          fontWeight = "bold",
          fontFamily = "Graphik", # works in view and export but is the wrong font
          fontSize = "30px",
          useHTML = TRUE)
      ) %>%

      hc_setup() %>%
      hc_exporting(enabled = FALSE,
                   filename = map_filename,
                   buttons = list(
                     contextButton = list(
                       menuItems = list('downloadPNG', 'downloadSVG')
                     ))) %>%

      hc_plotOptions(series = list(animation = FALSE,
                                   dataLabels = list(enabled = TRUE),
                                   cursor = "pointer",
                                   borderWidth = 3),
                     accessibility = list(enabled = TRUE,
                                          keyboardNavigation = list(enabled = TRUE),
                                          linkedDescription = 'This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year.',
                                          landmarkVerbosity = "one"),
                     area = list(accessibility = list(description = "This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year."))
      )

  } else {


    # Determine the new min and max where all values are negative
    NEW_MAX <- max_map
    NEW_MIN <- min_map

    highchart() %>%

      hc_add_series_map(
        map = hex_gj,
        df = df,
        joinBy = "state_abb",
        value = "change",
        dataLabels = list(enabled = TRUE, format = "{point.datalabel}",
                          style = list(fontSize = "14px",
                                       fontWeight = "regular",
                                       fontFamily = "Graphik",
                                       textOutline = 0)),
        nullColor = "#e8e8e8") %>%

      hc_colorAxis(min = NEW_MIN,
                   max = NEW_MAX,
                   stops = color_stops(4, c(darkorange, orange, lightorange, white)),
                   labels = list(format = "{value}%",
                                 style = list(fontSize = "14px"))) %>%

      hc_legend(align = "right",
                verticalAlign = "bottom",
                layout = "vertical",
                #padding = 10,
                symbolHeight = 200,
                symbolWidth = 25,
                x = -25,
                y = 0) %>%

      hc_add_theme(hc_theme_map_jc) %>%


      hc_xAxis(title = "") %>%
      hc_yAxis(title = "") %>%
      hc_title(
        text = paste0("Change in ", unique(df$metric), " ", unique(df$adm_or_pop), " from ", unique(df$year)),
        align = "center",
        style = list(#fontFamily = "Graphik-Bold", # works in view but not in export
          fontWeight = "bold",
          fontFamily = "Graphik",
          fontSize = "30px",
          useHTML = TRUE)) %>%

      hc_setup() %>%
      hc_exporting(enabled = FALSE,
                   filename = map_filename,
                   buttons = list(
                     contextButton = list(
                       menuItems = list('downloadPNG', 'downloadSVG')
                     ))) %>%

      hc_plotOptions(series = list(animation = FALSE, dataLabels = list(enabled = TRUE), cursor = "pointer", borderWidth = 3),
                     accessibility = list(enabled = TRUE,
                                          keyboardNavigation = list(enabled = TRUE), linkedDescription = 'This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year.',
                                          landmarkVerbosity = "one"),
                     area = list(accessibility = list(description = "This map was created by a selected metric of interest regarding prison admissions and population. Image description: A tile map of the United States of America with a diverging color palette to show the change from the year before. The map is interactive, and the user can hover over each state to see the change from the previous year."))
      )
  }

}


##########################
# Reactable tables
##########################

# fnc_reatable_table <- function(df){
#   # create table with 4 Year trend line in last column
#   reactable(df,
#             theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
#             defaultColDef = colDef(format = colFormat(separators = TRUE), align = "center"),
#             compact = TRUE,
#             fullWidth = FALSE)
# }
#
# columns_formatting <- list(text            = colDef(name = "Metric",
#                                                     align = "left",
#                                                     minWidth = 275),
#                            `2018`          = colDef(minWidth = 95),
#                            `2019`          = colDef(minWidth = 95),
#                            `2020`          = colDef(minWidth = 95),
#                            four_yr_change = colDef(minWidth = 110,
#                                                     name = "4 Year Change",
#                                                     format = colFormat(percent = TRUE, digits = 1)),
#                            # add 4 Year trend graphs to each row
#                            total_new  = colDef(minWidth = 110,
#                                                name = "4 Year Trend",
#                                                cell = function(value, index) {
#                                                  dui_sparkline(
#                                                    data = value[[1]],
#                                                    height = 80,
#                                                    margin = list(top = 30, right = 20, bottom = 30, left = 20),
#
#                                                    components = list(
#                                                      dui_sparkpatternlines(
#                                                        id = "total",
#                                                        height = 4,
#                                                        width = 4,
#                                                        stroke = total_co,
#                                                        strokeWidth = 2.5,
#                                                        orientation = "diagonal"
#                                                      ),
#
#                                                      dui_sparkpatternlines(
#                                                        id = "sup_violations",
#                                                        height = 4,
#                                                        width = 4,
#                                                        stroke = viol_co,
#                                                        strokeWidth = 2.5,
#                                                        orientation = "diagonal"
#                                                      ),
#
#                                                      dui_sparkpatternlines(
#                                                        id = "technical",
#                                                        height = 4,
#                                                        width = 4,
#                                                        stroke = tech_co,
#                                                        strokeWidth = 2.5,
#                                                        orientation = "diagonal"
#                                                      ),
#
#                                                      dui_sparkpatternlines(
#                                                        id = "new_offense",
#                                                        height = 4,
#                                                        width = 4,
#                                                        stroke = new_o_co,
#                                                        strokeWidth = 2.5,
#                                                        orientation = "diagonal"
#                                                      ),
#
#                                                      dui_sparklineseries(
#                                                        curve = "linear",
#                                                        showArea = FALSE,
#                                                        fill = colpal_fill[index],
#                                                        stroke = colpal_stroke[index])))}))

# Reactable table with 4 Year trend line in last column
fnc_reatable_table <- function(df){
  reactable(df,
            style = list(fontFamily = "Graphik, sans-serif"
                         #fontSize = "0.875rem"
                         ),
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            defaultColDef = colDef(format = colFormat(separators = TRUE), align = "center"),
            compact = TRUE,
            fullWidth = FALSE,
            columns = list(
              text            = colDef(name = "Metric",
                                       align = "left",
                                       minWidth = 275),
              `2018`          = colDef(minWidth = 95),
              `2019`          = colDef(minWidth = 95),
              `2020`          = colDef(minWidth = 95),
              `2021`          = colDef(minWidth = 95),

              four_yr_change = colDef(minWidth = 110,
                                       name = "4 Year Change",
                                       format = colFormat(percent = TRUE, digits = 1)),
              # add 4 Year trend graphs to each row
              total_new  = colDef(minWidth = 110,
                                  name = "4 Year Trend",
                                  cell = function(value, index) {
                                    dui_sparkline(
                                      data = value[[1]],
                                      height = 80,
                                      margin = list(top = 30, right = 20, bottom = 30, left = 20),

                                      components = list(
                                        dui_sparkpatternlines(
                                          id = "total",
                                          height = 4,
                                          width = 4,
                                          stroke = total_co,
                                          strokeWidth = 2.5,
                                          orientation = "diagonal"
                                        ),

                                        dui_sparkpatternlines(
                                          id = "sup_violations",
                                          height = 4,
                                          width = 4,
                                          stroke = viol_co,
                                          strokeWidth = 2.5,
                                          orientation = "diagonal"
                                        ),

                                        dui_sparkpatternlines(
                                          id = "technical",
                                          height = 4,
                                          width = 4,
                                          stroke = tech_co,
                                          strokeWidth = 2.5,
                                          orientation = "diagonal"
                                        ),

                                        dui_sparkpatternlines(
                                          id = "new_offense",
                                          height = 4,
                                          width = 4,
                                          stroke = new_o_co,
                                          strokeWidth = 2.5,
                                          orientation = "diagonal"
                                        ),

                                        dui_sparklineseries(
                                          curve = "linear",
                                          showArea = FALSE,
                                          fill = colpal_fill[index],
                                          stroke = colpal_stroke[index])))}))
  )
}

##########################
# Value boxes
##########################

# https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/
# Value boxes
valueBox1 <- function(value, title, title2, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL){

  shinydashboard:::validateColor(color)

  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")

  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      p(HTML(paste0("<b>", title, "</b><br><br>"))),
      h1(HTML(paste0("<b>", value, "</b>"))),
      p(HTML(paste0("<b>", subtitle, "</b>")))
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )

  if (!is.null(href))
    boxContent <- a(href = href, boxContent)

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}
valueBox2 <- function(value, title, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL){

  shinydashboard:::validateColor(color)

  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")

  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      p(HTML(paste0("<b>", title, "</b>"))),
      h1(HTML(paste0("<b>", value, "</b>"))),
      p(HTML(paste0("<b>", subtitle, "</b>")))
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )

  if (!is.null(href))
    boxContent <- a(href = href, boxContent)

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}

##########################
# Data cleaning
##########################

# Text depending on data type
fnc_create_data_text <- function(df){
  df <- df %>%
    mutate(text = case_when(
      data == "total_admissions"                            ~  "Total Admissions",
      data == "total_violation_admissions"                  ~  "Supervision Violation Admissions",
      data == "total_probation_violation_admissions"        ~  "Probation Violation Admissions",
      data == "new_offense_probation_violation_admissions"  ~  "Probation New Offense Violation Admissions",
      data == "technical_probation_violation_admissions"    ~  "Probation Technical Violation Admissions",
      data == "total_parole_violation_admissions"           ~  "Parole Violation Admissions",
      data == "new_offense_parole_violation_admissions"     ~  "Parole New Offense Violation Admissions",
      data == "technical_parole_violation_admissions"       ~  "Parole Technical Violation Admissions",
      data == "other_admissions"                            ~  "Other Admissions",

      data == "total_population"                            ~  "Total Population",
      data == "total_violation_population"                  ~  "Supervision Violation Population",
      data == "total_probation_violation_population"        ~  "Probation Violation Population",
      data == "new_offense_probation_violation_population"  ~  "Probation New Offense Violation Population",
      data == "technical_probation_violation_population"    ~  "Probation Technical Violation Population",
      data == "total_parole_violation_population"           ~  "Parole Violation Population",
      data == "new_offense_parole_violation_population"     ~  "Parole New Offense Violation Population",
      data == "technical_parole_violation_population"       ~  "Parole Technical Violation Population",
      data == "other_population"                            ~  "Other Population"

    ))
}

# Metric depending on data
fnc_create_data_metric <- function(df){
  df <- df %>%
    mutate(metric = case_when(
      data == "total_admissions"                            ~  "Total",
      data == "total_violation_admissions"                  ~  "Supervision Violation",
      data == "total_probation_violation_admissions"        ~  "Probation Violation",
      data == "new_offense_probation_violation_admissions"  ~  "New Offense Violation",
      data == "technical_probation_violation_admissions"    ~  "Technical Violation",
      data == "total_parole_violation_admissions"           ~  "Parole Violation",
      data == "new_offense_parole_violation_admissions"     ~  "New Offense Violation",
      data == "technical_parole_violation_admissions"       ~  "Technical Violation",
      data == "other_admissions"                            ~  "Other",

      data == "total_population"                            ~  "Total",
      data == "total_violation_population"                  ~  "Supervision Violation",
      data == "total_probation_violation_population"        ~  "Probation Violation",
      data == "new_offense_probation_violation_population"  ~  "New Offense Violation",
      data == "technical_probation_violation_population"    ~  "Technical Violation",
      data == "total_parole_violation_population"           ~  "Parole Violation",
      data == "new_offense_parole_violation_population"     ~  "New Offense Violation",
      data == "technical_parole_violation_population"       ~  "Technical Violation",
      data == "other_population"                            ~  "Other"
    ))
}

# Prob vs parole depending on data type
fnc_create_prob_vs_parole <- function(df){
  df <- df %>%
    mutate(prob_vs_parole = case_when(
      data == "total_admissions"                            ~  "Both",
      data == "total_violation_admissions"                  ~  "Both",
      data == "total_probation_violation_admissions"        ~  "Probation",
      data == "new_offense_probation_violation_admissions"  ~  "Probation",
      data == "technical_probation_violation_admissions"    ~  "Probation",
      data == "total_parole_violation_admissions"           ~  "Parole",
      data == "new_offense_parole_violation_admissions"     ~  "Parole",
      data == "technical_parole_violation_admissions"       ~  "Parole",
      data == "other_admissions"                            ~  "Both",

      data == "total_population"                            ~  "Both",
      data == "total_violation_population"                  ~  "Both",
      data == "total_probation_violation_population"        ~  "Probation",
      data == "new_offense_probation_violation_population"  ~  "Probation",
      data == "technical_probation_violation_population"    ~  "Probation",
      data == "total_parole_violation_population"           ~  "Parole",
      data == "new_offense_parole_violation_population"     ~  "Parole",
      data == "technical_parole_violation_population"       ~  "Parole",
      data == "other_population"                            ~  "Both"

    ))
}

# Adm vs pop depending on data type
fnc_create_adm_pop <- function(df){
  df <- df %>%
    mutate(adm_or_pop = ifelse(grepl("population", data), "Population", "Admissions"))
}

##########################
# Ui
##########################

# Add a nicely styled label above selection box
labeled_input <- function(id, label, input){
  div(id = id,
      span(label, style = "font-size: small;"),
      input)
}
