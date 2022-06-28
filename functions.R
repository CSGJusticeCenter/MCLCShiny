#######################################
# Project: MCLCShiny
# File: functions.R
# Authors: Mari Roberts
# Date: June 10, 2022
# Description:
#    Defines custom functions
#######################################

##########################
# custom functions
##########################

# add a nicely styled label above selection box
labeled_input <- function(id, label, input){
  div(id = id,
      span(label, style = "font-size: small;"),
      input)
}

# custom highcharts theme for hex map
hc_theme_map_jc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    chart = list(
      marginTop = 75,
      style = list(fontFamily = default_fonts)
    ),
    title = list(style = list(fontFamily = default_fonts, fontSize = "24px")),
    subtitle = list(style = list(fontFamily = default_fonts, fontSize = "16px")),
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

# custom highcharts theme for plots
hc_theme_jc <- hc_theme(colors = c("#D25E2D", "#EDB799", "#C7E8F5", "#236ca7", "#D6C246", "#dcdcdc"),
                        chart = list(style = list(fontFamily = default_fonts, color = "#666666")),
                        title = list(align = "left", style = list(fontFamily = default_fonts, fontSize = "24px")),
                        subtitle = list(align = "left", style = list(fontFamily = default_fonts, fontSize = "16px")),
                        legend = list(align = "left", verticalAlign = "top"),
                        xAxis = list(gridLineColor = "transparent", lineColor = "transparent", minorGridLineColor = "transparent", tickColor = "transparent"),
                        yAxis = list(labels = list(enabled = FALSE), gridLineColor = "transparent", lineColor = "transparent", minorGridLineColor = "transparent", tickColor = "transparent"),
                        plotOptions = list(line = list(marker = list(enabled = FALSE)),
                                           spline = list(marker = list(enabled = FALSE)),
                                           area = list(marker = list(enabled = FALSE)),
                                           areaspline = list(marker = list(enabled = FALSE)),
                                           arearange = list(marker = list(enabled = FALSE)),
                                           bubble = list(maxSize = "10%")))


# # set up highcharts download buttons
# hc_setup <- function(x) {
#
#   hc_exporting(enabled = TRUE) %>%
#   hc_add_dependency(name = "plugins/series-label.js") %>%
#   hc_add_dependency(name = "plugins/accessibility.js") %>%
#   hc_add_dependency(name = "plugins/exporting.js") %>%
#   hc_add_dependency(name = "plugins/export-data.js") %>%
#   hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
#
#   hc_yAxis(title = "") %>%
#   hc_add_theme(hc_theme_jc())
#
# }

# create text depending on data type
fnc_create_data_text <- function(df){
  df <- df %>%
    mutate(text = case_when(
      data == "total_admissions"                            ~  "Total Admissions",
      data == "total_violation_admissions"                  ~  "Supervision Violation Admissions",
      data == "total_probation_violation_admissions"        ~  "Probation Violation Admissions",
      data == "new_offense_probation_violation_admissions"  ~  "Probation New Offense Admissions",
      data == "technical_probation_violation_admissions"    ~  "Probation Technical Admissions",
      data == "total_parole_violation_admissions"           ~  "Parole Violation Admissions",
      data == "new_offense_parole_violation_admissions"     ~  "Parole New Offense Admissions",
      data == "technical_parole_violation_admissions"       ~  "Parole Technical Admissions",
      data == "other_admissions"                            ~  "Other Admissions",

      data == "total_population"                            ~  "Total Population",
      data == "total_violation_population"                  ~  "Supervision Violation Population",
      data == "total_probation_violation_population"        ~  "Probation Violation Population",
      data == "new_offense_probation_violation_population"  ~  "Probation New Offense Population",
      data == "technical_probation_violation_population"    ~  "Probation Technical Population",
      data == "total_parole_violation_population"           ~  "Parole Violation Population",
      data == "new_offense_parole_violation_population"     ~  "Parole New Offense Population",
      data == "technical_parole_violation_population"       ~  "Parole Technical Population",
      data == "other_population"                            ~  "Other Population"

    ))
}

# create metric depending on data
fnc_create_data_metric <- function(df){
  df <- df %>%
    mutate(metric = case_when(
      data == "total_admissions"                            ~  "Total",
      data == "total_violation_admissions"                  ~  "Supervision Violation",
      data == "total_probation_violation_admissions"        ~  "Probation Violation",
      data == "new_offense_probation_violation_admissions"  ~  "New Offense",
      data == "technical_probation_violation_admissions"    ~  "Technical Violation",
      data == "total_parole_violation_admissions"           ~  "Parole Violation",
      data == "new_offense_parole_violation_admissions"     ~  "New Offense",
      data == "technical_parole_violation_admissions"       ~  "Technical Violation",
      data == "other_admissions"                            ~  "Other",

      data == "total_population"                            ~  "Total",
      data == "total_violation_population"                  ~  "Supervision Violation",
      data == "total_probation_violation_population"        ~  "Probation Violation",
      data == "new_offense_probation_violation_population"  ~  "New Offense",
      data == "technical_probation_violation_population"    ~  "Technical Violation",
      data == "total_parole_violation_population"           ~  "Parole Violation",
      data == "new_offense_parole_violation_population"     ~  "New Offense",
      data == "technical_parole_violation_population"       ~  "Technical Violation",
      data == "other_population"                            ~  "Other"
    ))
}

# create adm vs pop depending on data
fnc_create_adm_pop <- function(df){
  df <- df %>%
    mutate(adm_or_pop = ifelse(grepl("population", data), "Population", "Admissions"))
}

# clean bjs probation data sets
clean_bjs_prob <- function(df){

  df$state <- gsub('/c','',df$state)
  df$state <- gsub('/b','',df$state)
  df$state <- gsub('/d','',df$state)
  df$state <- gsub('[[:punct:]]+','',df$state)
  df$state <- gsub('[[:digit:]]+', '', df$state)
  # df$state <- gsub('†','',df$state)
  df$state <- gsub("\u2020", "", df$state)
  df$state <- gsub('*','',df$state)
  df$state <- trimws(df$state, whitespace = "[\\h\\v]")

  # remove DC
  df <- df %>% filter(state != "District of Columbia" & state != "US total" & state != "District Of Columbia")

  # get indices for alabama and wyoming to subset rows to rows with values
  alabama <- which(df$state == "Alabama")
  wyoming <- which(df$state == "Wyoming")

  # remove NA rows
  df <- df[alabama:wyoming,]

}

# clean bjs parole data sets
clean_bjs_parole <- function(df){

  df$state <- gsub('/c','',df$state)
  df$state <- gsub('/b','',df$state)
  df$state <- gsub('/d','',df$state)
  df$state <- gsub('[[:punct:]]+','',df$state)
  df$state <- gsub('[[:digit:]]+', '', df$state)
  # df$state <- gsub('†','',df$state)
  df$state <- gsub("\u2020", "", df$state)
  df$state <- gsub('*','',df$state)
  df$state <- trimws(df$state, whitespace = "[\\h\\v]")

  # remove DC
  df <- df %>% filter(state != "District of Columbia" & state != "US total" & state != "District Of Columbia")

  # get indices for alabama and wyoming to subset rows to rows with values
  alabama <- which(df$state == "Alabama")
  wyoming <- which(df$state == "Wyoming")

  # remove NA rows
  df <- df[alabama:wyoming,]

}

# create incarcerated variable
incarcerated_bjs_prob <- function(df){
  df[] <- lapply(df, gsub, pattern = "..", replacement = NA, fixed = TRUE)
  df[] <- lapply(df, gsub, pattern = ",", replacement = "", fixed = TRUE)
  df$inc_new_sentence <- as.numeric(df$inc_new_sentence)
  df$inc_current_sentence <- as.numeric(df$inc_current_sentence)
  df <- df %>% rowwise() %>% mutate(incarcerated = sum(inc_current_sentence, inc_new_sentence))
  df <- data.frame(df)
  df <- df %>% select(state, year, type, incarcerated)
}

# create incarcerated variable
incarcerated_bjs_parole <- function(df){
  df[] <- lapply(df, gsub, pattern = "..", replacement = NA, fixed = TRUE)
  df[] <- lapply(df, gsub, pattern = ",", replacement = "", fixed = TRUE)
  df$inc_new_sentence <- as.numeric(df$inc_new_sentence)
  df$inc_revocation <- as.numeric(df$inc_revocation)
  df <- df %>% rowwise() %>% mutate(incarcerated = sum(inc_revocation, inc_new_sentence))
  df <- data.frame(df)
  df <- df %>% select(state, year, type, incarcerated)
}

# make parole data long form
bjs_parole_long_form <- function(df){

  df <- gather(df,
               data,
               total,
               incarcerated,
               factor_key=TRUE)
}

# make probation data long form
bjs_prob_long_form <- function(df){

  df <- gather(df,
               data,
               total,
               incarcerated,
               factor_key=TRUE)
}

# https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/
# create value boxes
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

