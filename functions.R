#######################################
# Project: MCLCShiny
# File: import.R
# Authors: Mari Roberts
# Date: April 27, 2022
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

# custom highcharts theme
hc_theme_jc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    colors = c(
      "#1795BF",
      "#68C6A8",
      "#F0EA44",
      "#E1B32D",
      "#001F35"
    ),
    chart = list(
      marginTop = 75,
      style = list(fontFamily = default_fonts)
    ),
    title = list(style = list(fontFamily = default_fonts, fontSize = "24px")),
    subtitle = list(style = list(fontFamily = default_fonts, fontSize = "16px")),
    legend = list(align = "right", verticalAlign = "bottom", layout = "vertical"),
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

# set up highcharts download buttons
hc_setup <- function(x) {
  hc_add_dependency(x, name = "modules/exporting.js") %>%
    hc_add_dependency(name = "modules/offline-exporting.js") %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(contextButton = list(menuItems = list("printChart", "downloadPNG", "downloadSVG", "downloadPDF")))
    ) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_plotOptions(series = list(animation = FALSE)) %>%
    hc_xAxis(
      title = "",
      labels = list(y = 25)
    ) %>%
    hc_yAxis(
      title = "",
      labels = list(format = "{value:,.0f}")
    )
}

# assign labels depending on data type
create_data_text <- function(df){
df <- df %>%
  mutate(text = case_when(
    data == "total_admissions"                            ~  "Total Admissions",
    data == "total_violation_admissions"                  ~  "Supervision Violation Admissions",
    data == "total_probation_violation_admissions"        ~  "Probation Violation Admissions",
    data == "new_offense_probation_violation_admissions"  ~  "Probation New Offense Admisisons",
    data == "technical_probation_violation_admissions"    ~  "Probation Technical Admisisons",
    data == "total_parole_violation_admissions"           ~  "Parole Violation Admissions",
    data == "new_offense_parole_violation_admissions"     ~  "Parole New Offense Admisisons",
    data == "technical_parole_violation_admissions"       ~  "Parole Technical Admisisons",

    data == "total_population"                            ~  "Total Population",
    data == "total_violation_population"                  ~  "Supervision Violation Population",
    data == "total_probation_violation_population"        ~  "Probation Violation Population",
    data == "new_offense_probation_violation_population"  ~  "Probation New Offense Population",
    data == "technical_probation_violation_population"    ~  "Probation Technical Population",
    data == "total_parole_violation_population"           ~  "Parole Violation Population",
    data == "new_offense_parole_violation_population"     ~  "Parole New Offense Population",
    data == "technical_parole_violation_population"       ~  "Parole Technical Population"
  ))
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
      tags$small(title),
      h3(value),
      p(subtitle)
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
