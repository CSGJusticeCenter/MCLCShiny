#######################################
# Project: MCLCShiny
# File: import.R
# Authors: Mari Roberts
# Sub-Author: Martha Eichlersmith
# Date last updated: May 30, 2023 (MAR)

# Description:
#    Loads packages
#    Loads custom functions needed for data cleaning and visuals
#######################################

# Path to data on research div sharepoint
# Make sure sharepoint folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# In your Renviron (usethis::edit_r_environ()), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"

# highcharter download instructions:
# remove the existing highcharter package from your R session: remove.packages("highcharter")
# restart your R session
# install highcharter with the devtools package (NOT the remotes package):
# install.packages("devtools")
# devtools::install_github("mrjoh3/highcharter")

# csgjcr download instructions:
# install.packages("remotes")
# remotes::install_github("csgjusticecenter/csgjcr")

# Load packages
library(csgjcr)
library(rlang)
library(dplyr)
library(tidyr)
library(geojsonsf)
library(janitor)
library(jsonlite)
library(readxl)
library(sf)
library(showtext)
library(sysfonts)
library(utils)
library(highcharter)
library(extrafont)
library(readr)
library(htmltools)

box::use(prep/box/admin)

# Load fonts
font_add("Graphik",     regular = "app/www/fonts/Graphik.ttf")
font_add("GraphikBold", regular = "app/www/fonts/GraphikBold.ttf")

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

# Create trend line data for reactable table on map explorer page
fnc_create_trend_data <- function(df){
  df <- df %>%
    group_by(state, data) %>%
    summarise(total_new = list(list(total))) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      vec_nona = list(total_new[[1]][!is.na(total_new[[1]])])
      , length_nona = length(vec_nona)
      , first  = ifelse(length_nona == 0, NA, vec_nona[1])
      , last   = ifelse(length_nona == 0, NA, vec_nona[length_nona])
      , trend = case_when(
        first == last ~ "same"
        , first >  last ~ "negative" #trend is negative, decreasing
        , first <  last ~ "positive" #trend is positive, increasing
      )
    ) %>%
    select(state, data, total_new, trend)
}

##########################
# Highchart
##########################

# CSG logo
render_image <- JS("
  function(){
    this.renderer.image('https://csg-state-violent-crime.netlify.app/img/csgjc-logo.png', 30, this.chartHeight - 37, 140.1, 30)
    .add();
  }")


# Highcharts theme for hex map
hc_theme_map_jc <-
  hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    chart = list(
      marginTop = 75,
      style = list(fontFamily = "Graphik",
                   align = "center")
    ),
    title = list(align = "center",
                 style = list(fontFamily = "Graphik",
                              fontWeight = "bold",
                              color = "black",
                              fontSize   = "30px")),
    subtitle = list(align = "center",
                    style = list(fontFamily = "Graphik",
                                 fontWeight = "bold",
                                 color = "black",
                                 fontSize   = "30px")),
    caption = list(align = "center", y = 15),
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
                        chart = list(style = list(fontFamily = "Graphik",
                                                  color      = "#666666")),
                        title = list(align = "center",
                                     style = list(fontFamily = "Graphik",
                                                  fontWeight = "bold",
                                                  color = "black",
                                                  fontSize   = "16px")),
                        subtitle = list(align = "center",
                                        style = list(fontFamily = "Graphik",
                                                     fontWeight = "bold",
                                                     color = "black",
                                                     fontSize   = "14px")),
                        legend = list(align         = "center",
                                      verticalAlign = "top"),
                        xAxis = list(gridLineColor      = "transparent",
                                     lineColor          = "transparent",
                                     minorGridLineColor = "transparent",
                                     tickColor          = "transparent"),
                        yAxis = list(labels = list(enabled = TRUE),
                                     gridLineColor      = "transparent",
                                     lineColor          = "transparent",
                                     majorGridLineColor = "transparent",
                                     minorGridLineColor = "transparent",
                                     tickColor          = "transparent"),
                        plotOptions = list(line       = list(marker = list(enabled = FALSE)),
                                           spline     = list(marker = list(enabled = FALSE)),
                                           area       = list(marker = list(enabled = FALSE)),
                                           areaspline = list(marker = list(enabled = FALSE)),
                                           arearange  = list(marker = list(enabled = FALSE)),
                                           bubble     = list(maxSize = "10%")))


# Highcharts download buttons
hc_setup <- function(x) {
  highcharter::hc_add_dependency(x, name = "plugins/series-label.js") %>%
    highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
    highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
    highcharter::hc_add_dependency(name = "plugins/export-data.js")
  # highcharter::hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
  # highcharter::hc_exporting(enabled = TRUE)
}

# Highchart area chart for state page
fnc_highchart_state_areachart <- function(df, title_name, state_name, adm_or_pop){

  subtitle_name <- df %>% filter(metric == "Supervision Violation") %>%
    select(probation_or_parole)
  subtitle_name <- unique(subtitle_name$probation_or_parole)

  highchart() %>%

    hc_chart(type="area") %>%
    hc_add_series(data = subset(df, metric == "Total"),
                  name = "Total",
                  type = "area",
                  hcaes(x = year, y = total),
                  color = total_co,
                  accessibility = list(
                    enabled = TRUE,
                    keyboardNavigation = list(enabled = TRUE),
                    point = list(valueDescriptionFormat =
                                   "{point.state}, {point.year}, {point.metric}, {point.adm_or_pop}, {point.total:,.0f}"))
    ) %>%
    hc_add_series(data = subset(df, metric == "Supervision Violation"),
                  name = "Supervision Violation",
                  type = "area",
                  hcaes(x = year, y = total),
                  color = viol_co,
                  accessibility = list(
                    enabled = TRUE,
                    keyboardNavigation = list(enabled = TRUE),
                    point = list(valueDescriptionFormat =
                                   "{point.state}, {point.year}, {point.metric}, {point.adm_or_pop}, {point.total:,.0f}"))
    ) %>%
    hc_add_series(data = subset(df, metric == "Technical Violation"),
                  name = "Technical Violation",
                  type = "area",
                  hcaes(x = year, y = total),
                  color = tech_co,
                  accessibility = list(
                    enabled = TRUE,
                    keyboardNavigation = list(enabled = TRUE),
                    point = list(valueDescriptionFormat =
                                   "{point.state}, {point.year}, {point.metric}, {point.adm_or_pop}, {point.total:,.0f}"))
    ) %>%
    hc_add_series(data = subset(df, metric == "New Offense Violation"),
                  name = "New Offense Violation",
                  type = "area",
                  hcaes(x = year, y = total),
                  color = new_o_co,
                  accessibility = list(
                    enabled = TRUE,
                    keyboardNavigation = list(enabled = TRUE),
                    point = list(valueDescriptionFormat =
                                   "{point.state}, {point.year}, {point.metric}, {point.adm_or_pop}, {point.total:,.0f}"))
    ) %>%

    hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020, 2021)) %>%
    hc_yAxis(title = "", labels=list(format="{value:,.0f}")) %>%

    hc_title(text = title_name) %>%
    hc_subtitle(text = subtitle_name) %>%

    # hc_setup() %>%
    hc_add_theme(hc_theme_jc) %>%

    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%

    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = paste0("This is an area chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to supervision violations,
                                                               subset by technical violations and new offense violations."),
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = paste0("This is an area chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to supervision violations,
                                                               subset by technical violations and new offense violations.")))
    )
}


# Supervision violation highchart bar chart for state page
fnc_highchart_state_barchart <- function(df, title_name, state_name, adm_or_pop){

  subtitle_name <- df %>%
    filter(metric == "Technical Violation") %>%
    select(probation_or_parole)
  subtitle_name <- unique(subtitle_name$probation_or_parole)

  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = df$metric) %>%
    hc_add_series(data = subset(df, metric == "Technical Violation"),
                  name = "Technical Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = tech_co,
                  accessibility = list(
                    enabled = TRUE,
                    keyboardNavigation = list(enabled = TRUE),
                    point = list(valueDescriptionFormat =
                                   "{point.state}, {point.year}, {point.metric}, {point.adm_or_pop}, {point.total:,.0f}"))
    ) %>%
    hc_add_series(data = subset(df, metric == "New Offense Violation"),
                  name = "New Offense Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = new_o_co,
                  accessibility = list(
                    enabled = TRUE,
                    keyboardNavigation = list(enabled = TRUE),
                    point = list(valueDescriptionFormat =
                                   "{point.state}, {point.year}, {point.metric}, {point.adm_or_pop}, {point.total:,.0f}"))
    ) %>%

    hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020, 2021)) %>%
    hc_yAxis(title = "", labels=list(format="{value:,.0f}")) %>%

    hc_title(text = title_name) %>%
    hc_subtitle(text = subtitle_name) %>%

    # hc_setup() %>%
    hc_add_theme(hc_theme_jc) %>%

    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%

    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = paste0("This is a bar chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to supervision violations,
                                                               subset by technical violations and new offense violations."),
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = paste0("This is a bar chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to supervision violations,
                                                               subset by technical violations and new offense violations.")))
    )
}

# Parole highchart bar chart for state page
fnc_highchart_parole_barchart <- function(df, title_name, state_name, adm_or_pop){
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = df$metric) %>%
    hc_add_series(data = subset(df, metric == "Technical Violation"),
                  name = "Technical Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = tech_co,
                  accessibility = list(
                    enabled = TRUE,
                    keyboardNavigation = list(enabled = TRUE),
                    point = list(valueDescriptionFormat =
                                   "{point.state}, {point.year}, {point.metric}, {point.adm_or_pop}, {point.total:,.0f}"))
    ) %>%
    hc_add_series(data = subset(df, metric == "New Offense Violation"),
                  name = "New Offense Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = new_o_co,
                  accessibility = list(
                    enabled = TRUE,
                    keyboardNavigation = list(enabled = TRUE),
                    point = list(valueDescriptionFormat =
                                   "{point.state}, {point.year}, {point.metric}, {point.adm_or_pop}, {point.total:,.0f}"))
    ) %>%

    hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020, 2021)) %>%
    hc_yAxis(title = "", labels=list(format="{value:,.0f}")) %>%

    hc_title(text = title_name) %>%

    # hc_setup() %>%
    hc_add_theme(hc_theme_jc) %>%

    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%

    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = paste0("This is a bar chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to parole violations,
                                                               subset by technical violations and new offense violations."),
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = paste0("This is a bar chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to parole violations,
                                                               subset by technical violations and new offense violations.")))
    )
}

# probation highchart bar chart for state page
fnc_highchart_probation_barchart <- function(df, title_name, state_name, adm_or_pop){
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = df$metric) %>%
    hc_add_series(data = subset(df, metric == "Technical Violation"),
                  name = "Technical Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = tech_co,
                  accessibility = list(
                    enabled = TRUE,
                    keyboardNavigation = list(enabled = TRUE),
                    point = list(valueDescriptionFormat =
                                   "{point.state}, {point.year}, {point.metric}, {point.adm_or_pop}, {point.total:,.0f}"))
    ) %>%
    hc_add_series(data = subset(df, metric == "New Offense Violation"),
                  name = "New Offense Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = new_o_co,
                  accessibility = list(
                    enabled = TRUE,
                    keyboardNavigation = list(enabled = TRUE),
                    point = list(valueDescriptionFormat =
                                   "{point.state}, {point.year}, {point.metric}, {point.adm_or_pop}, {point.total:,.0f}"))
    ) %>%

    hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020, 2021)) %>%
    hc_yAxis(title = "", labels=list(format="{value:,.0f}")) %>%

    hc_title(text = title_name) %>%

    # hc_setup() %>%
    hc_add_theme(hc_theme_jc) %>%

    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%

    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = paste0("This is an area chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to probation violations,
                                                               subset by technical violations and new offense violations."),
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = paste0("This is an area chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to probation violations,
                                                               subset by technical violations and new offense violations.")))
    )
}

# highchart map for map explorer page
fnc_highchart_map <- function(df, map_filename, state_name, adm_or_pop){

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

      hc_chart(
        spacingTop = 1,
        spacingRight = 1,
        spacingBottom = 1,
        spacingLeft = 1) %>%

      hc_add_series_map(
        map = hex_gj,
        df = df,
        joinBy = "state_abb",
        value = "change",
        dataLabels = list(enabled = TRUE,
                          useHTML = TRUE,
                          formatter = JS("function() {return '<div style=\"text-align:center;\">' +
                            '<span style=\"font-weight:bold;\">' + this.point.state_abb + '</span><br>' +
                            '<span>' + this.point.changelabel + '</span>' +
                            '</div>';}"),
                          style = list(fontSize = "14px",
                                       fontWeight = "regular",
                                       align = "center",
                                       fontFamily = "Graphik",
                                       textOutline = 0)),
        nullColor = "#e8e8e8",
        accessibility = list(
          enabled = TRUE,
          keyboardNavigation = list(enabled = TRUE),
          point = list(valueDescriptionFormat =
                         "{point.state}, {point.metric}, {point.adm_or_pop}, Change from {point.year}, {point.value}%"))) %>%

      hc_colorAxis(min = NEW_MIN,
                   max = NEW_MAX,
                   stops = color_stops(7, c(darkblue,
                                            regblue,
                                            lightblue,
                                            white,
                                            lightorange,
                                            orange,
                                            darkorange)),
                   labels = list(format = "{value}%",
                                 style = list(fontSize = "14px"))) %>%

      hc_legend(align = "right",
                verticalAlign = "bottom",
                layout = "vertical",
                symbolHeight = 200,
                symbolWidth = 25,
                x = -25,
                y = 0) %>%

      hc_xAxis(title = "") %>%
      hc_yAxis(title = "") %>%
      hc_title(
        text = if (adm_or_pop == "admissions" & unique(df$metric) != "Total") {
          paste0("Change in Admissions to State Prison for ", unique(df$metric), "s")
        } else if(adm_or_pop == "population" & unique(df$metric) != "Total") {
          paste0("Change in State Prison Population for ", unique(df$metric), "s")
        } else if(adm_or_pop == "admissions" & unique(df$metric) == "Total") {
          "Change in Total Admissions to State Prison"
        } else if(adm_or_pop == "population" & unique(df$metric) == "Total") {
          "Change in Total Population in State Prison"
        }) %>%
      hc_subtitle(text = unique(df$year)) %>%

      hc_add_theme(hc_theme_map_jc) %>%

      hc_tooltip(
        formatter = JS("function() {
        return '<div style=\"background-color: #FFFFFF; opacity: 1; padding: 8px; border: none;\">' +
        this.point.tooltip +
        '</div>';}"),
        useHTML = TRUE
      ) %>%

      hc_plotOptions(series = list(animation = FALSE,
                                   cursor = "pointer",
                                   borderWidth = 3),
                     accessibility = list(enabled = TRUE,
                                          keyboardNavigation = list(enabled = TRUE),
                                          linkedDescription = paste0("This is an area chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to probation violations,
                                                               subset by technical violations and new offense violations."),
                                          landmarkVerbosity = "one"),
                     area = list(accessibility = list(description = paste0("This is an area chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to probation violations,
                                                               subset by technical violations and new offense violations.")))
      )

  } else {


    # Determine the new min and max where all values are negative
    NEW_MAX <- max_map
    NEW_MIN <- min_map

    highchart() %>%

      hc_chart(
        spacingTop = 1,
        spacingRight = 1,
        spacingBottom = 1,
        spacingLeft = 1) %>%

      hc_add_series_map(
        map = hex_gj,
        df = df,
        joinBy = "state_abb",
        value = "change",
        dataLabels = list(enabled = TRUE,
                          useHTML = TRUE,
                          formatter = JS("function() {return '<div style=\"text-align:center;\">' +
                            '<span style=\"font-weight:bold;\">' + this.point.state_abb + '</span><br>' +
                            '<span>' + this.point.changelabel + '</span>' +
                            '</div>';}"),
                          style = list(fontSize = "14px",
                                       fontWeight = "regular",
                                       align = "center",
                                       fontFamily = "Graphik",
                                       textOutline = 0)),
        nullColor = "#e8e8e8",
        accessibility = list(
          enabled = TRUE,
          keyboardNavigation = list(enabled = TRUE),
          point = list(valueDescriptionFormat =
                         "{point.state}, {point.metric}, {point.adm_or_pop}, Change from {point.year}, {point.value}%"))) %>%

      hc_colorAxis(min = NEW_MIN,
                   max = NEW_MAX,
                   stops = color_stops(4, c(darkblue, regblue, lightblue, white)),
                   labels = list(format = "{value}%",
                                 style = list(fontSize = "14px"))) %>%

      hc_legend(align = "right",
                verticalAlign = "bottom",
                layout = "vertical",
                symbolHeight = 200,
                symbolWidth = 25,
                x = -25,
                y = 0) %>%

      hc_xAxis(title = "") %>%
      hc_yAxis(title = "") %>%

      hc_title(
        text = if (adm_or_pop == "admissions" & unique(df$metric) != "Total") {
          paste0("Change in Admissions to State Prison for ", unique(df$metric), "s")
        } else if(adm_or_pop == "population" & unique(df$metric) != "Total") {
          paste0("Change in State Prison Population for ", unique(df$metric), "s")
        } else if(adm_or_pop == "admissions" & unique(df$metric) == "Total") {
          "Change in Total Admissions to State Prison"
        } else if(adm_or_pop == "population" & unique(df$metric) == "Total") {
          "Change in Total Population in State Prison"
        }) %>%
      hc_subtitle(text = unique(df$year)) %>%

      hc_add_theme(hc_theme_map_jc) %>%

      hc_tooltip(
        formatter = JS("function() {
        return '<div style=\"background-color: #FFFFFF; opacity: 1; padding: 8px; border: none;\">' +
        this.point.tooltip +
        '</div>';}"),
        useHTML = TRUE
      ) %>%

      hc_plotOptions(series = list(animation = FALSE,
                                   cursor = "pointer",
                                   borderWidth = 3),
                     accessibility = list(enabled = TRUE,
                                          keyboardNavigation = list(enabled = TRUE),
                                          linkedDescription = paste0("This is an area chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to probation violations,
                                                               subset by technical violations and new offense violations."),
                                          landmarkVerbosity = "one"),
                     area = list(accessibility = list(description = paste0("This is an area chart for the state of", state_name, "
                                                               displaying the total prison ", adm_or_pop, " due to probation violations,
                                                               subset by technical violations and new offense violations.")))
      )
  }

}

##########################
# Highcharts with logo
##########################

# Highchart area chart for state page WITH LOGO
fnc_highchart_state_areachart_logo <- function(df, title_name){

  subtitle_name <- df %>% filter(metric == "Supervision Violation") %>%
    select(probation_or_parole)
  subtitle_name <- unique(subtitle_name$probation_or_parole)

  highchart() %>%

    hc_chart(type="area",
             events = list(render = render_image),
             marginBottom = 80) %>%
    hc_add_series(data = subset(df, metric == "Total"),
                  name = "Total",
                  type = "area",
                  hcaes(x = year, y = total),
                  color = total_co,
                  dataLabels = list(enabled = TRUE,
                                    format='{point.total:,.0f}')) %>%
    hc_add_series(data = subset(df, metric == "Supervision Violation"),
                  name = "Supervision Violation",
                  type = "area",
                  hcaes(x = year, y = total),
                  color = viol_co,
                  dataLabels = list(enabled = TRUE,
                                    format='{point.total:,.0f}')) %>%
    hc_add_series(data = subset(df, metric == "Technical Violation"),
                  name = "Technical Violation",
                  type = "area",
                  hcaes(x = year, y = total),
                  color = tech_co,
                  dataLabels = list(enabled = TRUE,
                                    format='{point.total:,.0f}')) %>%
    hc_add_series(data = subset(df, metric == "New Offense Violation"),
                  name = "New Offense Violation",
                  type = "area",
                  hcaes(x = year, y = total),
                  color = new_o_co,
                  dataLabels = list(enabled = TRUE,
                                    format='{point.total:,.0f}')) %>%

    hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020, 2021)) %>%
    hc_yAxis(title = "", labels=list(format="{value:,.0f}")) %>%

    hc_title(text = title_name) %>%
    hc_subtitle(text = subtitle_name) %>%

    hc_add_theme(hc_theme_jc)
}

# Supervision violation highchart bar chart for state page WITH LOGO
fnc_highchart_state_barchart_logo <- function(df, title_name){

  subtitle_name <- df %>%
    filter(metric == "Technical Violation") %>%
    select(probation_or_parole)
  subtitle_name <- unique(subtitle_name$probation_or_parole)

  highchart() %>%
    hc_chart(type = "column",
             events = list(render = render_image),
             marginBottom = 80) %>%
    hc_xAxis(categories = df$metric) %>%
    hc_add_series(data = subset(df, metric == "Technical Violation"),
                  name = "Technical Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = tech_co,
                  dataLabels = list(enabled = TRUE,
                                    format='{point.total:,.0f}')) %>%
    hc_add_series(data = subset(df, metric == "New Offense Violation"),
                  name = "New Offense Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = new_o_co,
                  dataLabels = list(enabled = TRUE,
                                    format='{point.total:,.0f}')) %>%

    hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020, 2021)) %>%
    hc_yAxis(title = "", labels=list(format="{value:,.0f}")) %>%

    hc_title(text = title_name) %>%
    hc_subtitle(text = subtitle_name) %>%

    # hc_setup() %>%
    hc_add_theme(hc_theme_jc)
}

# Parole highchart bar chart for state page WITH logo
fnc_highchart_parole_barchart_logo <- function(df, title_name){
  highchart() %>%
    hc_chart(type = "column",
             events = list(render = render_image),
             marginBottom = 80) %>%
    hc_xAxis(categories = df$metric) %>%
    hc_add_series(data = subset(df, metric == "Technical Violation"),
                  name = "Technical Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = tech_co,
                  dataLabels = list(enabled = TRUE,
                                    format='{point.total:,.0f}')) %>%
    hc_add_series(data = subset(df, metric == "New Offense Violation"),
                  name = "New Offense Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = new_o_co,
                  dataLabels = list(enabled = TRUE,
                                    format='{point.total:,.0f}')) %>%

    hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020, 2021)) %>%
    hc_yAxis(title = "", labels=list(format="{value:,.0f}")) %>%

    hc_title(text = title_name) %>%

    # hc_setup() %>%
    hc_add_theme(hc_theme_jc)
}

# probation highchart bar chart for state page WITH logo
fnc_highchart_probation_barchart_logo <- function(df, title_name){
  highchart() %>%
    hc_chart(type = "column",
             events = list(render = render_image),
             marginBottom = 80) %>%
    hc_xAxis(categories = df$metric) %>%
    hc_add_series(data = subset(df, metric == "Technical Violation"),
                  name = "Technical Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = tech_co,
                  dataLabels = list(enabled = TRUE,
                                    format='{point.total:,.0f}')) %>%
    hc_add_series(data = subset(df, metric == "New Offense Violation"),
                  name = "New Offense Violation",
                  type = "column",
                  hcaes(x = year, y = total),
                  color = new_o_co,
                  dataLabels = list(enabled = TRUE,
                                    format='{point.total:,.0f}')) %>%

    hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020, 2021)) %>%
    hc_yAxis(title = "", labels=list(format="{value:,.0f}")) %>%

    hc_title(text = title_name) %>%

    # hc_setup() %>%
    hc_add_theme(hc_theme_jc)
}

# Map explorer WITH logo
fnc_highchart_map_logo <- function(df, map_filename, adm_or_pop){

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

      hc_chart(
        spacingTop = 1,
        spacingRight = 1,
        spacingBottom = 1,
        spacingLeft = 1) %>%

      hc_add_series_map(
        map = hex_gj,
        df = df,
        joinBy = "state_abb",
        value = "change",
        dataLabels = list(enabled = TRUE,
                          useHTML = TRUE,
                          formatter = JS("function() {return '<div style=\"text-align:center;\">' +
                            '<span style=\"font-weight:bold;\">' + this.point.state_abb + '</span><br>' +
                            '<span>' + this.point.changelabel + '</span>' +
                            '</div>';}"),
                          style = list(fontSize = "14px",
                                       fontWeight = "regular",
                                       align = "center",
                                       fontFamily = "Graphik",
                                       textOutline = 0)),
        nullColor = "#e8e8e8") %>%

      hc_colorAxis(min = NEW_MIN,
                   max = NEW_MAX,
                   stops = color_stops(7, c(darkblue,
                                            regblue,
                                            lightblue,
                                            white,
                                            lightorange,
                                            orange,
                                            darkorange)),
                   labels = list(format = "{value}%",
                                 style = list(fontSize = "14px"))) %>%

      hc_legend(align = "right",
                verticalAlign = "bottom",
                layout = "vertical",
                symbolHeight = 200,
                symbolWidth = 25,
                x = -25,
                y = 0) %>%

      hc_add_theme(hc_theme_map_jc) %>%
      hc_chart(events = list(render = render_image),
               marginBottom = 80) %>%

      hc_xAxis(title = "") %>%
      hc_yAxis(title = "") %>%
      hc_title(
        text = if (adm_or_pop == "admissions" & unique(df$metric) != "Total") {
          paste0("Change in Admissions to State Prison for ", unique(df$metric), "s")
        } else if(adm_or_pop == "population" & unique(df$metric) != "Total") {
          paste0("Change in State Prison Population for ", unique(df$metric), "s")
        } else if(adm_or_pop == "admissions" & unique(df$metric) == "Total") {
          "Change in Total Admissions to State Prison"
        } else if(adm_or_pop == "population" & unique(df$metric) == "Total") {
          "Change in Total Population in State Prison"
        }) %>%
      hc_subtitle(text = unique(df$year))

  } else {


    # Determine the new min and max where all values are negative
    NEW_MAX <- max_map
    NEW_MIN <- min_map

    highchart() %>%

      hc_chart(
        spacingTop = 1,
        spacingRight = 1,
        spacingBottom = 1,
        spacingLeft = 1) %>%

      hc_add_series_map(
        map = hex_gj,
        df = df,
        joinBy = "state_abb",
        value = "change",
        dataLabels = list(enabled = TRUE,
                          useHTML = TRUE,
                          formatter = JS("function() {return '<div style=\"text-align:center;\">' +
                            '<span style=\"font-weight:bold;\">' + this.point.state_abb + '</span><br>' +
                            '<span>' + this.point.changelabel + '</span>' +
                            '</div>';}"),
                          style = list(fontSize = "14px",
                                       fontWeight = "regular",
                                       align = "center",
                                       fontFamily = "Graphik",
                                       textOutline = 0)),
        nullColor = "#e8e8e8") %>%

      hc_colorAxis(min = NEW_MIN,
                   max = NEW_MAX,
                   stops = color_stops(4, c(darkblue, regblue, lightblue, white)),
                   labels = list(format = "{value}%",
                                 style = list(fontSize = "14px"))) %>%

      hc_legend(align = "right",
                verticalAlign = "bottom",
                layout = "vertical",
                symbolHeight = 200,
                symbolWidth = 25,
                x = -25,
                y = 0) %>%

      hc_add_theme(hc_theme_map_jc) %>%
      hc_chart(events = list(render = render_image),
               marginBottom = 80) %>%

      hc_xAxis(title = "") %>%
      hc_yAxis(title = "") %>%
      hc_title(
        text = if (adm_or_pop == "admissions" & unique(df$metric) != "Total") {
          paste0("Change in Admissions to State Prison for ", unique(df$metric), "s")
        } else if(adm_or_pop == "population" & unique(df$metric) != "Total") {
          paste0("Change in State Prison Population for ", unique(df$metric), "s")
        } else if(adm_or_pop == "admissions" & unique(df$metric) == "Total") {
          "Change in Total Admissions to State Prison"
        } else if(adm_or_pop == "population" & unique(df$metric) == "Total") {
          "Change in Total Population in State Prison"
        }) %>%
      hc_subtitle(text = unique(df$year))
  }

}

##########################
# Reactable tables
##########################

# Reactable table with 4 Year trend line in last column
fnc_reatable_table <- function(df){
  reactable(df,
            style = list(fontFamily = "Graphik, sans-serif"
                         #fontSize = "0.875rem"
            ),
            theme = reactableTheme(cellStyle = list(display = "flex",
                                                    flexDirection = "column",
                                                    justifyContent = "center")),
            defaultColDef = colDef(format = colFormat(separators = TRUE),
                                   align = "center"),
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
                                      format = colFormat(percent = TRUE,
                                                         digits = 1)),
              # add 4 Year trend graphs to each row
              total_new  = colDef(minWidth = 110,
                                  name = "4 Year Trend",
                                  cell = function(value, index) {
                                    dui_sparkline(
                                      data = value[[1]],
                                      height = 80,
                                      margin = list(top = 30,
                                                    right = 20,
                                                    bottom = 30,
                                                    left = 20),

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

