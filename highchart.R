#######################################
# Project: MCLCShiny
# File: highchart.R
# Authors: Mari Roberts
# Date last updated: July 20, 2022
# Description:
#    Create and save highcharts so the app loads faster
#######################################

# path to data on research div sharepoint
# make sure sharepoint folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# in your Renviron (usethis::edit_r_environ()), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"

FULL_JC_FOLDER <- FALSE

if (FULL_JC_FOLDER == TRUE){
  sp_data_path <- csgjcr::csg_sp_path(file.path("MCLC Shiny App"))
} else {
  sp_data_path <- csgjcr::csg_sp_path(file.path("JC Research - 50 State Revocations Project", "MCLC Shiny App"))
}

# load data
load(file = "app/data/adm_pop_long.Rda")

# load packages
library(purrr)
library(dplyr)
library(highcharter)
library(scales)
library(stats)

# assign colors for visualizations
source("app/colors.R")

# get state list
states <- adm_pop_long$state %>%
  unique() %>%
  sort()

############
# MAP EXPLORER - Maps
############

df <- mclc_explorer %>%
  filter(adm_or_pop == "Admissions",
         metric     == "Total",
         year       == "2018 - 2019")

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
final_map <- if (min_map_type != max_map_type) {

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

    hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
              #padding = 10,
              symbolHeight = 200,
              symbolWidth = 25
    ) %>%

    hc_add_theme(hc_theme_map_jc) %>%
    hc_exporting(enabled = TRUE) %>%

    hc_xAxis(title = "") %>%
    hc_yAxis(title = "") %>%
    hc_title(
      text = paste0("Change in Title"),
      align = "center",
      style = list(fontWeight = "bold",
        fontFamily = "Graphik",
        fontSize = "30px",
        useHTML = TRUE)
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

    hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
              #padding = 10,
              symbolHeight = 200,
              symbolWidth = 25) %>%

    hc_add_theme(hc_theme_map_jc) %>%
    hc_exporting(enabled = TRUE) %>%


    hc_xAxis(title = "") %>%
    hc_yAxis(title = "") %>%
    hc_title(
      text = paste0("Change in Title"),
      align = "center",
      style = list(fontWeight = "bold",
        fontFamily = "Graphik",
        fontSize = "30px",
        useHTML = TRUE))
}

final_map

############
# STATE REPORTS - State area chart
############

# generate list of state highcharts to call in app (admissions)
all_state_area_adm <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Admissions" &
             (metric == "Total" | metric == "Supervision Violation" | metric == "New Offense" | metric == "Technical Violation")) %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total)) %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, digits = 0), "<br>"))
  highcharts <- fnc_highchart_state_areachart(df1)
  return(highcharts)
})

all_state_area_adm <- setNames(all_state_area_adm, states)

# generate list of state highcharts to call in app (population)
all_state_area_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population" &
             (metric == "Total" | metric == "Supervision Violation" | metric == "New Offense" | metric == "Technical Violation")) %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total)) %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, digits = 0), "<br>"))
  highcharts <- fnc_highchart_state_areachart(df1)
  return(highcharts)
})

# set names of charts
all_state_area_pop <- setNames(all_state_area_pop, states)

############
# STATE REPORTS - State bar chart
############

# generate list of state highcharts to call in app (admissions)
all_state_bar_adm <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Admissions") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total)) %>%
    filter(metric == "New Offense" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, digits = 0), "<br>"))
  highcharts <- fnc_highchart_state_barchart(df1)
  return(highcharts)
})

# set names of charts
all_state_bar_adm <- setNames(all_state_bar_adm, states)

# generate list of state highcharts to call in app (population)
all_state_bar_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total)) %>%
    filter(metric == "New Offense" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, digits = 0), "<br>"))
  highcharts <- fnc_highchart_state_barchart(df1)
  return(highcharts)
})

# set names of charts
all_state_bar_pop <- setNames(all_state_bar_pop, states)

############
# STATE REPORTS - Parole bar chart
############

# generate list of state highcharts to call in app (admissions)
parole_bar_adm <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
           adm_or_pop == "Admissions",
           prob_vs_parole == "Parole") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total)) %>%
    filter(metric == "New Offense" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, digits = 0), "<br>"))
  highcharts <- fnc_highchart_state_barchart(df1)
  return(highcharts)
})

# set names of charts
parole_bar_adm <- setNames(parole_bar_adm,states)

# generate list of state highcharts to call in app (population)
parole_bar_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
           adm_or_pop == "Population",
           prob_vs_parole == "Probation") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total)) %>%
    filter(metric == "New Offense" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, digits = 0), "<br>"))
  highcharts <- fnc_highchart_state_barchart(df1)
  return(highcharts)
})

# set names of charts
parole_bar_pop <- setNames(parole_bar_pop,states)

############
# STATE REPORTS - Probation bar chart
############

# generate list of state highcharts to call in app (admissions)
probation_bar_adm <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
           adm_or_pop == "Admissions",
           prob_vs_parole == "Probation") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total)) %>%
    filter(metric == "New Offense" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, digits = 0), "<br>"))
  highcharts <- fnc_highchart_state_barchart(df1)
  return(highcharts)
})

# set names of charts
probation_bar_adm <- setNames(probation_bar_adm, states)

# generate list of state highcharts to call in app (population)
probation_bar_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
           adm_or_pop == "Population",
           prob_vs_parole == "Probation") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total)) %>%
    filter(metric == "New Offense" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, digits = 0), "<br>"))
  highcharts <- fnc_highchart_state_barchart(df1)
  return(highcharts)
})

# set names of charts
probation_bar_pop <- setNames(probation_bar_pop, states)

############
# Save plots
############

# save to sharepoint
save(all_state_area_adm,     file=paste0(sp_data_path, "/Data/all_state_area_adm.Rda", sep = ""))
save(all_state_area_pop,     file=paste0(sp_data_path, "/Data/all_state_area_pop.Rda", sep = ""))
save(all_state_bar_adm,      file=paste0(sp_data_path, "/Data/all_state_bar_adm.Rda", sep = ""))
save(all_state_bar_pop,      file=paste0(sp_data_path, "/Data/all_state_bar_pop.Rda", sep = ""))

save(parole_bar_adm,         file=paste0(sp_data_path, "/Data/parole_bar_adm.Rda", sep = ""))
save(parole_bar_pop,         file=paste0(sp_data_path, "/Data/parole_bar_pop.Rda", sep = ""))
save(probation_bar_adm,      file=paste0(sp_data_path, "/Data/probation_bar_adm.Rda", sep = ""))
save(probation_bar_pop,      file=paste0(sp_data_path, "/Data/probation_bar_pop.Rda", sep = ""))

# save to clone
save(all_state_area_adm,     file="app/data/all_state_area_adm.Rda")
save(all_state_area_pop,     file="app/data/all_state_area_pop.Rda")
save(all_state_bar_adm,      file="app/data/all_state_bar_adm.Rda")
save(all_state_bar_pop,      file="app/data/all_state_bar_pop.Rda")

save(parole_bar_adm,         file="app/data/parole_bar_adm.Rda")
save(parole_bar_pop,         file="app/data/parole_bar_pop.Rda")
save(probation_bar_adm,      file="app/data/probation_bar_adm.Rda")
save(probation_bar_pop,      file="app/data/probation_bar_pop.Rda")

