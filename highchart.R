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



box::use( prep/box/admin)

# load data
load(file = file.path(admin$sp_data, "adm_pop_long.Rda"))

# load packages
library(purrr)
library(dplyr)
library(highcharter)
library(scales)
library(stats)



# assign colors for visualizations
source("app/library.R")
source("app/colors.R")
source("app/functions.R")

# get state list
states <- adm_pop_long$state %>%
  unique() %>%
  sort()

############
# MAP EXPLORER - Maps
############

metrics <- c("New Offense",
             "Parole Violation",
             "Probation Violation",
             "Supervision Violation",
             "Technical Violation",
             "Total")

# generate list of state highcharts to call in app (admissions)
adm_maps_2018_2019 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2018 - 2019",
           metric     == x)
  filename <- paste("Change_in_", x, "Admissions_from_2018_2019")
  highcharts <- fnc_highchart_map(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (admissions)
adm_maps_2018_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2018 - 2021",
           metric     == x)
  filename <- paste("Change_in_", x, "Admissions_from_2018_2021")
  highcharts <- fnc_highchart_map(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (admissions)
adm_maps_2019_2020 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2019 - 2020",
           metric     == x)
  filename <- paste("Change_in_", x, "Admissions_from_2019_2020")
  highcharts <- fnc_highchart_map(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (admissions)
adm_maps_2020_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2020 - 2021",
           metric     == x)
  filename <- paste("Change_in_", x, "Admissions_from_2020_2021")
  highcharts <- fnc_highchart_map(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (Population)
pop_maps_2018_2019 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2018 - 2019",
           metric     == x)
  filename <- paste("Change_in_", x, "Population_from_2018_2019")
  highcharts <- fnc_highchart_map(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (Population)
pop_maps_2018_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2018 - 2021",
           metric     == x)
  filename <- paste("Change_in_", x, "Population_from_2018_2021")
  highcharts <- fnc_highchart_map(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (Population)
pop_maps_2019_2020 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2019 - 2020",
           metric     == x)
  filename <- paste("Change_in_", x, "Population_from_2019_2020")
  highcharts <- fnc_highchart_map(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (Population)
pop_maps_2020_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2020 - 2021",
           metric     == x)
  filename <- paste("Change_in_", x, "Population_from_2020_2021")
  highcharts <- fnc_highchart_map(df1, filename)
  return(highcharts)
})

adm_maps_2018_2019 <- setNames(adm_maps_2018_2019, metrics)
adm_maps_2018_2021 <- setNames(adm_maps_2018_2021, metrics)
adm_maps_2019_2020 <- setNames(adm_maps_2019_2020, metrics)
adm_maps_2020_2021 <- setNames(adm_maps_2020_2021, metrics)

pop_maps_2018_2019 <- setNames(pop_maps_2018_2019, metrics)
pop_maps_2018_2021 <- setNames(pop_maps_2018_2021, metrics)
pop_maps_2019_2020 <- setNames(pop_maps_2019_2020, metrics)
pop_maps_2020_2021 <- setNames(pop_maps_2020_2021, metrics)

# New Offense Admissions
saveWidget(adm_maps_2018_2019$`New Offense`, "temp.html")
webshot2::webshot(url = "temp.html", file= "app/data/Change_New_Offense_Adm_2018_2019.png", delay = 5)
saveWidget(adm_maps_2019_2020$`New Offense`, "temp.html")
webshot2::webshot(url = "temp.html", file= "app/data/Change_New_Offense_Adm_2019_2020.png", delay = 5)
saveWidget(adm_maps_2020_2021$`New Offense`, "temp.html")
webshot2::webshot(url = "temp.html", file= "app/data/Change_New_Offense_Adm_2020_2021.png", delay = 5)
saveWidget(adm_maps_2018_2021$`New Offense`, "temp.html")
webshot2::webshot(url = "temp.html", file= "app/data/Change_New_Offense_Adm_2018_2021.png", delay = 5)






























############
# STATE REPORTS - State area chart
############

# set options so that y axis has comma separator
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

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



theseFOLDERS <- c( "sharepoint" = admin$sp_data, "app" = "app/data")

for (folder in theseFOLDERS){

  save(all_state_area_adm,     file=file.path(folder, "all_state_area_adm.Rda"))
  save(all_state_area_pop,     file=file.path(folder, "all_state_area_pop.Rda"))
  save(all_state_bar_adm,      file=file.path(folder, "all_state_bar_adm.Rda"))
  save(all_state_bar_pop,      file=file.path(folder, "all_state_bar_pop.Rda"))

  save(parole_bar_adm,         file=file.path(folder, "parole_bar_adm.Rda"))
  save(parole_bar_pop,         file=file.path(folder, "parole_bar_pop.Rda"))
  save(probation_bar_adm,      file=file.path(folder, "probation_bar_adm.Rda"))
  save(probation_bar_pop,      file=file.path(folder, "probation_bar_pop.Rda"))

}







