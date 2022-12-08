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
source("app/colors.R")
source("app/functions.R")

# list of states for function
states <- adm_pop_long$state %>%
  unique() %>%
  sort()

states_list <- list('Alabama',
                    'Alaska',
                    'Arizona',
                    'Arkansas',
                    'California',
                    'Colorado',
                    'Connecticut',
                    'Delaware',
                    'Florida',
                    'Georgia',
                    'Hawaii',
                    'Idaho',
                    'Illinois',
                    'Indiana',
                    'Iowa',
                    'Kansas',
                    'Kentucky',
                    'Louisiana',
                    'Maine',
                    'Maryland',
                    'Massachusetts',
                    'Michigan',
                    'Minnesota',
                    'Mississippi',
                    'Missouri',
                    'Montana',
                    'Nebraska',
                    'Nevada',
                    'New Hampshire',
                    'New Jersey',
                    'New Mexico',
                    'New York',
                    'North Carolina',
                    'North Dakota',
                    'Ohio',
                    'Oklahoma',
                    'Oregon',
                    'Pennsylvania',
                    'Rhode Island',
                    'South Carolina',
                    'South Dakota',
                    'Tennessee',
                    'Texas',
                    'Utah',
                    'Vermont',
                    'Virginia',
                    'Washington',
                    'West Virginia',
                    'Wisconsin',
                    'Wyoming')

# # temp list of states that isnt as large for testing
# states_list <- list('Alabama',
#                     'Connecticut',
#                     'Delaware')

# list of metrics for function
metrics <- c("New Offense",
             "Parole Violation",
             "Probation Violation",
             "Supervision Violation",
             "Technical Violation",
             "Total")

# metrics list for loop
metrics_list <- list("New Offense",
                     "Parole Violation",
                     "Probation Violation",
                     "Supervision Violation",
                     "Technical Violation",
                     "Total")

############
# MAP EXPLORER - Maps
############

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
  highcharts <- fnc_highchart_state_areachart(df1, "Prison Admissions")
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
  highcharts <- fnc_highchart_state_areachart(df1, "Prison Population")
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
  highcharts <- fnc_highchart_state_barchart(df1, "Supervision Violation Admissions by Type")
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
  highcharts <- fnc_highchart_state_barchart(df1, "Supervision Violation Population by Type")
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
  highcharts <- fnc_highchart_state_barchart(df1, "Parole Violation Admissions by Type")
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
  highcharts <- fnc_highchart_state_barchart(df1, "Parole Violation Population by Type")
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
  highcharts <- fnc_highchart_state_barchart(df1, "Probation Violation Admissions by Type")
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
  highcharts <- fnc_highchart_state_barchart(df1, "Probation Violation Population by Type")
  return(highcharts)
})

# set names of charts
probation_bar_pop <- setNames(probation_bar_pop, states)

############
# Save plots
############



theseFOLDERS <- c("sharepoint" = admin$sp_data, "app" = "app/data")

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

##########
# State Overview Area Chart
##########

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(all_state_area_adm[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Prison_Admissions.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(all_state_area_pop[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Prison_Population.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

##########
# State Supervision Violation Bar Chart
##########

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(all_state_bar_adm[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Supervision_Violation_Admissions_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(all_state_bar_pop[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Supervision_Violation_Population_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

##########
# Probation Bar Chart
##########

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(probation_bar_adm[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Probation_Violation_Admissions_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(probation_bar_pop[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Probation_Violation_Population_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

##########
# Parole Bar Chart
##########

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(parole_bar_adm[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Parole_Violation_Admissions_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(parole_bar_pop[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Parole_Violation_Population_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

##########
# MAPS Admissions - loops are separate for now because of timeout issues
##########

for (folder in theseFOLDERS){
  # 2018-2019
  for (i in metrics_list){
    htmlwidgets::saveWidget(adm_maps_2018_2019[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Admissions_2018 - 2019.png", sep = ""), delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2018-2021
  for (i in metrics_list){
    htmlwidgets::saveWidget(adm_maps_2018_2021[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Admissions_2018 - 2021.png", sep = ""), delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2019-2020
  for (i in metrics_list){
    htmlwidgets::saveWidget(adm_maps_2019_2020[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Admissions_2019 - 2020.png", sep = ""), delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2020-2021
  for (i in metrics_list){
    htmlwidgets::saveWidget(adm_maps_2020_2021[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Admissions_2020 - 2021.png", sep = ""), delay = 1)
  }
}

##########
# MAPS POPULATION
##########

for (folder in theseFOLDERS){
  # 2018-2019
  for (i in metrics_list){
    htmlwidgets::saveWidget(pop_maps_2018_2019[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Population_2018 - 2019.png", sep = ""),delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2018-2021
  for (i in metrics_list){
    htmlwidgets::saveWidget(pop_maps_2018_2021[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Population_2018 - 2021.png", sep = ""), delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2019-2020
  for (i in metrics_list){
    htmlwidgets::saveWidget(pop_maps_2019_2020[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Population_2019 - 2020.png", sep = ""), delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2020-2021
  for (i in metrics_list){
    htmlwidgets::saveWidget(pop_maps_2020_2021[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Population_2020 - 2021.png", sep = ""), delay = 1)
  }
  
}
