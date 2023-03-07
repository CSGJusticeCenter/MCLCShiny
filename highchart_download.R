#######################################
# Project: MCLCShiny
# File: highchart_download.R
# Authors: Mari Roberts, Martha Eichlersmith
# Date last updated: March 7, 2023 (MAR)
# Description:
#    Create and save highcharts WITH LOGO (pngs) so the app loads faster
#######################################

# path to data on research div sharepoint
# make sure sharepoint folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# in your Renviron (usethis::edit_r_environ()), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"



box::use(
  prep/box/admin
  , glue[glue]
  , purrr[...]
  , dplyr[...]
  , highcharter[...]
  , htmlwidgets[saveWidget]
  , webshot2[webshot]
)

# load packages
library(purrr)
library(dplyr)
library(highcharter)
library(scales)
library(stats)

obj_list <- c(
  "adm_pop_long.Rda"
  , "mclc_explorer.Rda"
  , "hex_gj.Rda"
)

walk(obj_list, ~load(file = file.path(admin$sp_data, .x), envir = .GlobalEnv))



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

# list of metrics for function
metrics <- c("New Offense Violation",
             "Parole Violation",
             "Probation Violation",
             "Supervision Violation",
             "Technical Violation",
             "Total")

# metrics list for loop
metrics_list <- list("New Offense Violation",
                     "Parole Violation",
                     "Probation Violation",
                     "Supervision Violation",
                     "Technical Violation",
                     "Total")

################################################################################

# Save graphs as lists (with logo and data labels - need to fix area chart still)
# Will save as pngs near the end of this R file

################################################################################

############
# MAP EXPLORER - Maps
############

# generate list of state highcharts to call in app (admissions)
adm_maps_2018_2019 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2018 - 2019",
           metric     == x)
  filename <- paste("Change_", x, "_Admissions_2018 - 2019", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (admissions)
adm_maps_2018_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2018 - 2021",
           metric     == x)
  filename <- paste("Change_", x, "_Admissions_2018 - 2021", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (admissions)
adm_maps_2019_2020 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2019 - 2020",
           metric     == x)
  filename <- paste("Change_", x, "_Admissions_2019 - 2020", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (admissions)
adm_maps_2020_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2020 - 2021",
           metric     == x)
  filename <- paste("Change_", x, "_Admissions_2020 - 2021", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (Population)
pop_maps_2018_2019 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2018 - 2019",
           metric     == x)
  filename <- paste("Change_", x, "_Population_2018 - 2019", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (Population)
pop_maps_2018_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2018 - 2021",
           metric     == x)
  filename <- paste("Change_", x, "_Population_2018 - 2021", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (Population)
pop_maps_2019_2020 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2019 - 2020",
           metric     == x)
  filename <- paste("Change_", x, "_Population_2019 - 2020", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename)
  return(highcharts)
})

# generate list of state highcharts to call in app (Population)
pop_maps_2020_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2020 - 2021",
           metric     == x)
  filename <- paste("Change_", x, "_Population_2020 - 2021", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename)
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
             (metric == "Total" | metric == "Supervision Violation" | metric == "New Offense Violation" | metric == "Technical Violation")) %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total), .groups = "keep") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, big.mark = ",", digits = 0), "<br>"))
  admin$mylog(glue("hc: Prison Admissions, {x}"))
  highcharts <- fnc_highchart_state_areachart_logo(df1, "Prison Admissions")
  return(highcharts)
})

all_state_area_adm <- setNames(all_state_area_adm, states)

# generate list of state highcharts to call in app (population)
all_state_area_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population" &
             (metric == "Total" | metric == "Supervision Violation" | metric == "New Offense Violation" | metric == "Technical Violation")) %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total), .groups = "keep") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, big.mark = ",", digits = 0), "<br>"))
  admin$mylog(glue("hc: Prison Population, {x}"))
  highcharts <- fnc_highchart_state_areachart_logo(df1, "Prison Population")
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
    summarise(total = sum(total), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, big.mark = ",", digits = 0), "<br>"))
  admin$mylog(glue("hc: Supervision Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart_logo(df1, "Supervision Violation Admissions by Type")
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
    summarise(total = sum(total), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, big.mark = ",", digits = 0), "<br>"))
  admin$mylog(glue("hc: Supervision Violation Population by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart_logo(df1, "Supervision Violation Population by Type")
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
    summarise(total = sum(total), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, big.mark = ",", digits = 0), "<br>"))
  admin$mylog(glue("hc: Parole Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart_logo(df1, "Parole Violation Admissions by Type")
  return(highcharts)
})

# set names of charts
parole_bar_adm <- setNames(parole_bar_adm,states)

# generate list of state highcharts to call in app (population)
parole_bar_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population",
           prob_vs_parole == "Parole") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, big.mark = ",", digits = 0), "<br>"))
  admin$mylog(glue("hc: Parole Violations Population by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart_logo(df1, "Parole Violation Population by Type")
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
    summarise(total = sum(total), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, big.mark = ",", digits = 0), "<br>"))
  admin$mylog(glue("hc: Probation Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart_logo(df1, "Probation Violation Admissions by Type")
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
    summarise(total = sum(total), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, big.mark = ",", digits = 0), "<br>"))
  admin$mylog(glue("hc: Probation Violation Population by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart_logo(df1, "Probation Violation Population by Type")
  return(highcharts)
})

# set names of charts
probation_bar_pop <- setNames(probation_bar_pop, states)



################################################################################

# Save graphs as pngs (with logo and data labels - need to fix area chart still)

################################################################################

admin$mylog("!!START SAVING HIGHCHARTS AS PNGS")


## functions

add_st_name <- function(hc_obj, state = NA){

  org_title <- hc_obj$x$hc_opts$title$text
  thisstate <- ifelse(!is.na(state), state, hc_obj$x$hc_opts$series[[1]]$data[[1]]$state)

  new_title <- paste0(thisstate, " ", org_title)

  #admin$mylog(glue("Add state to title: {org_title} plot for {thisstate}"))

  hc_obj %>%
    hc_title(text = new_title)

}


save_state_png <- function(hc_obj, folderpath, id, title){

  admin$mylog(glue("Save plot: {title} for {id}"))

  temp <- tempfile(fileext = ".html")
  saveWidget(hc_obj, file = temp, selfcontained = TRUE)
  webshot(
    url = temp
    , file = file.path(folderpath, glue("{id}_{title}.png"))
    , zoom = 4
    , vwidth = 500
    , vheight = 500
    , delay = 1
  )


}


save_map_png <- function(hc_obj, folderpath, id, title){

  admin$mylog(glue("Save plot: {title} for {id}"))

  temp <- tempfile(fileext = ".html")
  saveWidget(hc_obj, file = temp, selfcontained = TRUE)
  webshot(
    url = temp
    , file = file.path(folderpath, glue("Change_{id}_{title}.png"))
    , delay = 1
  )


}

## folders

theseFOLDERS <- c("sharepoint" = file.path(admin$sp_data, "plots"), "app" = "app/data/plots")
savefolder <- theseFOLDERS[1]
copyfolder <- theseFOLDERS[2]


## remove pngs from sharepoint and app
for (folder in theseFOLDERS){

  walk(list.files(folder, pattern = "*.png"), ~file.remove(file.path(folder, .x)))

}


##########
# MAPS Admissions - loops are separate for now because of timeout issues
##########

admin$mylog("ADMISSIONS MAP 2018-2019")
# 2018-2019
walk(
  metrics_list,
  ~save_map_png(
    adm_maps_2018_2019[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2018 - 2019")
)

admin$mylog("ADMISSIONS MAP 2018-2021")
# 2018-2021
walk(
  metrics_list,
  ~save_map_png(
    adm_maps_2018_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2018 - 2021")
)


admin$mylog("ADMISSIONS MAP 2019-2020")
# 2019-2020
walk(
  metrics_list,
  ~save_map_png(
    adm_maps_2019_2020[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2019 - 2020")
)

admin$mylog("ADMISSIONS MAP 2020-2021")
# 2020-2021
walk(
  metrics_list,
  ~save_map_png(
    adm_maps_2020_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2020 - 2021")
)


##########
# MAPS POPULATION
##########

admin$mylog("POPULATIONS MAP 2018-2019")
# 2018-2019
walk(
  metrics_list,
  ~save_map_png(
    pop_maps_2018_2019[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2018 - 2019")
)

admin$mylog("POPULATIONS MAP 2018-2021")
# 2018-2021
walk(
  metrics_list,
  ~save_map_png(
    pop_maps_2018_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2018 - 2021")
)

admin$mylog("POPULATIONS MAP 2019-2020")
# 2019-2020
walk(
  metrics_list,
  ~save_map_png(
    pop_maps_2019_2020[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2019 - 2020")
)

admin$mylog("POPULATIONS MAP 2020-2021")
# 2020-2021
walk(
  metrics_list,
  ~save_map_png(
    pop_maps_2020_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2020 - 2021")
)


##########
# State Overview Area Chart
##########

admin$mylog("PRISON ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_area_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Prison_Admissions")
)


admin$mylog("PRISON POPULATION")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_area_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Prison_Population")
)



##########
# State Supervision Violation Bar Chart
##########

admin$mylog("SUPERVISION VIOLATION ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_bar_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Supervision_Violation_Admissions_by_Type")
)

admin$mylog("SUPERVISION VIOLATION POPULATION")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_bar_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Supervision_Violation_Population_by_Type")
)


##########
# Probation Bar Chart
##########


admin$mylog("PROBATION VIOLATION ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(probation_bar_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Probation_Violation_Admissions_by_Type")
)


admin$mylog("PROBATION VIOLATION POPULATIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(probation_bar_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Probation_Violation_Population_by_Type")
)



##########
# Parole Bar Chart
##########

admin$mylog("PAROLE VIOLATION ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(parole_bar_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Parole_Violation_Admissions_by_Type")
)

admin$mylog("PAROLE VIOLATION POPULATIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(parole_bar_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Parole_Violation_Population_by_Type")
)

#########################
## copy over pngs from sharepoint to app
walk(
  list.files(savefolder, pattern = "*.png")
  , ~file.copy(
    from = file.path(savefolder, .x)
    , to = file.path(copyfolder, .x)
    , overwrite = TRUE
  )
)

admin$mylog("!!END SAVING HIGHCHARTS AS PNGS")

