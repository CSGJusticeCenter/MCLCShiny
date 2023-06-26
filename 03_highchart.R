#######################################
# Project: MCLCShiny
# File: highchart.R
# Authors: Mari Roberts, Martha Eichlersmith
# Date last updated: June 26, 2023 (MAR)
# Description:
#    Create and save highcharts so the app loads faster
#######################################

# path to data on research div sharepoint
# make sure sharepoint folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# in your Renviron (usethis::edit_r_environ()), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"



box::use(
  prep/box/admin
  , glue[glue]
)



obj_list <- c(
  "adm_pop_long.rds"
  , "mclc_explorer.rds"
  , "hex_gj.rds"
)

walk(obj_list, ~load(file = file.path(admin$sp_data, .x), envir = .GlobalEnv))



# assign colors for visualizations
source("app/colors.R")
source("00_fnc_library.R")



# list of states for function
states <- adm_pop_long$state %>%
  unique() %>%
  sort()

# list of metrics for function
metrics <- c("New Offense Violation",
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
  filename <- paste("Change_", x, "_Admissions_2018 - 2019", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map(df1, filename, x, "admissions")
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
  highcharts <- fnc_highchart_map(df1, filename, x, "admissions")
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
  highcharts <- fnc_highchart_map(df1, filename, x, "admissions")
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
  highcharts <- fnc_highchart_map(df1, filename, x, "admissions")
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
  highcharts <- fnc_highchart_map(df1, filename, x, "population")
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
  highcharts <- fnc_highchart_map(df1, filename, x, "population")
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
  highcharts <- fnc_highchart_map(df1, filename, x, "population")
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
  highcharts <- fnc_highchart_map(df1, filename, x, "population")
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
             (metric == "Total" |
                metric == "Supervision Violation" |
                metric == "New Offense Violation" |
                metric == "Technical Violation")) %>%
    group_by(state, year, metric, adm_or_pop, probation_or_parole) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    ungroup() %>%
    mutate(total = ifelse(total == 0, NA, total),
           tooltip = paste0("<b>", state, " - ", year, "</b><br>",
                            metric, " ",
                            adm_or_pop, "<br>",
                            formattable::comma(total, digits = 0), "<br>"))
  admin$mylog(glue("hc: Prison Admissions, {x}"))
  highcharts <- fnc_highchart_state_areachart(df1, "Prison Admissions", x, "admissions")
  return(highcharts)
})

all_state_area_adm <- setNames(all_state_area_adm, states)

# generate list of state highcharts to call in app (population)
all_state_area_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population" &
             (metric == "Total" |
                metric == "Supervision Violation" |
                metric == "New Offense Violation" |
                metric == "Technical Violation")) %>%
    group_by(state, year, metric, adm_or_pop, probation_or_parole) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    ungroup() %>%
    mutate(total = ifelse(total == 0, NA, total),
           tooltip = paste0("<b>", state, " - ", year, "</b><br>",
                            metric, " ",
                            adm_or_pop, "<br>",
                            formattable::comma(total, digits = 0), "<br>"))
  admin$mylog(glue("hc: Prison Population, {x}"))
  highcharts <- fnc_highchart_state_areachart(df1, "Prison Population", x, "population")
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
    group_by(state, year, metric, adm_or_pop, probation_or_parole) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    ungroup() %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total),
           tooltip = paste0("<b>", state, " - ", year, "</b><br>",
                            metric, " ",
                            adm_or_pop, "<br>",
                            formattable::comma(total, digits = 0), "<br>"))
  admin$mylog(glue("hc: Supervision Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart(df1, "Supervision Violation Admissions by Type", x, "admissions")
  return(highcharts)
})

# set names of charts
all_state_bar_adm <- setNames(all_state_bar_adm, states)

# generate list of state highcharts to call in app (population)
all_state_bar_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population") %>%
    group_by(state, year, metric, adm_or_pop, probation_or_parole) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    ungroup() %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total),
           tooltip = paste0("<b>", state, " - ", year, "</b><br>",
                            metric, " ",
                            adm_or_pop, "<br>",
                            formattable::comma(total, digits = 0), "<br>"))
  admin$mylog(glue("hc: Supervision Violation Population by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart(df1, "Supervision Violation Population by Type", x, "population")
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
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total),
           tooltip = paste0("<b>", state, " - ", year, "</b><br>",
                            metric, " ",
                            adm_or_pop, "<br>",
                            formattable::comma(total, digits = 0), "<br>"))
  admin$mylog(glue("hc: Parole Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_parole_barchart(df1, "Parole Violation Admissions by Type", x, "admissions")
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
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total),
           tooltip = paste0("<b>", state, " - ", year, "</b><br>",
                            metric, " ",
                            adm_or_pop, "<br>",
                            formattable::comma(total, digits = 0), "<br>"))
  admin$mylog(glue("hc: Parole Violations Population by Type, {x}"))
  highcharts <- fnc_highchart_parole_barchart(df1, "Parole Violation Population by Type", x, "population")
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
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total),
           tooltip = paste0("<b>", state, " - ", year, "</b><br>",
                            metric, " ",
                            adm_or_pop, "<br>",
                            formattable::comma(total, digits = 0), "<br>"))
  admin$mylog(glue("hc: Probation Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_probation_barchart(df1, "Probation Violation Admissions by Type", x, "admissions")
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
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total),
           tooltip = paste0("<b>", state, " - ", year, "</b><br>",
                            metric, " ",
                            adm_or_pop, "<br>",
                            formattable::comma(total, digits = 0), "<br>"))
  admin$mylog(glue("hc: Probation Violation Population by Type, {x}"))
  highcharts <- fnc_highchart_probation_barchart(df1, "Probation Violation Population by Type", x, "population")
  return(highcharts)
})

# set names of charts
probation_bar_pop <- setNames(probation_bar_pop, states)

############
# Save plots
############

data_map <- list(
  "Admissions" = list(
    "2018 - 2019" = adm_maps_2018_2019,
    "2018 - 2021" = adm_maps_2018_2021,
    "2019 - 2020" = adm_maps_2019_2020,
    "2020 - 2021" = adm_maps_2020_2021
  ),
  "Population" = list(
    "2018 - 2019" = pop_maps_2018_2019,
    "2018 - 2021" = pop_maps_2018_2021,
    "2019 - 2020" = pop_maps_2019_2020,
    "2020 - 2021" = pop_maps_2020_2021
  )
)


theseFOLDERS <- c("sharepoint" = admin$sp_data, "app" = "app/data")

for (folder in theseFOLDERS){

  # save(adm_maps_2018_2019,     file=file.path(folder, "adm_maps_2018_2019.rds"))
  # save(adm_maps_2018_2021,     file=file.path(folder, "adm_maps_2018_2021.rds"))
  # save(adm_maps_2019_2020,     file=file.path(folder, "adm_maps_2019_2020.rds"))
  # save(adm_maps_2020_2021,     file=file.path(folder, "adm_maps_2020_2021.rds"))
  #
  # save(pop_maps_2018_2019,     file=file.path(folder, "pop_maps_2018_2019.rds"))
  # save(pop_maps_2018_2021,     file=file.path(folder, "pop_maps_2018_2021.rds"))
  # save(pop_maps_2019_2020,     file=file.path(folder, "pop_maps_2019_2020.rds"))
  # save(pop_maps_2020_2021,     file=file.path(folder, "pop_maps_2020_2021.rds"))

  save(data_map,               file=file.path(folder, "data_map.rds"))

  save(all_state_area_adm,     file=file.path(folder, "all_state_area_adm.rds"))
  save(all_state_area_pop,     file=file.path(folder, "all_state_area_pop.rds"))
  save(all_state_bar_adm,      file=file.path(folder, "all_state_bar_adm.rds"))
  save(all_state_bar_pop,      file=file.path(folder, "all_state_bar_pop.rds"))

  save(parole_bar_adm,         file=file.path(folder, "parole_bar_adm.rds"))
  save(parole_bar_pop,         file=file.path(folder, "parole_bar_pop.rds"))
  save(probation_bar_adm,      file=file.path(folder, "probation_bar_adm.rds"))
  save(probation_bar_pop,      file=file.path(folder, "probation_bar_pop.rds"))

}
