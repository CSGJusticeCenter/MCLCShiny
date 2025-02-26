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




theseFOLDERS <- c(#"sharepoint" = admin$sp_data, 
                  "app" = "app/data")

for (folder in theseFOLDERS){

  save(adm_pop_maps,           file=file.path(folder, "adm_pop_maps.rds"))

  save(all_state_area_adm,     file=file.path(folder, "all_state_area_adm.rds"))
  save(all_state_area_pop,     file=file.path(folder, "all_state_area_pop.rds"))
  save(all_state_bar_adm,      file=file.path(folder, "all_state_bar_adm.rds"))
  save(all_state_bar_pop,      file=file.path(folder, "all_state_bar_pop.rds"))

  save(parole_bar_adm,         file=file.path(folder, "parole_bar_adm.rds"))
  save(parole_bar_pop,         file=file.path(folder, "parole_bar_pop.rds"))
  save(probation_bar_adm,      file=file.path(folder, "probation_bar_adm.rds"))
  save(probation_bar_pop,      file=file.path(folder, "probation_bar_pop.rds"))

}
