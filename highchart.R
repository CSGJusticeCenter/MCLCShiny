#######################################
# Project: MCLCShiny
# File: highchart.R
# Authors: Mari Roberts
# Date last updated: July 18, 2022
# Description:
#    Create and save highcharts so the app loads faster
#######################################

library(purrr)

# get state list
states <- adm_pop_long$state %>%
  unique() %>%
  sort()

############
# State area chart
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

all_state_area_adm <- setNames(all_state_area_adm, c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))

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
all_state_area_pop <- setNames(all_state_area_pop ,c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))

############
# State bar chart
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
all_state_bar_adm <- setNames(all_state_bar_adm,c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))

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
all_state_bar_pop <- setNames(all_state_bar_pop,c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))

############
# Save plots
############

# save to sharepoint
save(all_state_area_adm,     file=paste0(sp_data_path, "/Data/all_state_area_adm.Rda", sep = ""))
save(all_state_area_pop,     file=paste0(sp_data_path, "/Data/all_state_area_pop.Rda", sep = ""))
save(all_state_bar_adm,      file=paste0(sp_data_path, "/Data/all_state_bar_adm", sep = ""))
save(all_state_bar_pop,      file=paste0(sp_data_path, "/Data/all_state_bar_pop", sep = ""))

# save to clone
save(all_state_area_adm,     file="app/data/all_state_area_adm.Rda")
save(all_state_area_pop,     file="app/data/all_state_area_pop.Rda")
save(all_state_bar_adm,      file="app/data/all_state_bar_pop.Rda")
save(all_state_bar_pop,      file="app/data/all_state_bar_adm.Rda")
