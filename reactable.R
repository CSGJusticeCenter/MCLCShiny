#######################################
# Project: MCLCShiny
# File: reactable.R
# Authors: Mari Roberts
# Date last updated: April 13, 2023 (MAR)
# Description:
#    Create and save reactable tables so the app loads faster
#######################################

# path to data on research div sharepoint
# make sure sharepoint folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# in your Renviron (usethis::edit_r_environ()), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"

box::use( prep/box/admin)

# load data
load(file = file.path(admin$sp_data, "state_table.rds"))
load(file = file.path(admin$sp_data, "state_table_wide.rds"))
load(file = file.path(admin$sp_data, "parole_table.rds"))
load(file = file.path(admin$sp_data, "parole_table_wide.rds"))
load(file = file.path(admin$sp_data, "probation_table.rds"))
load(file = file.path(admin$sp_data, "probation_table_wide.rds"))

# load packages
library(dplyr)
library(reactable)
library(stats)
library(purrr)
library(highcharter)
# install.packages("remotes")
# remotes::install_github("timelyportfolio/dataui")
library(dataui)

# assign colors for visualizations
source("app/colors.R")
source("app/functions.R")

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

# get state list
states <- state_table$state %>%
  unique() %>%
  sort()

############
# State overall table
############

# generate list of state reactables to call in app (admissions)
state_reactable_adm <- map(.x = states,  .f = function(x) {

    # filter data by state
    df <- state_table %>%
      filter(state == x & adm_or_pop == "Admissions") %>%
      group_by(text) %>%
      summarise(total_new = list(list(total)))
    df1 <- state_table_wide %>%
      filter(state == x & adm_or_pop == "Admissions") %>%
      arrange(order) %>%
      select(-adm_or_pop, -state)

    # merge data
    df2 <- merge(df1, df, by = "text")
    df2 <- df2 %>% arrange(order) %>% select(-order)

    # custom function to generate reactable table
    reactables <- fnc_reatable_table(df2)
    return(reactables)

})

state_reactable_adm <- setNames(state_reactable_adm, states_list)

# generate list of state reactables to call in app (population)
state_reactable_pop <- map(.x = states,  .f = function(x) {

  # filter data by state
  df <- state_table %>%
    filter(state == x & adm_or_pop == "Population") %>%
    group_by(text) %>%
    summarise(total_new = list(list(total)))
  df1 <- state_table_wide %>%
    filter(state == x & adm_or_pop == "Population") %>%
    arrange(order) %>%
    select(-adm_or_pop, -state)

  # merge data
  df2 <- merge(df1, df, by = "text")
  df2 <- df2 %>% arrange(order) %>% select(-order)

  # custom function to generate reactable table
  reactables <- fnc_reatable_table(df2)
  return(reactables)

})

state_reactable_pop <- setNames(state_reactable_pop, states_list)

############
# Parole table
############

# generate list of parole reactables to call in app (admissions)
parole_reactable_adm <- map(.x = states,  .f = function(x) {

  # filter data by state
  df <- parole_table %>%
    filter(state == x & adm_or_pop == "Admissions") %>%
    group_by(text) %>%
    summarise(total_new = list(list(total)))
  df1 <- parole_table_wide %>%
    filter(state == x & adm_or_pop == "Admissions") %>%
    arrange(order) %>%
    select(-adm_or_pop, -state)

  # merge data
  df2 <- merge(df1, df, by = "text")
  df2 <- df2 %>% arrange(order) %>% select(-c(state, adm_or_pop, metric, prob_vs_parole, order))

  # custom function to generate reactable table
  reactables <- fnc_reatable_table(df2)
  return(reactables)

})

parole_reactable_adm <- setNames(parole_reactable_adm, states_list)

# generate list of parole reactables to call in app (population)
parole_reactable_pop <- map(.x = states,  .f = function(x) {

  # filter data by state
  df <- parole_table %>%
    filter(state == x & adm_or_pop == "Population") %>%
    group_by(text) %>%
    summarise(total_new = list(list(total)))
  df1 <- parole_table_wide %>%
    filter(state == x & adm_or_pop == "Population") %>%
    arrange(order) %>%
    select(-adm_or_pop, -state)

  # merge data
  df2 <- merge(df1, df, by = "text")
  df2 <- df2 %>% arrange(order) %>% select(-c(state, adm_or_pop, metric, prob_vs_parole, order))

  # custom function to generate reactable table
  reactables <- fnc_reatable_table(df2)
  return(reactables)

})

parole_reactable_pop <- setNames(parole_reactable_pop, states_list)

############
# Probation table
############

# generate list of probation reactables to call in app (admissions)
probation_reactable_adm <- map(.x = states,  .f = function(x) {

  # filter data by state
  df <- probation_table %>%
    filter(state == x & adm_or_pop == "Admissions") %>%
    group_by(text) %>%
    summarise(total_new = list(list(total)))
  df1 <- probation_table_wide %>%
    filter(state == x & adm_or_pop == "Admissions") %>%
    arrange(order) %>%
    select(-adm_or_pop, -state)

  # merge data
  df2 <- merge(df1, df, by = "text")
  df2 <- df2 %>% arrange(order) %>% select(-c(state, adm_or_pop, metric, prob_vs_parole, order))

  # custom function to generate reactable table
  reactables <- fnc_reatable_table(df2)
  return(reactables)

})

probation_reactable_adm <- setNames(probation_reactable_adm, states_list)

# generate list of probation reactables to call in app (population)
probation_reactable_pop <- map(.x = states,  .f = function(x) {

  # filter data by state
  df <- probation_table %>%
    filter(state == x & adm_or_pop == "Population") %>%
    group_by(text) %>%
    summarise(total_new = list(list(total)))
  df1 <- probation_table_wide %>%
    filter(state == x & adm_or_pop == "Population") %>%
    arrange(order) %>%
    select(-adm_or_pop, -state)

  # merge data
  df2 <- merge(df1, df, by = "text")
  df2 <- df2 %>% arrange(order) %>% select(-c(state, adm_or_pop, metric, prob_vs_parole, order))

  # custom function to generate reactable table
  reactables <- fnc_reatable_table(df2)
  return(reactables)

})

probation_reactable_pop <- setNames(probation_reactable_pop, states_list)

############
# Save data
############

theseFOLDERS <- c( "sharepoint" = admin$sp_data, "app" = "app/data")

for (folder in theseFOLDERS){

  save(state_reactable_adm,     file=file.path(folder, "state_reactable_adm.rds"))
  save(state_reactable_pop,     file=file.path(folder, "state_reactable_pop.rds"))
  save(parole_reactable_adm,    file=file.path(folder, "parole_reactable_adm.rds"))
  save(parole_reactable_pop,    file=file.path(folder, "parole_reactable_pop.rds"))
  save(probation_reactable_adm, file=file.path(folder, "probation_reactable_adm.rds"))
  save(probation_reactable_pop, file=file.path(folder, "probation_reactable_pop.rds"))

}




