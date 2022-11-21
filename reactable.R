#######################################
# Project: MCLCShiny
# File: reactable.R
# Authors: Mari Roberts
# Date last updated: July 20, 2022
# Description:
#    Create and save reactable tables so the app loads faster
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
load(file = "app/data/state_table.Rda")
load(file = "app/data/state_table_wide.Rda")
load(file = "app/data/parole_table.Rda")
load(file = "app/data/parole_table_wide.Rda")
load(file = "app/data/probation_table.Rda")
load(file = "app/data/probation_table_wide.Rda")

# load packages
library(dplyr)
library(reactable)
library(stats)
library(purrr)

# assign colors for visualizations
source("app/colors.R")
source("app/library.R")
source("app/functions.R")

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

state_reactable_adm <- setNames(state_reactable_adm, c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))

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

state_reactable_pop <- setNames(state_reactable_pop, c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))

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

parole_reactable_adm <- setNames(parole_reactable_adm, c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))

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

parole_reactable_pop <- setNames(parole_reactable_pop, c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))

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

probation_reactable_adm <- setNames(probation_reactable_adm, c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))

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

probation_reactable_pop <- setNames(probation_reactable_pop, c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))

############
# Save data
############

# save to sharepoint
save(state_reactable_adm,     file=paste0(sp_data_path, "/Data/state_reactable_adm.Rda", sep = ""))
save(state_reactable_pop,     file=paste0(sp_data_path, "/Data/state_reactable_pop.Rda", sep = ""))
save(parole_reactable_adm,    file=paste0(sp_data_path, "/Data/parole_reactable_adm.Rda", sep = ""))
save(parole_reactable_pop,    file=paste0(sp_data_path, "/Data/parole_reactable_pop.Rda", sep = ""))
save(probation_reactable_adm, file=paste0(sp_data_path, "/Data/probation_reactable_adm.Rda", sep = ""))
save(probation_reactable_pop, file=paste0(sp_data_path, "/Data/probation_reactable_pop.Rda", sep = ""))

# save to clone
save(state_reactable_adm,     file="app/data/state_reactable_adm.Rda")
save(state_reactable_pop,     file="app/data/state_reactable_pop.Rda")
save(parole_reactable_adm,    file="app/data/parole_reactable_adm.Rda")
save(parole_reactable_pop,    file="app/data/parole_reactable_pop.Rda")
save(probation_reactable_adm, file="app/data/probation_reactable_adm.Rda")
save(probation_reactable_pop, file="app/data/probation_reactable_pop.Rda")
