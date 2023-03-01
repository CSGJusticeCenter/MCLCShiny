#######################################
# Project: MCLCShiny
# File: import.R
# Authors: Mari Roberts
# Sub-Author: Martha Eichlersmith
# Date last updated: February 28, 2023 (MAR)

# Description:
#    Loads packages
#    Imports data
#    Combines data by year
#    Cleans variable names
#    Creates data files for app

# Input:
#    "data/raw/notes/state_notes_overview.csv" state notes
#    "data/raw/mclc/mclc_data_2022_v4.xlsx"     2022 survey data
#     Map files

# Output:
#     Data frames needed to run shiny app
#     Saves data to research SP folder
#######################################

# Path to data on research div sharepoint
# Make sure sharepoint folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# In your Renviron (usethis::edit_r_environ()), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"

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

box::use( prep/box/admin)

# Load fonts
font_add("Graphik",     regular = "app/www/fonts/Graphik.ttf")
font_add("GraphikBold", regular = "app/www/fonts/GraphikBold.ttf")
extrafont::loadfonts(quiet = TRUE)
loadfonts(device="win")
showtext_auto()

# Load custom functions
source("app/functions.R")

########
# Import data
########

# Load sp file
hex <- read_sf(file.path(admin$sp_data_raw, "us_states_hexgrid.geojson")) %>%
  select(state_abb = iso3166_2) %>%
  filter(state_abb != "DC")

# Load state abb
stateAbb <- read.csv(file.path(admin$sp_data_raw, "stateAbb.csv"))

# Load admissions data
adm18 <- read_excel(file.path(admin$sp_data_raw, "mclc/mclc_data_2022_v4.xlsx"), sheet = "Admissions 2018")
adm19 <- read_excel(file.path(admin$sp_data_raw, "mclc/mclc_data_2022_v4.xlsx"), sheet = "Admissions 2019")
adm20 <- read_excel(file.path(admin$sp_data_raw, "mclc/mclc_data_2022_v4.xlsx"), sheet = "Admissions 2020")
adm21 <- read_excel(file.path(admin$sp_data_raw, "mclc/mclc_data_2022_v4.xlsx"), sheet = "Admissions 2021")

# Load population data
pop18 <- read_excel(file.path(admin$sp_data_raw, "mclc/mclc_data_2022_v4.xlsx"), sheet = "Population 2018")
pop19 <- read_excel(file.path(admin$sp_data_raw, "mclc/mclc_data_2022_v4.xlsx"), sheet = "Population 2019")
pop20 <- read_excel(file.path(admin$sp_data_raw, "mclc/mclc_data_2022_v4.xlsx"), sheet = "Population 2020")
pop21 <- read_excel(file.path(admin$sp_data_raw, "mclc/mclc_data_2022_v4.xlsx"), sheet = "Population 2021")

# Load states  - will change to new notes when ready
notes_raw <- read_csv(file.path(admin$sp_data_raw, "notes/state_notes_overview.csv"), show_col_types = FALSE)

# Load info on abolishment of parole or probation
abolish_prob_parole <- read_excel(file.path(admin$sp_survey, "MCLC 2022 Progress Tracking.xlsx"))

# Import BJS total admissions and population since these numbers are more reliable
bjs_pop <- read_excel(file.path(admin$sp_data_raw, "bjs/BJS - Prison Year-End Populations - 1978 to current.xlsx"))
bjs_adm <- read_excel(file.path(admin$sp_data_raw, "bjs/BJS - Prison Admissions & Releases - 1978 to current.xlsx"))

################################################################################
# Reformat shapefile for hex map
################################################################################

# Reformat hex data
hex_gj <- hex %>%
  st_transform(3857) %>%
  sf_geojson() %>%
  fromJSON(simplifyVector = FALSE)

# clean state abbreviations file
stateAbb <- clean_names(stateAbb)

################################################################################
# Reformat data about probation and parole being abolished
################################################################################

abolish_prob_parole <- abolish_prob_parole %>%
  clean_names() %>%
  select(state, abolished_probation, abolished_discretionary_parole) %>%
  distinct() %>%
  mutate(state = gsub('Excel', "", state),
         state = gsub('[()]', "", state),
         state = trimws(state)) %>%
  filter(abolished_discretionary_parole == "Yes")
abolish_prob_parole <- abolish_prob_parole$state

################################################################################
# Reformat notes file
################################################################################

# weird special space character in Word, retained when copied over
# In word, it's not a space, it's shown as the last character within a cell
# can't be removed with trimws default
# note that special_space_char == " " returns false
# when into csv and just manually removed spaces at end of note

notes <- notes_raw %>%
  group_by(state) %>%
  summarize(note_lst = list(notes)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    notes = paste(note_lst, collapse = "</p><p class = 'statetxt'>")
    , notes = paste0("<p class = 'statetxt'>", notes, "</p>")
  ) %>%
  ungroup() %>%
  select(state, notes)

################################################################################
# Extract total population and total admissions from BJS data
################################################################################

# BJS pop
bjs_pop <- bjs_pop %>%
  clean_names() %>%
  mutate(year = as.character(year)) %>%
  select(state,
         year,
         total_prison_population_bjs = total_population) %>%
  filter(year >= 2018 & year <= 2021)

# BJS adm
bjs_adm <- bjs_adm %>%
  clean_names() %>%
  mutate(year = as.character(year)) %>%
  select(state,
         year,
         total_prison_admissions_bjs = admissions_total) %>%
  filter(year >= 2018 & year <= 2021)

################################################################################
# Admissions and populations dataset
# Wide form of data
################################################################################

# add year variable
adm18$year <- "2018"
adm19$year <- "2019"
adm20$year <- "2020"
adm21$year <- "2021"

# add year variable
pop18$year <- "2018"
pop19$year <- "2019"
pop20$year <- "2020"
pop21$year <- "2021"

# add data together
adm <- rbind(adm18, adm19, adm20, adm21)
pop <- rbind(pop18, pop19, pop20, pop21)

# clean names
adm <- clean_names(adm)
pop <- clean_names(pop)

# add adm and pop data together
# remove state abbrevs
# change data types
# calculate difference between total and supervision violations to get number of other
# replace total admissions and population numbers with BJS numbers since these are more reliable
adm_pop <- adm %>%
  left_join(pop, by = c("state", "year")) %>%
  left_join(bjs_pop, by = c("state", "year")) %>%
  left_join(bjs_adm, by = c("state", "year")) %>%
  select(-c(total_prison_admissions, total_prison_population)) %>%
  select(state,
         year,
         total_prison_admissions = total_prison_admissions_bjs,
         total_prison_population = total_prison_population_bjs,
         everything()) %>%
  ungroup() %>%
  select(state, year, everything()) %>%
  select(-c(total_technical_violation_admissions,
            total_new_offense_admissions,
            total_technical_violation_population,
            total_new_offense_population)) %>%
  mutate(state = factor(state)) %>%
  mutate_if(is.character, as.numeric) %>%

  mutate(other_admissions = total_prison_admissions-total_supervision_violation_admissions,
         other_population = total_prison_population-total_supervision_violation_population) %>%

  select(state,
         year,
         total_admissions                            = total_prison_admissions,
         total_violation_admissions                  = total_supervision_violation_admissions,
         total_probation_violation_admissions        = probation_violation_admissions,
         new_offense_probation_violation_admissions,
         technical_probation_violation_admissions,
         total_parole_violation_admissions           = parole_violation_admissions,
         new_offense_parole_violation_admissions,
         technical_parole_violation_admissions,
         total_population                            = total_prison_population,
         total_violation_population                  = total_supervision_violation_population,
         total_probation_violation_population        = probation_violation_population,
         new_offense_probation_violation_population,
         technical_probation_violation_population,
         total_parole_violation_population           = parole_violation_population,
         new_offense_parole_violation_population,
         technical_parole_violation_population,
         other_admissions,
         other_population)

# replace all NaN with NA
adm_pop[adm_pop == "NaN"] <- NA

################################################################################
# MAP EXPLORER PAGE
# Value box data
################################################################################

# add prob and parole variables together to get total new offense and technical admissions and population
# remove prob and parole variables
mclc <- adm_pop %>%
  mutate(new_offense_admissions = new_offense_probation_violation_admissions + new_offense_parole_violation_admissions,
         new_offense_population = new_offense_probation_violation_population + new_offense_parole_violation_population,
         technical_admissions   = technical_probation_violation_admissions   + technical_parole_violation_admissions,
         technical_population   = technical_probation_violation_population   + technical_parole_violation_population) %>%
  select(-c(new_offense_probation_violation_admissions, new_offense_parole_violation_admissions,
            new_offense_probation_violation_population, new_offense_parole_violation_population,
            technical_probation_violation_admissions, technical_parole_violation_admissions,
            technical_probation_violation_population, technical_parole_violation_population))

# make long form
mclc_all <- gather(mclc, data, total, total_admissions:technical_population)

# create change from 2018 to 2019 to 2020
# remove dups
# create label ready variable called metric
# create pop vs adm variable
# change data types
# add state abbreviations
mclc_all <- mclc_all %>%
  ungroup() %>%
  arrange(state) %>%
  group_by(state, data) %>%
  mutate(change = total/lag(total) - 1) %>%
  distinct() %>%
  mutate(metric =
           case_when(data == "total_admissions"                            ~ "Total",
                     data == "total_violation_admissions"                  ~ "Supervision Violation",
                     data == "total_probation_violation_admissions"        ~ "Probation Violation",
                     data == "total_parole_violation_admissions"           ~ "Parole Violation",
                     data == "new_offense_admissions"                      ~ "New Offense Violation",
                     data == "technical_admissions"                        ~ "Technical Violation",
                     data == "other_admissions"                            ~ "Other",

                     data == "total_population"                            ~ "Total",
                     data == "total_violation_population"                  ~ "Supervision Violation",
                     data == "total_probation_violation_population"        ~ "Probation Violation",
                     data == "total_parole_violation_population"           ~ "Parole Violation",
                     data == "new_offense_population"                      ~ "New Offense Violation",
                     data == "technical_population"                        ~ "Technical Violation",
                     data == "other_population"                            ~ "Other"),
         adm_or_pop = ifelse(grepl("population", data), "Population", "Admissions"),
         data = paste0(metric, " " , adm_or_pop)) %>%
  mutate_if(is.character, as.factor) %>%
  left_join(stateAbb, by = "state") %>%
  select(state, year, total, everything())

# save labels
labels <- mclc_all %>% ungroup() %>% select(data, metric, adm_or_pop) %>% distinct()

# make data frame for counts
# data is in wide form
mclc_counts <- mclc_all %>%
  select(-change)
mclc_counts <- spread(mclc_counts, year, total) %>%
  select(state, data, `2018`, `2019`, `2020`, `2021`)

# make data frame for change
# data is in wide form
mclc_change <- mclc_all %>%
  filter(year != 2018) %>%
  select(-total)
mclc_change <-
  spread(mclc_change, year, change) %>%
  select(state,
         data,
         `2018 - 2019` = `2019`,
         `2019 - 2020` = `2020`,
         `2020 - 2021` = `2021`
  )

# combine counts and change tables together
mclc_explorer_table <-
  left_join(mclc_counts, mclc_change, by = c("state", "data")) %>%
  mutate(`2018 - 2021` = (`2021`-`2018`)/`2018`)

# Get 4 year change
mclc_explorer_table_4_yr <- mclc_explorer_table %>%
  select(state, data, change = `2018 - 2021`) %>%
  left_join(stateAbb, by = "state") %>%
  left_join(labels, by = "data") %>%
  rename(state_abb = code) %>%
  mutate(year = 2022) %>%
  select(-abbrev)

# save reactable version of map explorer table
mclc_explorer_table_long <- mclc_explorer_table %>%
  select(state, data, `2018`, `2019`, `2020`, `2021`) %>%
  pivot_longer(cols=c(`2018`, `2019`, `2020`, `2021`),
               names_to='year',
               values_to='total') %>%
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

mclc_explorer_table <- merge(mclc_explorer_table, mclc_explorer_table_long, by = c("state", "data"))

# create year range
# create min and max values for legend scale
mclc_explorer <- mclc_all %>%
  filter(year != 2018) %>%
  rename(state_abb = code) %>%
  select(-abbrev) %>%
  full_join(mclc_explorer_table_4_yr, by = c("state", "data", "year", "change", "state_abb", "metric", "adm_or_pop")) %>%
  mutate(year = case_when(year == 2019 ~ "2018 - 2019",
                          year == 2020 ~ "2019 - 2020",
                          year == 2021 ~ "2020 - 2021",
                          year == 2022 ~ "2018 - 2021"),
         change = round(change*100, 1),
         tooltip = paste0("<b>", state, "</b><br>","Change from ", year, "<br>",change, "%<br>"),
         datalabel = ifelse(is.na(change), paste0("", state_abb, ""),
                            paste0("<p style=", "text-align:center", ">", state_abb, "", "<br>",
                                   round(change, 0), "%</p>"))) %>%
  ungroup() %>%
  group_by(year, data) %>%
  mutate(min_map = round(min(change, na.rm = TRUE), 0),      # use -1 to round up to nearest tenth
         max_map = round(max(change, na.rm = TRUE), 0)) %>%
  mutate(# get absolute value for comparison
    min_map_abs = abs(min_map),
    max_map_abs = abs(max_map),
    min_map_type = ifelse(min_map >= 0, "positive", "negative"),
    max_map_type = ifelse(max_map >= 0, "positive", "negative"))

################################################################################
# STATE REPORTS PAGE
# Value box data
################################################################################

# filter to value box values (total, supervision violations, and technical violations)
# create increase or decrease category for change
# change data types
vb_adm_pop <- mclc_all %>%
  filter(metric == "Total" |
           metric == "Supervision Violation" |
           metric == "Technical Violation" |
           metric == "New Offense Violation") %>%
  mutate(change = round(change*100, 0),
         change_type = ifelse(change > 0, "increase", "decrease"),
         state = as.character(state),
         year = as.character(year),
         metric = as.character(metric),
         adm_or_pop = as.character(adm_or_pop))

##############################
# State table under graph
##############################

# select variables
# sum by type
# remove probation, parole and other
# create text for table
state_table <- mclc_all %>%
  select(state, year, data, total, metric, adm_or_pop) %>%
  group_by(state, year, metric, adm_or_pop) %>%
  summarise(total = sum(total)) %>%
  filter(metric != "Other" & metric != "Probation Violation" & metric != "Parole Violation") %>%
  mutate(text = case_when(metric == "New Offense Violation" & adm_or_pop == "Admissions"  ~ "New Offense Violation Admissions",
                          metric == "Supervision Violation" & adm_or_pop == "Admissions"  ~ "Supervision Violation Admissions",
                          metric == "Technical Violation" & adm_or_pop == "Admissions"    ~ "Technical Violation Admissions",
                          metric == "Total" & adm_or_pop == "Admissions"                  ~ "Total Admissions",
                          metric == "New Offense Violation" & adm_or_pop == "Population"  ~ "New Offense Violation Population",
                          metric == "Supervision Violation" & adm_or_pop == "Population"  ~ "Supervision Violation Population",
                          metric == "Technical Violation" & adm_or_pop == "Population"    ~ "Technical Population",
                          metric == "Total" & adm_or_pop == "Population"                  ~ "Total Population")) %>%
  select(state, text, adm_or_pop, everything())

# make wide form
state_table_wide <- spread(state_table, key = year, value = total)

# order data for table output
state_table_wide <- state_table_wide %>%
  mutate(order = case_when(metric == "New Offense Violation"   ~ 4,
                           metric == "Supervision Violation"   ~ 2,
                           metric == "Technical Violation"     ~ 3,
                           metric == "Total"                   ~ 1,

                           metric == "New Offense Violation"   ~ 4,
                           metric == "Supervision Violation"   ~ 2,
                           metric == "Technical Violation"     ~ 3,
                           metric == "Total"                   ~ 1),
         four_yr_change = (`2021`-`2018`)/`2018`) %>%
  select(state, text, `2018`, `2019`, `2020`, `2021`, four_yr_change, everything()) %>%
  ungroup() %>%
  select(-metric)

################################################################################
# Parole table under graph
################################################################################

# make data long form
prob_parole_tables <- gather(adm_pop, data, total, total_admissions:other_population)

# filter to prob and parole info only
prob_parole_tables <- prob_parole_tables %>%
  select(state, year, data, total) %>%
  filter(grepl("parole|probation", data)) %>%
  mutate(adm_or_pop     = ifelse(grepl("population", data), "Population", "Admissions"),
         prob_vs_parole = ifelse(grepl("probation", data),  "Probation", "Parole"))

# create metric and text for table
prob_parole_tables <- fnc_create_data_text(prob_parole_tables)
prob_parole_tables <- fnc_create_data_metric(prob_parole_tables) %>%
  group_by(state, year, metric, adm_or_pop, prob_vs_parole, text) %>%
  summarise(total = sum(total))

# filter to parole
parole_table <- prob_parole_tables %>%
  filter(prob_vs_parole == "Parole") %>%
  select(state, text, adm_or_pop, everything()) %>%
  select(-metric)

# make wide form
parole_table_wide <- spread(parole_table, key = year, value = total)

# order data for table output
parole_table_wide <- parole_table_wide %>%
  mutate(order = case_when(
    metric == "New Offense Violation"   ~ 3,
    metric == "Technical Violation"     ~ 2,
    metric == "Parole Violation"        ~ 1)) %>%
  mutate(four_yr_change = (`2021`-`2018`)/`2018`) %>%
  select(state, text, `2018`, `2019`, `2020`, `2021`, four_yr_change, everything()) %>%
  select(-metric)

################################################################################
# Probation table under graph
################################################################################

# filter to probation
probation_table <- prob_parole_tables %>%
  filter(prob_vs_parole == "Probation") %>%
  select(state, text, adm_or_pop, everything()) %>%
  select(-metric)

# make wide form
probation_table_wide <- spread(probation_table, key = year, value = total)

# order data for table output
probation_table_wide <- probation_table_wide %>%
  mutate(order = case_when(
    metric == "New Offense Violation"   ~ 3,
    metric == "Technical Violation"     ~ 2,
    metric == "Probation Violation"     ~ 1)) %>%
  # 3 year change
  mutate(four_yr_change = (`2021`-`2018`)/`2018`) %>%
  select(state, text, `2018`, `2019`, `2020`, `2021`, four_yr_change, everything()) %>%
  select(-metric)

################################################################################
# Download data tables (BJS vs CSG)
################################################################################

########
# CSG download data
########

adm_pop_long <- gather(adm_pop, data, total, total_admissions:other_population)

# custom function to add text label depending on metric
# custom function to create an adm vs pop variable
# custom function to create a prob vs parole variable
adm_pop_long <- fnc_create_data_metric(adm_pop_long)
adm_pop_long <- fnc_create_adm_pop(adm_pop_long)
adm_pop_long <- fnc_create_prob_vs_parole(adm_pop_long)

# add tooltip
# add info on probation and parole being abolished
adm_pop_long <- adm_pop_long %>%
  mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", formattable::comma(total, digits = 0), "<br>"))
# left_join(abolish_prob_parole, by = "state")

# create new df
csg <- adm_pop_long
csg <- fnc_create_data_text(csg)

# select data and change data types
csg <- csg %>% ungroup() %>%
  select(state,
         metric = text,
         year,
         total) %>%
  mutate(state = as.character(state),
         year = as.character(year))


################################################################################
# states that don't have graphs because of missing data
################################################################################

# states that are missing data and will not have a graph showing technical and new offense violations
nt_na_adm1 <- mclc_all %>%
  filter(data == "New Offense Violation Admissions" | data == "Technical Violation Admissions") %>%
  group_by(state, data) %>%
  summarise(total = sum(total, na.rm = TRUE)) %>%
  group_by(state) %>% filter(all(total == 0)) %>%
  select(state) %>% distinct()
nt_na_adm <- nt_na_adm1$state
nt_na_pop1 <- mclc_all %>%
  filter(data == "New Offense Violation Population" | data == "Technical Violation Population") %>%
  group_by(state, data) %>%
  summarise(total = sum(total, na.rm = TRUE)) %>%
  group_by(state) %>% filter(all(total == 0)) %>%
  select(state) %>% distinct()
nt_na_pop <- nt_na_pop1$state

# states that are NOT missing data and will have a graph showing technical and new offense violations
nt_not_na_adm <- mclc_all %>%   ungroup() %>% select(state) %>% distinct() %>%
  anti_join(nt_na_adm1, by = "state")
nt_not_na_adm <- nt_not_na_adm$state
nt_not_na_pop <- mclc_all %>%   ungroup() %>% select(state) %>% distinct() %>%
  anti_join(nt_na_pop1, by = "state")
nt_not_na_pop <- nt_not_na_pop$state

# states that are missing data and will not have a parole graph
parole_na_adm1 <- adm_pop_long %>%
  filter(data == "new_offense_parole_violation_admissions" | data == "technical_parole_violation_admissions") %>%
  mutate(state = as.character(state)) %>%
  group_by(state, data) %>%
  summarise(total = sum(total, na.rm = TRUE)) %>%
  group_by(state) %>%
  filter(all(total == 0)) %>%
  select(state) %>% distinct()
parole_na_adm <- parole_na_adm1$state
parole_na_pop1 <- adm_pop_long %>%
  filter(data == "new_offense_parole_violation_population" | data == "technical_parole_violation_population") %>%
  mutate(state = as.character(state)) %>%
  group_by(state, data) %>%
  summarise(total = sum(total, na.rm = TRUE)) %>%
  group_by(state) %>%
  filter(all(total == 0)) %>%
  select(state) %>% distinct()
parole_na_pop <- parole_na_pop1$state

# states that are NOT missing data and will have a graph showing technical and new offense parole violations
parole_not_na_adm <- adm_pop_long %>% mutate(state = as.character(state)) %>% ungroup() %>% select(state) %>% distinct() %>%
  anti_join(parole_na_adm1, by = "state")
parole_not_na_adm <- parole_not_na_adm$state
parole_not_na_pop <- adm_pop_long %>% mutate(state = as.character(state)) %>% ungroup() %>% select(state) %>% distinct() %>%
  anti_join(parole_na_pop1, by = "state")
parole_not_na_pop <- parole_not_na_pop$state

# states that are missing data and will not have a probation graph
probation_na_adm1 <- adm_pop_long %>%
  filter(data == "new_offense_probation_violation_admissions" | data == "technical_probation_violation_admissions") %>%
  mutate(state = as.character(state)) %>%
  group_by(state, data) %>%
  summarise(total = sum(total, na.rm = TRUE)) %>%
  group_by(state) %>%
  filter(all(total == 0)) %>%
  select(state) %>% distinct()
probation_na_adm <- probation_na_adm1$state
probation_na_pop1 <- adm_pop_long %>%
  filter(data == "new_offense_probation_violation_population" | data == "technical_probation_violation_population") %>%
  mutate(state = as.character(state)) %>%
  group_by(state, data) %>%
  summarise(total = sum(total, na.rm = TRUE)) %>%
  group_by(state) %>%
  filter(all(total == 0)) %>%
  select(state) %>% distinct()
probation_na_pop <- probation_na_pop1$state

# states that are NOT missing data and will have a graph showing technical and new offense probation violations
probation_not_na_adm <- adm_pop_long %>% mutate(state = as.character(state)) %>% ungroup() %>% select(state) %>% distinct() %>%
  anti_join(probation_na_adm1, by = "state")
probation_not_na_adm <- probation_not_na_adm$state
probation_not_na_pop <- adm_pop_long %>% mutate(state = as.character(state)) %>% ungroup() %>% select(state) %>% distinct() %>%
  anti_join(probation_na_pop1, by = "state")
probation_not_na_pop <- probation_not_na_pop$state

################################################################################
# save Rdata
################################################################################


theseFOLDERS <- c( "sharepoint" = admin$sp_data, "app"  = "app/data")

for (folder in theseFOLDERS){

  save(adm_pop_long,                file=file.path(folder, "adm_pop_long.Rda"))
  save(mclc_explorer,               file=file.path(folder, "mclc_explorer.Rda"))
  save(mclc_explorer_table,         file=file.path(folder, "mclc_explorer_table.Rda"))
  save(vb_adm_pop,                  file=file.path(folder, "vb_adm_pop.Rda"))
  save(state_table,                 file=file.path(folder, "state_table.Rda"))
  save(state_table_wide,            file=file.path(folder, "state_table_wide.Rda"))
  save(parole_table,                file=file.path(folder, "parole_table.Rda"))
  save(parole_table_wide,           file=file.path(folder, "parole_table_wide.Rda"))
  save(probation_table,             file=file.path(folder, "probation_table.Rda"))
  save(probation_table_wide,        file=file.path(folder, "probation_table_wide.Rda"))
  save(hex_gj,                      file=file.path(folder, "hex_gj.Rda"))
  save(notes,                       file=file.path(folder, "notes.Rda"))
  save(csg,                         file=file.path(folder, "csg.Rda"))

  save(nt_na_adm,                   file=file.path(folder, "nt_na_adm.Rda"))
  save(nt_na_pop,                   file=file.path(folder, "nt_na_pop.Rda"))
  save(nt_not_na_adm,               file=file.path(folder, "nt_not_na_adm.Rda"))
  save(nt_not_na_pop,               file=file.path(folder, "nt_not_na_pop.Rda"))
  save(abolish_prob_parole,         file=file.path(folder, "abolish_prob_parole.Rda"))
  save(parole_na_adm,               file=file.path(folder, "parole_na_adm.Rda"))
  save(parole_na_pop,               file=file.path(folder, "parole_na_pop.Rda"))
  save(parole_not_na_adm,           file=file.path(folder, "parole_not_na_adm.Rda"))
  save(parole_not_na_pop,           file=file.path(folder, "parole_not_na_pop.Rda"))
  save(probation_na_adm,            file=file.path(folder, "probation_na_adm.Rda"))
  save(probation_na_pop,            file=file.path(folder, "probation_na_pop.Rda"))
  save(probation_not_na_adm,        file=file.path(folder, "probation_not_na_adm.Rda"))
  save(probation_not_na_pop,        file=file.path(folder, "probation_not_na_pop.Rda"))

}
