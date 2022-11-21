#######################################
# Project: MCLCShiny
# File: import.R
# Authors: Mari Roberts
# Date last updated: October 25, 2022

# Description:
#    Loads packages
#    Imports data
#    Combines data by year
#    Cleans variable names
#    Creates data files for app

# Input:
#    "Data for web team v13.xlsx" Notes are here for now
#    "mclc_data_2022_v3.xlsx"     2022 survey data
#     Map files

# Output:
#     Data frames needed to run shiny app
#     Saves data to research SP folder
#######################################

# Install this version of highcharter???
# Remotes::install_github("batpigandme/highcharter@module-testing")
# Remotes::install_github("jbkunst/highcharter")
# install.packages("highcharter")

# Load packages
library(rlang)
library(dplyr)
library(tidyr)
library(csgjcr)
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

# Add fonts required to run functions.R
# Fonts are found in app folder
font_add("Graphik",      regular = "app/www/fonts/GraphikRegular.otf",
                         bold    = "app/www/fonts/GraphikBold.otf")
font_add("Graphik-Bold", regular = "app/www/fonts/GraphikBold.otf")

font_import(paths = "C:/Users/mroberts/AppData/Local/Microsoft/Windows/Fonts")
extrafont::loadfonts()
loadfonts(device="win")

showtext_auto()
default_fonts <- c("Graphik")

# ggplot(data.frame(x=1:5,y=1:5),aes(x,y))+
#   geom_point()+
#   geom_text(aes(label=y),nudge_x=0.5, family="Graphik",fontface = "bold", size = 10)+
#   theme_bw(base_family="Graphik")
# 
# ggplot(data.frame(x=1:5,y=1:5),aes(x,y))+
#   geom_point()+
#   geom_text(aes(label=y),nudge_x=0.5, family="Graphik-Bold", size = 10)+
#   theme_bw(base_family="Graphik-Bold")

# Load custom functions
source("app/functions.R")

# Path to data on research div sharepoint
# Make sure sharepoint folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# In your Renviron (usethis::edit_r_environ()), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"

FULL_JC_FOLDER <- FALSE

if (FULL_JC_FOLDER == TRUE){
  sp_data_path <- csgjcr::csg_sp_path(file.path("MCLC Shiny App"))
} else {
  sp_data_path <- csgjcr::csg_sp_path(file.path("JC Research - 50 State Revocations Project", "MCLC Shiny App"))
}

########
# Import data
########

# Load sp file
hex <- read_sf(file.path(paste0(sp_data_path, "/Data/us_states_hexgrid.geojson", sep = ""))) %>%
  select(state_abb = iso3166_2) %>%
  filter(state_abb != "DC")

# Load state abb
stateAbb <- read.csv(paste0(sp_data_path, "/Data/stateAbb.csv", sep = ""))

# Load admissions data
adm18 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2022_v3.xlsx", sep = ""), sheet = "Admissions 2018")
adm19 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2022_v3.xlsx", sep = ""), sheet = "Admissions 2019")
adm20 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2022_v3.xlsx", sep = ""), sheet = "Admissions 2020")
adm21 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2022_v3.xlsx", sep = ""), sheet = "Admissions 2021")

# Load population data
pop18 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2022_v3.xlsx", sep = ""), sheet = "Population 2018")
pop19 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2022_v3.xlsx", sep = ""), sheet = "Population 2019")
pop20 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2022_v3.xlsx", sep = ""), sheet = "Population 2020")
pop21 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2022_v3.xlsx", sep = ""), sheet = "Population 2021")

# Load states  - will change to new notes when ready ????????????????????????????????
notes <- read_excel(paste0(sp_data_path, "/Data/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Notes")

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
# Admissions and populations dataset
# Wide form of data
################################################################################

# clean notes file
notes <- clean_names(notes)

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
# rename variable
adm <- clean_names(adm)
pop <- clean_names(pop)

# add adm and pop data together
adm_pop <- merge(adm, pop, by = c("state", "year"))

# remove state abbrevs
# change data types
# calculate difference between total and supervision violations to get number of other
adm_pop <- adm_pop %>%
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
                     data == "new_offense_admissions"                      ~ "New Offense",
                     data == "technical_admissions"                        ~ "Technical Violation",
                     data == "other_admissions"                            ~ "Other",

                     data == "total_population"                            ~ "Total",
                     data == "total_violation_population"                  ~ "Supervision Violation",
                     data == "total_probation_violation_population"        ~ "Probation Violation",
                     data == "total_parole_violation_population"           ~ "Parole Violation",
                     data == "new_offense_population"                      ~ "New Offense",
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
mclc_explorer_table <- left_join(mclc_counts, mclc_change, by = c("state", "data")) %>%
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
  summarise(total_new = list(list(total)))

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
           metric == "New Offense") %>%
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
  mutate(text = case_when(metric == "New Offense" & adm_or_pop == "Admissions"            ~ "New Offense Admissions",
                          metric == "Supervision Violation" & adm_or_pop == "Admissions"  ~ "Supervision Violation Admissions",
                          metric == "Technical Violation" & adm_or_pop == "Admissions"    ~ "Technical Admissions",
                          metric == "Total" & adm_or_pop == "Admissions"                  ~ "Total Admissions",
                          metric == "New Offense" & adm_or_pop == "Population"            ~ "New Offense Population",
                          metric == "Supervision Violation" & adm_or_pop == "Population"  ~ "Supervision Violation Population",
                          metric == "Technical Violation" & adm_or_pop == "Population"    ~ "Technical Population",
                          metric == "Total" & adm_or_pop == "Population"                  ~ "Total Population")) %>%
  select(state, text, adm_or_pop, everything())

# make wide form
state_table_wide <- spread(state_table, key = year, value = total)

# order data for table output
state_table_wide <- state_table_wide %>%
  mutate(order = case_when(metric == "New Offense"             ~ 4,
                           metric == "Supervision Violation"   ~ 2,
                           metric == "Technical Violation"     ~ 3,
                           metric == "Total"                   ~ 1,

                           metric == "New Offense"             ~ 4,
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
    metric == "New Offense"             ~ 3,
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
    metric == "New Offense"             ~ 3,
    metric == "Technical Violation"     ~ 2,
    metric == "Probation Violation"        ~ 1)) %>%
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
adm_pop_long <- adm_pop_long %>%
  mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", formattable::comma(total, digits = 0), "<br>"))

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
# save Rdata
################################################################################

# save to SharePoint project folder
save(adm_pop_long,            file=paste0(sp_data_path, "/Data/adm_pop_long.Rda", sep = ""))
save(mclc_explorer,           file=paste0(sp_data_path, "/Data/mclc_explorer.Rda", sep = ""))
save(mclc_explorer_table,     file=paste0(sp_data_path, "/Data/mclc_explorer_table.Rda", sep = ""))
save(vb_adm_pop,              file=paste0(sp_data_path, "/Data/vb_adm_pop.Rda", sep = ""))
save(state_table,             file=paste0(sp_data_path, "/Data/state_table.Rda", sep = ""))
save(state_table_wide,        file=paste0(sp_data_path, "/Data/state_table_wide.Rda", sep = ""))
save(parole_table,            file=paste0(sp_data_path, "/Data/parole_table.Rda", sep = ""))
save(parole_table_wide,       file=paste0(sp_data_path, "/Data/parole_table_wide.Rda", sep = ""))
save(probation_table,         file=paste0(sp_data_path, "/Data/probation_table.Rda", sep = ""))
save(probation_table_wide,    file=paste0(sp_data_path, "/Data/probation_table_wide.Rda", sep = ""))
save(hex_gj,                  file=paste0(sp_data_path, "/Data/hex_gj.Rda", sep = ""))
save(notes,                   file=paste0(sp_data_path, "/Data/notes.Rda", sep = ""))
save(csg,                     file=paste0(sp_data_path, "/Data/csg.Rda", sep = ""))

# save to clone
save(adm_pop_long,            file=paste0("app/data/adm_pop_long.Rda", sep = ""))
save(mclc_explorer,           file=paste0("app/data/mclc_explorer.Rda", sep = ""))
save(mclc_explorer_table,     file=paste0("app/data/mclc_explorer_table.Rda", sep = ""))
save(vb_adm_pop,              file=paste0("app/data/vb_adm_pop.Rda", sep = ""))
save(state_table,             file=paste0("app/data/state_table.Rda", sep = ""))
save(state_table_wide,        file=paste0("app/data/state_table_wide.Rda", sep = ""))
save(parole_table,            file=paste0("app/data/parole_table.Rda", sep = ""))
save(parole_table_wide,       file=paste0("app/data/parole_table_wide.Rda", sep = ""))
save(probation_table,         file=paste0("app/data/probation_table.Rda", sep = ""))
save(probation_table_wide,    file=paste0("app/data/probation_table_wide.Rda", sep = ""))
save(hex_gj,                  file=paste0("app/data/hex_gj.Rda", sep = ""))
save(notes,                   file=paste0("app/data/notes.Rda", sep = ""))
save(csg,                     file=paste0("app/data/csg.Rda", sep = ""))
