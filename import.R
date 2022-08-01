#######################################
# Project: MCLCShiny
# File: import.R
# Authors: Mari Roberts
# Date last updated: June 10, 2022

# Description:
#    Loads packages
#    Imports data
#    Combines data by year
#    Cleans variable names
#    Creates data files for app

# Input:
#    "Data for web team v13.xlsx"
#     Map files

# Output:
#     Data frames needed to run shiny app
#     Saves data to research SP folder
#######################################

# load packages
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
library(janitor)

# Add fonts required to run functions.R
font_add("Graphik", regular = "app/www/Fonts/GraphikRegular.otf")
showtext_auto()
default_fonts <- c("Graphik")

# load custom functions
source("app/functions.R")

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

########
# Import data
########

# load sp file
hex <- read_sf(file.path(paste0(sp_data_path, "/Data/us_states_hexgrid.geojson", sep = ""))) %>%
  select(state_abb = iso3166_2)

# load state abb
stateAbb <- read.csv(paste0(sp_data_path, "/Data/stateAbb.csv", sep = ""))

# load admissions data
adm18 <- read_excel(paste0(sp_data_path, "/Data/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Admissions 2018")
adm19 <- read_excel(paste0(sp_data_path, "/Data/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Admissions 2019")
adm20 <- read_excel(paste0(sp_data_path, "/Data/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Admissions 2020")

# load population data
pop18 <- read_excel(paste0(sp_data_path, "/Data/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Population 2018")
pop19 <- read_excel(paste0(sp_data_path, "/Data/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Population 2019")
pop20 <- read_excel(paste0(sp_data_path, "/Data/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Population 2020")

# load states notes
notes <- read_excel(paste0(sp_data_path, "/Data/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Notes")

# load bjs parole and probation survey data (2010-2018)
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Parole_2010/ICPSR_34382/DS0001/34382-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Parole_2011/ICPSR_34718/DS0001/34718-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Parole_2012/ICPSR_35257/DS0001/35257-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Parole_2013/ICPSR_35629/DS0001/35629-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Parole_2014/ICPSR_36320/DS0001/36320-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Parole_2015/ICPSR_36619/DS0001/36619-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Parole_2016/ICPSR_37441/DS0001/37441-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Parole_2017/ICPSR_37471/DS0001/37471-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Parole_2018/ICPSR_38058/DS0001/38058-0001-Data.rda"))

load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Probation_2010/ICPSR_34321/DS0001/34321-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Probation_2011/ICPSR_34717/DS0001/34717-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Probation_2012/ICPSR_35256/DS0001/35256-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Probation_2013/ICPSR_35631/DS0001/35631-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Probation_2014/ICPSR_36343/DS0001/36343-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Probation_2015/ICPSR_36618/DS0001/36618-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Probation_2016/ICPSR_37459/DS0001/37459-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Probation_2017/ICPSR_37482/DS0001/37482-0001-Data.rda"))
load(file = paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Series (2010-2018)/BJS_Probation_2018/ICPSR_38057/DS0001/38057-0001-Data.rda"))

# rename rda tables
bjs_parole_2010.rda <- da34382.0001
bjs_parole_2011.rda <- da34718.0001
bjs_parole_2012.rda <- da35257.0001
bjs_parole_2013.rda <- da35629.0001
bjs_parole_2014.rda <- da36320.0001
bjs_parole_2015.rda <- da36619.0001
bjs_parole_2016.rda <- da37441.0001
bjs_parole_2017.rda <- da37471.0001
bjs_parole_2018.rda <- da38058.0001
bjs_probation_2010.rda <- da34321.0001
bjs_probation_2011.rda <- da34717.0001
bjs_probation_2012.rda <- da35256.0001
bjs_probation_2013.rda <- da35631.0001
bjs_probation_2014.rda <- da36343.0001
bjs_probation_2015.rda <- da36618.0001
bjs_probation_2016.rda <- da37459.0001
bjs_probation_2017.rda <- da38057.0001
bjs_probation_2018.rda <- da38057.0001

# load bjs parole and probation exits and entries data (2015-2020)
bjs_parole_exits_2015.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/parole_exits_15.csv", sep = ""))
bjs_parole_exits_2016.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/parole_exits_16.csv", sep = ""))
bjs_parole_exits_2017.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/parole_exits_17.csv", sep = ""))
bjs_parole_exits_2018.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/parole_exits_18.csv", sep = ""))
bjs_parole_exits_2019.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/parole_exits_19.csv", sep = ""))
bjs_parole_exits_2020.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/parole_exits_20.csv", sep = ""))
bjs_probation_exits_2015.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/prob_exits_15.csv", sep = ""))
bjs_probation_exits_2016.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/prob_exits_16.csv", sep = ""))
bjs_probation_exits_2017.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/prob_exits_17.csv", sep = ""))
bjs_probation_exits_2018.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/prob_exits_18.csv", sep = ""))
bjs_probation_exits_2019.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/prob_exits_19.csv", sep = ""))
bjs_probation_exits_2020.csv <- read.csv(paste0(sp_data_path, "/Data/BJS Annual Probation and Parole Entries and Exits (2015-2020)/prob_exits_20.csv", sep = ""))

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

# add year variable
pop18$year <- "2018"
pop19$year <- "2019"
pop20$year <- "2020"

# add data together
adm <- rbind(adm18, adm19, adm20)
pop <- rbind(pop18, pop19, pop20)

# clean names
adm <- clean_names(adm)
pop <- clean_names(pop)

# rename variable
adm <- adm %>% rename(state = states)
pop <- pop %>% rename(state = states)

# add adm and pop data together
adm_pop <- merge(adm, pop, by = c("state", "state_abbrev", "year"))

# remove state abbrevs
# change data types
# calculate difference between total and supervision violations to get number of other
adm_pop <- adm_pop %>%
  select(-state_abbrev) %>%
  mutate(state = factor(state)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(other_admissions = total_admissions-total_violation_admissions,
         other_population = total_population-total_violation_population)

# replace all NaN with NA
adm_pop[adm_pop == "NaN"] <- NA

################################################################################
# MAP EXPLORER PAGE
# Value box data
################################################################################

# make data long form
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
adm_pop_long <- gather(adm_pop, data, total, total_admissions:other_population)

# add text depending on data
adm_pop_long <- fnc_create_data_metric(adm_pop_long)
adm_pop_long <- fnc_create_adm_pop(adm_pop_long)
adm_pop_long <- fnc_create_prob_vs_parole(adm_pop_long)

# add tooltip
adm_pop_long <- adm_pop_long %>%
  mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>", metric, " ", adm_or_pop, "<br>", comma(total, digits = 0), "<br>"))


# create change from 2018 to 2019 to 2020
# remove dups
# create label ready variable called metric
# create pop vs adm variable
# change data types
# add state abbreviations
mclc_all <- mclc_all %>%
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

# make data frame for counts
# data is in wide form
mclc_counts <- mclc_all %>%
  select(-change)
mclc_counts <- spread(mclc_counts, year, total) %>%
  select(state, data, `2018`, `2019`, `2020`)

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
         `2019 - 2020` = `2020`)

# combine counts and change tables together
mclc_explorer_table <- left_join(mclc_counts, mclc_change, by = c("state", "data"))

# create year range
# create min and max values
mclc_explorer <- mclc_all %>%
  filter(year != 2018) %>%
  rename(state_abb = code) %>%
  mutate(year = case_when(year == 2019 ~ "2018 - 2019",
                          year == 2020 ~ "2019 - 2020"),
         change = round(change*100, 2),
         tooltip = paste0("<b>", state, "</b><br>","Change from ", year, "<br>",change, "%<br>"),
         datalabel = ifelse(is.na(change), paste0("", state_abb, ""),
                            paste0("<p style=", "text-align:center", ">", state_abb, "", "<br>",
                                   round(change, 0), "%</p>"))) %>%
  group_by(year, data) %>%
  mutate(min_map = round(min(change, na.rm = TRUE), -1),
         max_map = round(max(change, na.rm = TRUE), -1),
         # get absolute value for comparison
         min_map_abs = abs(min_map),
         max_map_abs = abs(max_map),
         min_map_type = ifelse(min_map >= 0, "positive", "negative"),
         max_map_type = ifelse(max_map >= 0, "positive", "negative"))

################################################################################
# STATE REPORTS PAGE
# Value box data
################################################################################

# filter to vb values (total, supervision violations, and technical violations)
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
# summarise by type
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
                          metric == "Total" & adm_or_pop == "Population"                  ~ "Total Population"))

# rearrange data
state_table <- state_table %>% select(state, text, adm_or_pop, everything())

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
         three_yr_change = (`2020`-`2018`)/`2018`)

# rearrange data
state_table_wide <- state_table_wide %>% select(state, text, `2018`, `2019`, `2020`, three_yr_change, everything()) %>%
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
parole_table <- prob_parole_tables %>% filter(prob_vs_parole == "Parole")

# make wide form
parole_table_wide <- spread(parole_table, key = year, value = total)

# order data for table output
parole_table_wide <- parole_table_wide %>%
  mutate(order = case_when(
    metric == "New Offense"             ~ 3,
    metric == "Technical Violation"     ~ 2,
    metric == "Parole Violation"        ~ 1)) %>%
  # 3 year change
  mutate(three_yr_change = (`2020`-`2018`)/`2018`)

# rearrange data
parole_table <- parole_table %>% select(state, text, adm_or_pop, everything()) %>%
  select(-metric)

# rearrange data
parole_table_wide <- parole_table_wide %>% select(state, text, `2018`, `2019`, `2020`, three_yr_change, everything()) %>%
  select(-metric)

################################################################################
# Probation table under graph
################################################################################

# filter to probation
probation_table <- prob_parole_tables %>% filter(prob_vs_parole == "Probation")

# make wide form
probation_table_wide <- spread(probation_table, key = year, value = total)

# order data for table output
probation_table_wide <- probation_table_wide %>%
  mutate(order = case_when(
    metric == "New Offense"             ~ 3,
    metric == "Technical Violation"     ~ 2,
    metric == "Probation Violation"        ~ 1)) %>%
  # 3 year change
  mutate(three_yr_change = (`2020`-`2018`)/`2018`)

# rearrange data
probation_table <- probation_table %>% select(state, text, adm_or_pop, everything()) %>%
  select(-metric)

# rearrange data
probation_table_wide <- probation_table_wide %>% select(state, text, `2018`, `2019`, `2020`, three_yr_change, everything()) %>%
  select(-metric)

################################################################################
# Download data tables (BJS vs CSG)
################################################################################

########
# CSG download data
########

# create new df
csg <- adm_pop_long
csg <- fnc_create_data_text(csg)

# select data and change data types
csg <- csg %>% ungroup() %>%
  select(state, year, text, total, adm_or_pop) %>%
  mutate(state = as.character(state))

########
# BJS download data
########

# create year variable and select variables
bjs_parole_2013 <- bjs_parole_2013.rda %>% clean_names() %>% mutate(year = 2013)
bjs_parole_2014 <- bjs_parole_2014.rda %>% clean_names() %>% mutate(year = 2014)
bjs_parole_2015 <- bjs_parole_2015.rda %>% clean_names() %>% mutate(year = 2015)
bjs_parole_2016 <- bjs_parole_2016.rda %>% clean_names() %>% mutate(year = 2016)
bjs_parole_2017 <- bjs_parole_2017.rda %>% clean_names() %>% mutate(year = 2017)
bjs_parole_2018 <- bjs_parole_2018.rda %>% clean_names() %>% mutate(year = 2018)

# create year variable and select variables
bjs_probation_2013 <- bjs_probation_2013.rda %>% clean_names() %>% mutate(year = 2013)
bjs_probation_2014 <- bjs_probation_2014.rda %>% clean_names() %>% mutate(year = 2014)
bjs_probation_2015 <- bjs_probation_2015.rda %>% clean_names() %>% mutate(year = 2015)
bjs_probation_2016 <- bjs_probation_2016.rda %>% clean_names() %>% mutate(year = 2016)
bjs_probation_2017 <- bjs_probation_2017.rda %>% clean_names() %>% mutate(year = 2017)
bjs_probation_2018 <- bjs_probation_2018.rda %>% clean_names() %>% mutate(year = 2018)

# add data together
bjs_parole <- rbind(bjs_parole_2013, bjs_parole_2014, bjs_parole_2015, bjs_parole_2016, bjs_parole_2017, bjs_parole_2018)
bjs_probation <- rbind(bjs_probation_2013, bjs_probation_2014, bjs_probation_2015, bjs_probation_2016, bjs_probation_2017, bjs_probation_2018)

# rename variables
# create metric description
bjs_parole <- bjs_parole %>% select(stateid, year,
                                    total_parole_end = totend,       # total parole population end of year
                                    total_entries_to_parole = toten, # total entries to parole
                                    inc_new_sentence = exincnew,     # incarcerated with a new sentence
                                    inc_revocation = exincrev        # incarcerated with a revocation (no new sentence)
                                    )
  # mutate(text = case_when(data == "total_parole_end"        ~ "Total Parole Population (End of Year)",
  #                         data == "total_entries_to_parole" ~ "Total Entries to Parole",
  #                         data == "inc_new_sentence"        ~ "Entries with New Sentence",
  #                         data == "inc_revocation"          ~ "Entries with Revocation"
  #                         ))

# rename variables
# create metric description
bjs_probation <- bjs_probation %>% select(stateid, year,
                                          total_prob_end = totend,         # prob population end of year
                                          entries_w_inc = eninc,           # entries with incarceration
                                          entries_wo_inc = ennoinc,        # entries without incarceration
                                          entries_total = toten,           # total entries to prob
                                          inc_new_sentence = exincnew,     # incarceration with new sentence
                                          inc_current_sentence = exincurr  # incarceration with current sentence
                                          )
  # mutate(text = case_when(data == "total_pop_end"         ~ "Total Probation Population (End of Year)",
  #                         data == "entries_w_inc"         ~ "Entries with Incarceration",
  #                         data == "entries_wo_inc"        ~ "Entries without Incarceration",
  #                         data == "entries_total"         ~ "Total Entries to Probation",
  #                         data == "inc_new_sentence"      ~ "Incarcerated with New Sentence",
  #                         data == "inc_current_sentence"  ~ "Incarcerated under Current Sentence"))

# remove punctuation and numbers from state name
bjs_parole$stateid <- gsub('[[:punct:]]+','',bjs_parole$stateid)
bjs_parole$stateid <- gsub('[[:digit:]]+', '', bjs_parole$stateid)
bjs_probation$stateid <- gsub('[[:punct:]]+','',bjs_probation$stateid)
bjs_probation$stateid <- gsub('[[:digit:]]+', '', bjs_probation$stateid)
bjs_probation$stateid <- trimws(bjs_probation$stateid, whitespace = "[\\h\\v]")
bjs_parole$stateid <- trimws(bjs_parole$stateid, whitespace = "[\\h\\v]")

# remove federal and DC
# rename state variable
bjs_parole <- bjs_parole %>% filter(stateid != "Federal" & stateid != "District of Columbia") %>% rename(state = stateid)
bjs_probation <- bjs_probation %>% filter(stateid != "Federal" & stateid != "District of Columbia") %>% rename(state = stateid)

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
save(bjs_parole,              file=paste0(sp_data_path, "/Data/bjs_parole.Rda", sep = ""))
save(bjs_probation,           file=paste0(sp_data_path, "/Data/bjs_probation.Rda", sep = ""))
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
save(bjs_parole,              file=paste0("app/data/bjs_parole.Rda", sep = ""))
save(bjs_probation,           file=paste0("app/data/bjs_probation.Rda", sep = ""))
save(csg,                     file=paste0("app/data/csg.Rda", sep = ""))
