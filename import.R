#######################################
# Project: MCLCShiny
# File: import.R
# Authors: Mari Roberts
# Date: December 8, 2021
# Description: 
#    Loads packages
#    Imports data
#    Combines data by year
#    Cleans variable names
#    Creates data files for app

# Input:
#    "Data for web team v13.xlsx"
#######################################

library(formattable)

########
# Import data
########

#set wd to teams (for collaboration) - change user name to read in data
# setwd("C:/Users/mroberts/The Council of State Governments/JC Research - 50 State Revocations Project/50 State Survey (2021)")
# setwd("~/The Council of State Governments/JC Research - 50 State Survey (2021)")
# setwd("C:/Users/jmallett/The Council of State Governments/JC Research - Documents/50 State Revocations Project/50 State Survey (2021)")

# load sp file
us <- geojson_read("Data/us_states_hexgrid.geojson", what = "sp")

# load state abb
stateAbb <- read_csv("Data/stateAbb.csv")

# load admissions data
adm18 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2018")
adm19 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2019")
adm20 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2020")

# load population data
pop18 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2018")
pop19 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2019")
pop20 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2020")

# load mclc costs
costs <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Costs")

# load bjs probation data
load("Data/Annual Probation Survey, 2014/DS0001/36343-0001-Data.rda")
load("Data/Annual Probation Survey, 2015/DS0001/36618-0001-Data.rda")
load("Data/Annual Probation Survey, 2016/DS0001/37459-0001-Data.rda")
load("Data/Annual Probation Survey, 2017/DS0001/37482-0001-Data.rda")
load("Data/Annual Probation Survey, 2018/DS0001/38057-0001-Data.rda")

# load bjs parole data
load("Data/Annual Parole Survey, 2014/DS0001/36320-0001-Data.rda")
load("Data/Annual Parole Survey, 2015/DS0001/36619-0001-Data.rda")
load("Data/Annual Parole Survey, 2016/DS0001/37441-0001-Data.rda")
load("Data/Annual Parole Survey, 2017/DS0001/37471-0001-Data.rda")
load("Data/Annual Parole Survey, 2018/DS0001/38058-0001-Data.rda")

# load 2000-2022 probation and parole data (not useful, entire probation and parole numbers not prison admissions or pops)
parole_by_state <- readRDS("Data/parole_by_state.rds")
prob_by_state   <- readRDS("Data/probation_by_state.rds")

# load probation data for 2010-2020
# https://bjs.ojp.gov/library/publications/list?series_filter=Probation%20and%20Parole%20Populations
prob_exits_15.csv <- read.csv("Data/Annual Probation and Parole Surveys/prob_exits_15.csv")
prob_exits_16.csv <- read.csv("Data/Annual Probation and Parole Surveys/prob_exits_16.csv")
prob_exits_17.csv <- read.csv("Data/Annual Probation and Parole Surveys/prob_exits_17.csv")
prob_exits_18.csv <- read.csv("Data/Annual Probation and Parole Surveys/prob_exits_18.csv")
prob_exits_19.csv <- read.csv("Data/Annual Probation and Parole Surveys/prob_exits_19.csv")
prob_exits_20.csv <- read.csv("Data/Annual Probation and Parole Surveys/prob_exits_20.csv")

# load parole data for 2010-2020
parole_exits_15.csv <- read.csv("Data/Annual Probation and Parole Surveys/parole_exits_15.csv")
parole_exits_16.csv <- read.csv("Data/Annual Probation and Parole Surveys/parole_exits_16.csv")
parole_exits_17.csv <- read.csv("Data/Annual Probation and Parole Surveys/parole_exits_17.csv")
parole_exits_18.csv <- read.csv("Data/Annual Probation and Parole Surveys/parole_exits_18.csv")
parole_exits_19.csv <- read.csv("Data/Annual Probation and Parole Surveys/parole_exits_19.csv")
parole_exits_20.csv <- read.csv("Data/Annual Probation and Parole Surveys/parole_exits_20.csv")

########
# clean BJS probation 
########

prob_exits_15 <- prob_exits_15.csv %>% select(state            = X, 
                                          inc_new_sentence     = X.3, 
                                          inc_current_sentence = X.4) %>% mutate(year = 2015,
                                                                                 type = "Probation")

prob_exits_16 <- prob_exits_16.csv %>% select(state            = X, 
                                          inc_new_sentence     = X.4, 
                                          inc_current_sentence = X.5) %>% mutate(year = 2016,
                                                                                 type = "Probation")

prob_exits_17 <- prob_exits_17.csv %>% select(state            = X, 
                                          inc_new_sentence     = X.3, 
                                          inc_current_sentence = X.4) %>% mutate(year = 2017,
                                                                                 type = "Probation")

prob_exits_18 <- prob_exits_18.csv %>% select(state            = X, 
                                          inc_new_sentence     = X.3, 
                                          inc_current_sentence = X.4) %>% mutate(year = 2018,
                                                                                 type = "Probation")

prob_exits_19 <- prob_exits_19.csv %>% select(state            = X, 
                                          inc_new_sentence     = X.3, 
                                          inc_current_sentence = X.4) %>% mutate(year = 2019,
                                                                                 type = "Probation")

prob_exits_20 <- prob_exits_20.csv %>% select(state            = X, 
                                          inc_new_sentence     = X.4, 
                                          inc_current_sentence = X.5) %>% mutate(year = 2020,
                                                                                 type = "Probation")
# clean bjs data format
prob_exits_15 <- clean_bjs_prob(prob_exits_15)
prob_exits_16 <- clean_bjs_prob(prob_exits_16)
prob_exits_17 <- clean_bjs_prob(prob_exits_17)
prob_exits_18 <- clean_bjs_prob(prob_exits_18)
prob_exits_19 <- clean_bjs_prob(prob_exits_19)
prob_exits_20 <- clean_bjs_prob(prob_exits_20)

# add incarcerated variable
prob_exits_15 <- incarcerated_bjs_prob(prob_exits_15)
prob_exits_16 <- incarcerated_bjs_prob(prob_exits_16)
prob_exits_17 <- incarcerated_bjs_prob(prob_exits_17)
prob_exits_18 <- incarcerated_bjs_prob(prob_exits_18)
prob_exits_19 <- incarcerated_bjs_prob(prob_exits_19)
prob_exits_20 <- incarcerated_bjs_prob(prob_exits_20)

# make long form
prob_exits_15 <- bjs_prob_long_form(prob_exits_15)
prob_exits_16 <- bjs_prob_long_form(prob_exits_16)
prob_exits_17 <- bjs_prob_long_form(prob_exits_17)
prob_exits_18 <- bjs_prob_long_form(prob_exits_18)
prob_exits_19 <- bjs_prob_long_form(prob_exits_19)
prob_exits_20 <- bjs_prob_long_form(prob_exits_20)
  
########
# clean BJS parole 
########
  
parole_exits_15 <- parole_exits_15.csv %>% select(state        = X, 
                                              inc_new_sentence = X.3, 
                                              inc_revocation   = X.4) %>% mutate(year = 2015,
                                                                                 type = "Parole")

parole_exits_16 <- parole_exits_16.csv %>% select(state            = X, 
                                              inc_new_sentence = X.3, 
                                              inc_revocation   = X.4) %>% mutate(year = 2016,
                                                                                 type = "Parole")

parole_exits_17 <- parole_exits_17.csv %>% select(state            = X, 
                                              inc_new_sentence = X.3, 
                                              inc_revocation   = X.4) %>% mutate(year = 2017,
                                                                                 type = "Parole")

parole_exits_18 <- parole_exits_18.csv %>% select(state            = X, 
                                              inc_new_sentence = X.3, 
                                              inc_revocation   = X.4) %>% mutate(year = 2018,
                                                                                 type = "Parole")

parole_exits_19 <- parole_exits_19.csv %>% select(state            = X, 
                                              inc_new_sentence = X.3, 
                                              inc_revocation   = X.4) %>% mutate(year = 2019,
                                                                                 type = "Parole")

parole_exits_20 <- parole_exits_20.csv %>% select(state            = X, 
                                              inc_new_sentence = X.4, 
                                              inc_revocation   = X.5) %>% mutate(year = 2020,
                                                                                 type = "Parole")

parole_exits_15 <- clean_bjs_parole(parole_exits_15)
parole_exits_16 <- clean_bjs_parole(parole_exits_16)
parole_exits_17 <- clean_bjs_parole(parole_exits_17)
parole_exits_18 <- clean_bjs_parole(parole_exits_18)
parole_exits_19 <- clean_bjs_parole(parole_exits_19)
parole_exits_20 <- clean_bjs_parole(parole_exits_20)

# add incarcerated variable
parole_exits_15 <- incarcerated_bjs_parole(parole_exits_15)
parole_exits_16 <- incarcerated_bjs_parole(parole_exits_16)
parole_exits_17 <- incarcerated_bjs_parole(parole_exits_17)
parole_exits_18 <- incarcerated_bjs_parole(parole_exits_18)
parole_exits_19 <- incarcerated_bjs_parole(parole_exits_19)
parole_exits_20 <- incarcerated_bjs_parole(parole_exits_20)

# make long form
parole_exits_15 <- bjs_parole_long_form(parole_exits_15)
parole_exits_16 <- bjs_parole_long_form(parole_exits_16)
parole_exits_17 <- bjs_parole_long_form(parole_exits_17)
parole_exits_18 <- bjs_parole_long_form(parole_exits_18)
parole_exits_19 <- bjs_parole_long_form(parole_exits_19)
parole_exits_20 <- bjs_parole_long_form(parole_exits_20)

# combine data
prob_parole_exits <- rbind(parole_exits_15, parole_exits_16, parole_exits_17, parole_exits_18, parole_exits_19, parole_exits_20,
                           prob_exits_15, prob_exits_16, prob_exits_17, prob_exits_18, prob_exits_19, prob_exits_20)

# descriptions
prob_parole_exits <- prob_parole_exits %>% mutate(text = case_when(
  data == "incarcerated" & type == "Parole"    ~ "Incarcerated with New Sentence or Revocation",
  data == "incarcerated" & type == "Probation" ~ "Incarcerated with New/Current Sentence"
  
))

# assign admissions and population variable
prob_parole_exits <- prob_parole_exits %>% mutate(adm_or_pop = case_when(
  data == "incarcerated" ~ "Population"
))

########
# clean shapefile for hex map
########

# remove DC and territories
us_map <- fortify(us, region="iso3166_2")
centers <- cbind.data.frame(data.frame(gCentroid(us, byid=TRUE), id=us@data$iso3166_2))
centers <- centers[centers$id != "DC", ]

##########
# create long form data
##########

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

########
# Wide form
########

# add adm and pop data together
adm_pop <- merge(adm, pop, by = c("states", "state_abbrev", "year"))

# assign regions
region <- adm_pop %>% mutate(region = case_when(
  states == "Florida" ~ "South",
  states == "Texas" ~ "South",
  states == "Louisiana" ~ "South",
  states == "Mississippi" ~ "South",
  states == "Alabama" ~ "South",
  states == "Georgia" ~ "South",
  states == "South Carolina" ~ "South",
  states == "Florida" ~ "South",
  states == "Oklahoma" ~ "South",
  states == "Arkansas" ~ "South",
  states == "Tennessee" ~ "South",
  states == "North Carolina" ~ "South",
  states == "Virginia" ~ "South",
  states == "West Virginia" ~ "South",
  states == "Maryland" ~ "South",
  
  states == "Pennsylvania" ~ "Northeast",
  states == "Delaware" ~ "Northeast",
  states == "New Jersey" ~ "Northeast",
  states == "New York" ~ "Northeast",
  states == "Connecticut" ~ "Northeast",
  states == "Massachusetts" ~ "Northeast",
  states == "Vermont" ~ "Northeast",
  states == "New Hampshire" ~ "Northeast",
  states == "Maine" ~ "Northeast",
  states == "Rhode Island" ~ "Northeast",
  
  states == "North Dakota" ~ "Midwest",
  states == "Minnesota" ~ "Midwest",
  states == "Wisconsin" ~ "Midwest",
  states == "Michigan" ~ "Midwest",
  states == "South Dakota" ~ "Midwest",
  states == "Iowa" ~ "Midwest",
  states == "Illinois" ~ "Midwest",
  states == "Indiana" ~ "Midwest",
  states == "Ohio" ~ "Midwest",
  states == "Kansas" ~ "Midwest",
  states == "Nebraska" ~ "Midwest",
  states == "Missouri" ~ "Midwest",
  states == "Kentucky" ~ "Midwest",
  
  states == "Washington" ~ "West",
  states == "Montana" ~ "West",
  states == "Oregon" ~ "West",
  states == "Idaho" ~ "West",
  states == "Wyoming" ~ "West",
  states == "California" ~ "West",
  states == "Nevada" ~ "West",
  states == "Utah" ~ "West",
  states == "Wyoming" ~ "West",
  states == "Colorado" ~ "West",
  states == "New Mexico" ~ "West",
  states == "Alaska" ~ "West",
  states == "Hawaii" ~ "West",
  states == "Arizona" ~ "West"
)) %>% select(states, region)

# remove abbrev
adm_pop <- adm_pop %>% dplyr::select(-state_abbrev)

# change data types
adm_pop$states <- factor(adm_pop$states)
adm_pop <- adm_pop %>% mutate_if(is.character,as.numeric)

# calculate difference between total and Supervision Violation
adm_pop <- adm_pop %>% mutate(other_admissions = total_admissions-total_violation_admissions,
                              other_population = total_population-total_violation_population)

#######
# make long form
#######

# make data long form
mclc <- adm_pop 
mclc <- gather(mclc, data, total, total_admissions:other_population)

# create change from 2018 to 2019 to 2020
mclc <- mclc %>%
  group_by(states, data) %>%
  mutate(change = total/lag(total) - 1)

# # round
# mclc$change <- round(mclc$change, 0)

# add regions
mclc <- merge(mclc, region, by = "states")

# remove dups
mclc <- mclc %>% distinct()

# add data type
mclc <- mclc %>% mutate(metric = case_when(
  data == "total_admissions"                            ~ "Total",
  data == "total_violation_admissions"                  ~ "Supervision Violations",
  data == "total_probation_violation_admissions"        ~ "Probation",
  data == "new_offense_probation_violation_admissions"  ~ "New Offense",
  data == "technical_probation_violation_admissions"    ~ "Technical",
  data == "total_parole_violation_admissions"           ~ "Parole",
  data == "new_offense_parole_violation_admissions"     ~ "New Offense",
  data == "technical_parole_violation_admissions"       ~ "Technical",
  data == "other_admissions"                            ~ "Other",
  
  data == "total_population"                            ~ "Total",
  data == "total_violation_population"                  ~ "Supervision Violations",
  data == "total_probation_violation_population"        ~ "Probation",
  data == "new_offense_probation_violation_population"  ~ "New Offense",
  data == "technical_probation_violation_population"    ~ "Technical",
  data == "total_parole_violation_population"           ~ "Parole",
  data == "new_offense_parole_violation_population"     ~ "New Offense",
  data == "technical_parole_violation_population"       ~ "Technical",
  data == "other_population"                            ~ "Other"
))

# create pop vs adm variable
mclc <- mclc %>% mutate(adm_or_pop = ifelse(
  grepl("population", data), "Population", "Admissions"
))

# change data types
mclc$states <- as.factor(mclc$states)
mclc$year <- as.factor(mclc$year)
mclc$metric <- as.factor(mclc$metric)
mclc$data <- as.factor(mclc$data)
mclc$region <- as.factor(mclc$region)
mclc$total <- as.numeric(mclc$total)

# remove 2018 since change is missing
mclc_change <- mclc %>% filter(year != 2018)

# remove inf and NaN
mclc_change[mapply(is.infinite, mclc_change)] <- NA
mclc_change$change[mclc_change$change %in% "NaN"] <- NA
mclc[mapply(is.infinite, mclc)] <- NA
mclc$change[mclc$change %in% "NaN"] <- NA

# add state abb
mclc_change <- merge(mclc_change, stateAbb, by.x = "states", by.y = "State")
mclc <- merge(mclc, stateAbb, by.x = "states", by.y = "State")

# remove total from mclc_change and rename change variable
mclc_change <- mclc_change %>% select(-total)
mclc_change <- mclc_change %>% rename(total = change)

# add variable for type of dataset used in conditional filter
mclc_change$choice <- "Change from Previous Year"
mclc$choice <- "Count"
temp <- mclc %>% select(-change)

# final map data
mclc_explorer <- rbind(temp, mclc_change)

########
# Long form
########

# make data long form
adm_pop_long <- gather(adm_pop, 
                       data,
                       total,
                       total_admissions:other_population, 
                       factor_key=TRUE)

# change text for metrics
adm_pop_long <- adm_pop_long %>% mutate(metric = case_when(
  data == "total_admissions"                            ~ "Total",
  data == "total_violation_admissions"                  ~ "Supervision Violations",
  data == "total_probation_violation_admissions"        ~ "Probation",
  data == "new_offense_probation_violation_admissions"  ~ "New Offense",
  data == "technical_probation_violation_admissions"    ~ "Technical",
  data == "total_parole_violation_admissions"           ~ "Parole",
  data == "new_offense_parole_violation_admissions"     ~ "New Offense",
  data == "technical_parole_violation_admissions"       ~ "Technical",
  data == "other_admissions"                            ~ "Other",
  
  data == "total_population"                            ~ "Total",
  data == "total_violation_population"                  ~ "Supervision Violations",
  data == "total_probation_violation_population"        ~ "Probation",
  data == "new_offense_probation_violation_population"  ~ "New Offense",
  data == "technical_probation_violation_population"    ~ "Technical",
  data == "total_parole_violation_population"           ~ "Parole",
  data == "new_offense_parole_violation_population"     ~ "New Offense",
  data == "technical_parole_violation_population"       ~ "Technical",
  data == "other_population"                            ~ "Other"
))

# create probation vs parole variable
# change text for metrics
adm_pop_long <- adm_pop_long %>% mutate(prob_vs_parole = case_when(
  data == "total_admissions"                            ~ "Probation and Parole",
  data == "total_violation_admissions"                  ~ "Probation and Parole",
  data == "total_probation_violation_admissions"        ~ "Probation",
  data == "new_offense_probation_violation_admissions"  ~ "Probation",
  data == "technical_probation_violation_admissions"    ~ "Probation",
  data == "total_parole_violation_admissions"           ~ "Parole",
  data == "new_offense_parole_violation_admissions"     ~ "Parole",
  data == "technical_parole_violation_admissions"       ~ "Parole",
  
  data == "total_population"                            ~ "Probation and Parole",
  data == "total_violation_population"                  ~ "Probation and Parole",
  data == "total_probation_violation_population"        ~ "Probation",
  data == "new_offense_probation_violation_population"  ~ "Probation",
  data == "technical_probation_violation_population"    ~ "Probation",
  data == "total_parole_violation_population"           ~ "Parole",
  data == "new_offense_parole_violation_population"     ~ "Parole",
  data == "technical_parole_violation_population"       ~ "Parole",
))

# change text for metrics
adm_pop_long <- adm_pop_long %>% mutate(tech_vs_nontech = case_when(
  data == "total_admissions"                            ~ "Technical & Non-Technical",
  data == "total_violation_admissions"                  ~ "Technical & Non-Technical",
  data == "total_probation_violation_admissions"        ~ "Technical & Non-Technical",
  data == "new_offense_probation_violation_admissions"  ~ "Non-Technical",
  data == "technical_probation_violation_admissions"    ~ "Technical",
  data == "total_parole_violation_admissions"           ~ "Technical & Non-Technical",
  data == "new_offense_parole_violation_admissions"     ~ "Non-Technical",
  data == "technical_parole_violation_admissions"       ~ "Technical",
  
  data == "total_population"                            ~ "Technical & Non-Technical",
  data == "total_violation_population"                  ~ "Technical & Non-Technical",
  data == "total_probation_violation_population"        ~ "Technical & Non-Technical",
  data == "new_offense_probation_violation_population"  ~ "Non-Technical",
  data == "technical_probation_violation_population"    ~ "Technical",
  data == "total_parole_violation_population"           ~ "Technical & Non-Technical",
  data == "new_offense_parole_violation_population"     ~ "Non-Technical",
  data == "technical_parole_violation_population"       ~ "Technical"
))

# create pop vs adm variable
adm_pop_long <- adm_pop_long %>% mutate(adm_or_pop = ifelse(
  grepl("population", data), "Population", "Admissions"
))

# create change from 2018 to 2019 to 2020
adm_pop_long <- adm_pop_long %>%
  group_by(states, data) %>%
  arrange(states, data, year) %>% 
  mutate(change = (total/lag(total) - 1) * 100)

# round
adm_pop_long$change <- round(adm_pop_long$change, 0)

# create increase or decrease category
adm_pop_long <- adm_pop_long %>% 
  mutate(change_type = ifelse(
    change > 0, "increase", "decrease"
  ))

# create lowercase adm and pop
adm_pop_long <- adm_pop_long %>% 
  mutate(adm_or_pop_lc = ifelse(
    adm_or_pop == "Admissions", "admissions", "population"
  ))

# change data types
adm_pop_long$year <- as.factor(adm_pop_long$year)

########
# State table under graph
########

# select variables
state_table <- adm_pop_long %>% select(states,
                                       year,
                                       data,
                                       total,
                                       metric,
                                       adm_or_pop)

# summarise by type
state_table <- state_table %>% 
  group_by(states, year, metric, adm_or_pop) %>% 
  summarise(total = sum(total))

# remove probation, parole and other
state_table <- state_table %>% 
  filter(metric != "Probation" &
         metric != "Parole" &
         metric != "Other")

# create text for table
state_table <- state_table %>% mutate(text = case_when(
  metric == "New Offense" & adm_or_pop == "Admissions"            ~ "New Offense Admissions",
  metric == "Supervision Violations" & adm_or_pop == "Admissions" ~ "Supervision Violation Admissions",
  metric == "Technical" & adm_or_pop == "Admissions"              ~ "Technical Admissions",
  metric == "Total" & adm_or_pop == "Admissions"                  ~ "Total Admissions",
  
  metric == "New Offense" & adm_or_pop == "Population"            ~ "New Offense Population",
  metric == "Supervision Violations" & adm_or_pop == "Population" ~ "Supervision Violation Population",
  metric == "Technical" & adm_or_pop == "Population"              ~ "Technical Population",
  metric == "Total" & adm_or_pop == "Population"                  ~ "Total Population"
))

# make wide form
state_table_wide <- spread(state_table, key = year, value = total)

# order data for table output
state_table_wide <- state_table_wide %>%
  mutate(order = case_when(
    metric == "New Offense"             ~ 4,
    metric == "Supervision Violations"  ~ 2,
    metric == "Technical"               ~ 3,
    metric == "Total"                   ~ 1,

    metric == "New Offense"             ~ 4,
    metric == "Supervision Violations"  ~ 2,
    metric == "Technical"               ~ 3,
    metric == "Total"                   ~ 1)) %>% 
    # 3 year change
  mutate(three_yr_change = (`2020`-`2018`)/`2018`)

# rearrange data
state_table <- state_table %>% select(states, text, adm_or_pop, everything()) %>%
  ungroup() %>%
  select(-metric)

# rearrange data
state_table_wide <- state_table_wide %>% select(states, text, `2018`, `2019`, `2020`, three_yr_change, everything()) %>%
  ungroup() %>%
  select(-metric)

########
# Parole table under graph
########

# select variables
parole_table <- adm_pop_long %>% select(states,
                                        year,
                                        data,
                                        total,
                                        metric,
                                        adm_or_pop,
                                        prob_vs_parole)

# summarise by type
parole_table <- parole_table %>% 
  group_by(states, year, metric, adm_or_pop, prob_vs_parole) %>% 
  summarise(total = sum(total))

# select
parole_table <- parole_table %>% 
  filter(prob_vs_parole == "Parole")

# create text for table
parole_table <- parole_table %>% mutate(text = case_when(
  metric == "New Offense" & adm_or_pop == "Admissions"       ~ "Parole New Offense Admissions",
  metric == "Technical" & adm_or_pop == "Admissions"         ~ "Parole Technical Admissions",
  metric == "Parole" & adm_or_pop == "Admissions"            ~ "Parole Admissions",
  
  metric == "New Offense" & adm_or_pop == "Population"       ~ "Parole New Offense Population",
  metric == "Technical" & adm_or_pop == "Population"         ~ "Parole Technical Population",
  metric == "Parole" & adm_or_pop == "Population"            ~ "Parole Population"
))

# make wide form
parole_table_wide <- spread(parole_table, key = year, value = total)

# order data for table output
parole_table_wide <- parole_table_wide %>%
  mutate(order = case_when(
    metric == "New Offense"             ~ 3,
    metric == "Technical"               ~ 2,
    metric == "Parole"                  ~ 1,
    
    metric == "New Offense"             ~ 3,
    metric == "Technical"               ~ 2,
    metric == "Parole"                  ~ 1)) %>% 
  # 3 year change
  mutate(three_yr_change = (`2020`-`2018`)/`2018`)

# rearrange data
parole_table <- parole_table %>% select(states, text, adm_or_pop, everything()) %>%
  ungroup() %>%
  select(-metric)

# rearrange data
parole_table_wide <- parole_table_wide %>% select(states, text, `2018`, `2019`, `2020`, three_yr_change, everything()) %>%
  ungroup() %>%
  select(-metric)

########
# Probation table under graph
########

# select variables
prob_table <- adm_pop_long %>% select(states,
                                      year,
                                      data,
                                      total,
                                      metric,
                                      adm_or_pop,
                                      prob_vs_parole)

# summarise by type
prob_table <- prob_table %>% 
  group_by(states, year, metric, adm_or_pop, prob_vs_parole) %>% 
  summarise(total = sum(total))

# select
prob_table <- prob_table %>% 
  filter(prob_vs_parole == "Probation")

# create text for table
prob_table <- prob_table %>% mutate(text = case_when(
  metric == "New Offense" & adm_or_pop == "Admissions"       ~ "Probation New Offense Admissions",
  metric == "Technical" & adm_or_pop == "Admissions"         ~ "Probation Technical Admissions",
  metric == "Probation" & adm_or_pop == "Admissions"         ~ "Probation Admissions",
  
  metric == "New Offense" & adm_or_pop == "Population"       ~ "Probation New Offense Population",
  metric == "Technical" & adm_or_pop == "Population"         ~ "Probation Technical Population",
  metric == "Probation" & adm_or_pop == "Population"         ~ "Probation Population"
))

# make wide form
prob_table_wide <- spread(prob_table, key = year, value = total)

# order data for table output
prob_table_wide <- prob_table_wide %>%
  mutate(order = case_when(
    metric == "New Offense"             ~ 3,
    metric == "Technical"               ~ 2,
    metric == "Probation"               ~ 1,
    
    metric == "New Offense"             ~ 3,
    metric == "Technical"               ~ 2,
    metric == "Probation"               ~ 1)) %>% 
  # 3 year change
  mutate(three_yr_change = (`2020`-`2018`)/`2018`)

# rearrange data
prob_table <- prob_table %>% select(states, text, adm_or_pop, everything()) %>%
  ungroup() %>%
  select(-metric)

# rearrange data
prob_table_wide <- prob_table_wide %>% select(states, text, `2018`, `2019`, `2020`, three_yr_change, everything()) %>%
  ungroup() %>%
  select(-metric)

########
# National numbers
# get data from website for now 
# state data will change so national numbers will change
# https://csgjusticecenter.org/publications/more-community-less-confinement/
########

year2018 <- c(629811, 259927, 1237179, 288959, 108904, 369884, 948220)
year2019 <- c(610192, 246096, 1217876, 271804, 91216, 364096, 946072)
year2020 <- c(410601, 172753, 1051101, 214773, 74849, 237848, 836328)
metric <- c("overall_admissions", 
            "admissions_for_violations", 
            "overall_population",
            "violator_population",
            "technical_violator_population",
            "other_admissions",
            "other_population")
mclc_report <- cbind(metric, year2018, year2019, year2020)
mclc_report <- as.data.frame(mclc_report)

# transform data to long form
mclc_report <- gather(mclc_report, yearname, total, year2018:year2020)

# rename years
mclc_report <- mclc_report %>% mutate(year = case_when(
  yearname == "year2018" ~ 2018,
  yearname == "year2019" ~ 2019,
  yearname == "year2020" ~ 2020
)) %>% select(-yearname)

# remove comma and round
mclc_report$total <- as.numeric(gsub(",", "", mclc_report$total))
mclc_report$total <- round(mclc_report$total, 0)
# add comma for labels in area chart
mclc_report$total1 <- scales::comma(mclc_report$total)

########
# CSG download data
########

# filter data
csg <- adm_pop_long %>% rename(state = states)

# create text from data variable
csg <- create_data_text(csg)

# select data and change data types
csg <- csg %>% ungroup() %>% select(state, year, text, total, adm_or_pop)
csg$state <- as.character(csg$state)
csg$year <- as.factor(csg$year)

########
# BJS download data
########

# rename probation dfs
prob_14 <- da36343.0001 %>% mutate(year = 2014)
prob_15 <- da36618.0001 %>% mutate(year = 2015)
prob_16 <- da37459.0001 %>% mutate(year = 2016)
prob_17 <- da37482.0001 %>% mutate(year = 2017)
prob_18 <- da38057.0001 %>% mutate(year = 2018)

# rename parole dfs
parole_14 <- da36320.0001 %>% mutate(year = 2014)
parole_15 <- da36619.0001 %>% mutate(year = 2015)
parole_16 <- da37441.0001 %>% mutate(year = 2016)
parole_17 <- da37471.0001 %>% mutate(year = 2017)
parole_18 <- da38058.0001 %>% mutate(year = 2018)

# # remove probation and parole dfs
# rm(da36343.0001, da36618.0001, da37459.0001, da37482.0001, da38057.0001)
# rm(da36320.0001, da36619.0001, da37441.0001, da37471.0001, da38058.0001)

# add data together
prob <- rbind(prob_14, prob_15, prob_16, prob_17, prob_18)
parole <- rbind(parole_14, parole_15, parole_16, parole_17, parole_18)

# clean names
prob <- clean_names(prob)
parole <- clean_names(parole)

# remove punctuation and numbers from state name
prob$stateid <- gsub('[[:punct:]]+','',prob$stateid)
prob$stateid <- gsub('[[:digit:]]+', '', prob$stateid)
parole$stateid <- gsub('[[:punct:]]+','',parole$stateid)
parole$stateid <- gsub('[[:digit:]]+', '', parole$stateid)
prob$stateid <- trimws(prob$stateid, whitespace = "[\\h\\v]")
parole$stateid <- trimws(parole$stateid, whitespace = "[\\h\\v]")

# remove federal and DC
prob <- prob %>% filter(stateid != "Federal" & stateid != "District of Columbia")
parole <- parole %>% filter(stateid != "Federal" & stateid != "District of Columbia")

# rename state variable
prob <- prob %>% rename(state_abb = state, state = stateid)
parole <- parole %>% rename(state_abb = state, state = stateid)

########
# BJS Probation
########

# select variables and rename
bjs_prob <- prob %>% select(state, state_abb, year,
                        total_pop_end = totend,          # prob population end of year 
                        entries_w_inc = eninc,           # entries with incarceration
                        entries_wo_inc = ennoinc,        # entries without incarceration
                        entries_total = toten,           # total entries to prob
                        inc_new_sentence = exincnew,     # incarceration with new sentence
                        inc_current_sentence = exincurr  # incarceration with current sentence
)

# combine inc_new_sentence + inc_current_sentence
bjs_prob <- bjs_prob %>% mutate(incarcerated = inc_new_sentence + inc_current_sentence)

# add types
bjs_prob$type <- "Probation"

# make long form
bjs_prob <- gather(bjs_prob, data, total, total_pop_end:incarcerated)

# descriptions
bjs_prob <- bjs_prob %>% mutate(text = case_when(
  data == "entries_w_inc"         & type == "Probation" ~ "Entries with Incarceration",
  data == "inc_current_sentence"  & type == "Probation" ~ "Incarcerated under Current Sentence",
  data == "inc_new_sentence"      & type == "Probation" ~ "Incarcerated with New Sentence",
  data == "incarcerated"          & type == "Probation" ~ "Incarcerated with Current or New Sentence",
  
))

# assign admissions and population variable
bjs_prob <- bjs_prob %>% mutate(adm_or_pop = case_when(
  data == "incarcerated"          ~ "Population",
  data == "inc_new_sentence"      ~ "Population",
  data == "inc_current_sentence"  ~ "Population",
  data == "entries_w_inc"         ~ "Admissions"
))

# filter to data needed (entries with incarceration, incarceration with new sentence or current sentence)
bjs_prob <- bjs_prob %>% filter(data == "entries_w_inc" | data == "incarcerated")

########
# BJS Parole
########
 
bjs_parole <- parole %>% select(state, state_abb, year,
                                entries_total = toten,           # total entries to probation 
                                inc_new_sentence = exincnew,     # incarcerated with new sentence
                                inc_w_revocation = exincrev) %>% # incarcerated with revocation 
                     mutate(incarcerated = inc_new_sentence + inc_w_revocation)

# add types
bjs_parole$type <- "Parole"

# make long form
bjs_parole <- gather(bjs_parole, data, total, entries_total:incarcerated)

# descriptions
bjs_parole <- bjs_parole %>% mutate(text = case_when(
  data == "entries_total"               & type == "Parole" ~ "Total Entries",
  data == "incarcerated_w_new_sentence" & type == "Parole" ~ "Incarcerated with New Sentence",
  data == "incarcerated_w_revocation"   & type == "Parole" ~ "Incarcerated with Revocation",
  data == "incarcerated"                & type == "Parole" ~ "Incarcerated with New Sentence or Revocation"
))

# assign admissions and population variable
bjs_parole <- bjs_parole %>% mutate(adm_or_pop = case_when(
  data == "entries_total"               ~ "Admissions",
  data == "incarcerated_w_new_sentence" ~ "Population",
  data == "incarcerated_w_revocation"   ~ "Population",
  data == "incarcerated"                ~ "Population"
))

# select data
bjs_parole <- bjs_parole %>% filter(data == "entries_total" | data == "incarcerated")

########
# BJS Data 2014-2018
########

# combine prob and parole data
bjs <- rbind(bjs_parole, bjs_prob)

# select variables
bjs <- bjs %>% select(state, year, text, total, adm_or_pop, type)

# change data types
bjs$state <- as.character(bjs$state)
bjs$year  <- as.factor(bjs$year)

########
# BJS Data 2000-2022
########

# add info about parole or probation
prob_by_state$type   <- "Probation"
parole_by_state$type <- "Parole"

# combine data
bjs_overall <- rbind(prob_by_state, parole_by_state)

########
# save Rdata
########

save(mclc_explorer,    file="mclc_explorer.Rda")

save(adm_pop_long,     file="adm_pop_long.Rda")
save(state_table,      file="state_table.Rda")
save(state_table_wide, file="state_table_wide.Rda")
save(parole_table,     file="parole_table.Rda")
save(parole_table_wide,file="parole_table_wide.Rda")
save(prob_table,       file="prob_table.Rda")
save(prob_table_wide,  file="prob_table_wide.Rda")

save(us_map,           file="us_map.Rda")
save(us,               file="us.Rda")
save(centers,          file="centers.Rda")

<<<<<<< HEAD
save(prob_parole_exits,file="prob_parole_exits.Rda")
=======
save(bjs_prob,         file="bjs_prob.Rda")
>>>>>>> d2b5e22f2869832a5866dbf9c3d7552c84dc6dde
save(bjs,              file="bjs.Rda")
save(csg,              file="csg.Rda")

