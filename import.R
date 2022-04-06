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
#     BJS Annual Probation and Parole Surveys
#     Shapefiles, map related files
#     Census API through tidycensus
#######################################

library(formattable)

# use to pull state population data
# census_api_key("37fb837476737351145409de9fceaa40d6164494")

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
stateAbb <- read.csv("Data/stateAbb.csv")

# load static hex map info
load(file="Data/combined.Rda")
load(file="Data/combined_labels.Rda")

# load admissions data
adm18 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2018")
adm19 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2019")
adm20 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2020")

# load population data
pop18 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2018")
pop19 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2019")
pop20 <- read_excel("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2020")

# load probation and parole exits
# https://bjs.ojp.gov/library/publications/list?series_filter=Probation%20and%20Parole%20Populations
prob_exits_20.csv <- read.csv("Data/Annual Probation and Parole Surveys/prob_exits_20.csv")
parole_exits_20.csv <- read.csv("Data/Annual Probation and Parole Surveys/parole_exits_20.csv")
prob_exits_19.csv <- read.csv("Data/Annual Probation and Parole Surveys/prob_exits_19.csv")
parole_exits_19.csv <- read.csv("Data/Annual Probation and Parole Surveys/parole_exits_19.csv")

# load probation and parole populations
comm_sup_pop_20.csv <- read.csv("Data/Annual Probation and Parole Surveys/comm_sup_pop_20.csv")
comm_sup_pop_19.csv <- read.csv("Data/Annual Probation and Parole Surveys/comm_sup_pop_19.csv")
prob_pop_20.csv     <- read.csv("Data/Annual Probation and Parole Surveys/prob_pop_20.csv")
parole_pop_20.csv   <- read.csv("Data/Annual Probation and Parole Surveys/parole_pop_20.csv")
prob_pop_19.csv     <- read.csv("Data/Annual Probation and Parole Surveys/prob_pop_19.csv")
parole_pop_19.csv   <- read.csv("Data/Annual Probation and Parole Surveys/parole_pop_19.csv")

################################################################################
# clean shapefile for leaflet hex map - not using now but might later
################################################################################

# remove DC and territories
us_map <- fortify(us, region="iso3166_2")
centers <- cbind.data.frame(data.frame(gCentroid(us, byid=TRUE), id=us@data$iso3166_2))
centers <- centers[centers$id != "DC", ]

# clean stateAbb file
stateAbb <- clean_names(stateAbb)
stateAbb <- stateAbb %>% select(state = i_state,
                                Abbrev = abbrev,
                                Code = code)

##########################################################################################
# ADM POP
# create wide form data
##########################################################################################

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

########
# Wide form
########

# add adm and pop data together
adm_pop <- merge(adm, pop, by = c("state", "state_abbrev", "year"))

# assign regions
region <- adm_pop %>% mutate(region = case_when(
  state == "Florida" ~ "South",
  state == "Texas" ~ "South",
  state == "Louisiana" ~ "South",
  state == "Mississippi" ~ "South",
  state == "Alabama" ~ "South",
  state == "Georgia" ~ "South",
  state == "South Carolina" ~ "South",
  state == "Florida" ~ "South",
  state == "Oklahoma" ~ "South",
  state == "Arkansas" ~ "South",
  state == "Tennessee" ~ "South",
  state == "North Carolina" ~ "South",
  state == "Virginia" ~ "South",
  state == "West Virginia" ~ "South",
  state == "Maryland" ~ "South",

  state == "Pennsylvania" ~ "Northeast",
  state == "Delaware" ~ "Northeast",
  state == "New Jersey" ~ "Northeast",
  state == "New York" ~ "Northeast",
  state == "Connecticut" ~ "Northeast",
  state == "Massachusetts" ~ "Northeast",
  state == "Vermont" ~ "Northeast",
  state == "New Hampshire" ~ "Northeast",
  state == "Maine" ~ "Northeast",
  state == "Rhode Island" ~ "Northeast",

  state == "North Dakota" ~ "Midwest",
  state == "Minnesota" ~ "Midwest",
  state == "Wisconsin" ~ "Midwest",
  state == "Michigan" ~ "Midwest",
  state == "South Dakota" ~ "Midwest",
  state == "Iowa" ~ "Midwest",
  state == "Illinois" ~ "Midwest",
  state == "Indiana" ~ "Midwest",
  state == "Ohio" ~ "Midwest",
  state == "Kansas" ~ "Midwest",
  state == "Nebraska" ~ "Midwest",
  state == "Missouri" ~ "Midwest",
  state == "Kentucky" ~ "Midwest",

  state == "Washington" ~ "West",
  state == "Montana" ~ "West",
  state == "Oregon" ~ "West",
  state == "Idaho" ~ "West",
  state == "Wyoming" ~ "West",
  state == "California" ~ "West",
  state == "Nevada" ~ "West",
  state == "Utah" ~ "West",
  state == "Wyoming" ~ "West",
  state == "Colorado" ~ "West",
  state == "New Mexico" ~ "West",
  state == "Alaska" ~ "West",
  state == "Hawaii" ~ "West",
  state == "Arizona" ~ "West"
)) %>% select(state, region)

# remove abbrev
adm_pop <- adm_pop %>% dplyr::select(-state_abbrev)

# change data types
adm_pop$state <- factor(adm_pop$state)
adm_pop <- adm_pop %>% mutate_if(is.character,as.numeric)

# calculate difference between total and Supervision Violation
adm_pop <- adm_pop %>% mutate(other_admissions = total_admissions-total_violation_admissions,
                              other_population = total_population-total_violation_population)

#######################################################################################
# MAP DATA
#######################################################################################

# make data long form
mclc <- adm_pop

# add prob/parole technical and new offense categories together
mclc <- mclc %>% mutate(new_offense_admissions = new_offense_probation_violation_admissions + new_offense_parole_violation_admissions,
                        new_offense_population = new_offense_probation_violation_population + new_offense_parole_violation_population,
                        technical_admissions   = technical_probation_violation_admissions   + technical_parole_violation_admissions,
                        technical_population   = technical_probation_violation_population   + technical_parole_violation_population) %>%
  select(-c(new_offense_probation_violation_admissions, new_offense_parole_violation_admissions,
            new_offense_probation_violation_population, new_offense_parole_violation_population,
            technical_probation_violation_admissions, technical_parole_violation_admissions,
            technical_probation_violation_population, technical_parole_violation_population))

# make long form
mclc <- gather(mclc, data, total, total_admissions:technical_population)

# create change from 2018 to 2019 to 2020
mclc <- mclc %>%
  group_by(state, data) %>%
  mutate(change = total/lag(total) - 1)

# add regions
mclc <- merge(mclc, region, by = "state")

# remove dups
mclc <- mclc %>% distinct()

# add data type
mclc <- mclc %>% mutate(metric = case_when(
  data == "total_admissions"                            ~ "Total",
  data == "total_violation_admissions"                  ~ "Supervision Violations",
  data == "total_probation_violation_admissions"        ~ "Probation",
  data == "total_parole_violation_admissions"           ~ "Parole",
  data == "new_offense_admissions"                      ~ "New Offense",
  data == "technical_admissions"                        ~ "Technical",
  data == "other_admissions"                            ~ "Other",

  data == "total_population"                            ~ "Total",
  data == "total_violation_population"                  ~ "Supervision Violations",
  data == "total_probation_violation_population"        ~ "Probation",
  data == "total_parole_violation_population"           ~ "Parole",
  data == "new_offense_population"                      ~ "New Offense",
  data == "technical_population"                        ~ "Technical",
  data == "other_population"                            ~ "Other"
))

# create pop vs adm variable
mclc <- mclc %>% mutate(adm_or_pop = ifelse(
  grepl("population", data), "Population", "Admissions"
))

# change data types
mclc$state <- as.factor(mclc$state)
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
mclc_change <- merge(mclc_change, stateAbb, by = "state")
mclc <- merge(mclc, stateAbb, by = "state")

# remove total from mclc_change and rename change variable
mclc_change <- mclc_change %>% select(-total)
mclc_change <- mclc_change %>% rename(total = change)

# add variable for type of dataset used in conditional filter
mclc_change$choice <- "Change from Previous Year"
mclc$choice <- "Count"
temp <- mclc %>% select(-change)

# join data
mclc_explorer <- rbind(temp, mclc_change)

# final map table
mclc_explorer_table <- mclc_explorer %>% mutate(data = paste0(metric, " " , adm_or_pop)) %>% filter(choice == "Change from Previous Year")

# make year column into column headers
mclc_explorer_table <- spread(mclc_explorer_table, year, total) %>%
  select(state,
         data,
         `2018 - 2019` = `2019`,
         `2019 - 2020` = `2020`)

# get counts to add to table
mclc_counts <- mclc_explorer %>% mutate(data = paste0(metric, " " , adm_or_pop)) %>% filter(choice == "Count")

# make year column into column headers
mclc_counts <- spread(mclc_counts, year, total) %>% select(state, data, `2018`, `2019`, `2020`)

# combine counts and change tables together
mclc_explorer_table <- left_join(mclc_counts, mclc_explorer_table, by = c("state", "data"))

# only use change for now
mclc_explorer <- mclc_explorer %>% filter(choice == "Change from Previous Year") %>% select(-choice)

# create year range
mclc_explorer <- mclc_explorer %>%
  mutate(year = case_when(year == 2019 ~ "2018 - 2019",
                          year == 2020 ~ "2019 - 2020"))

# # add state abb for merging with shapefile in server
# mclc_explorer <- merge(mclc_explorer, stateAbb, by = "state")

################################################################################
# Value box data
################################################################################

# add technical prob/parole together
vb_adm_pop <- adm_pop %>% mutate(technical_admissions = technical_probation_violation_admissions + technical_parole_violation_admissions,
                                 technical_population = technical_probation_violation_population + technical_parole_violation_population)

# make data long form
vb_adm_pop <- gather(vb_adm_pop,
                     data,
                     total,
                     total_admissions:technical_population,
                     factor_key=TRUE)

# filter to vb values
vb_adm_pop <- vb_adm_pop %>% filter(data == "total_admissions" |
                                      data == "total_violation_admissions" |
                                      data == "technical_admissions" |
                                      data == "total_population" |
                                      data == "total_violation_population" |
                                      data == "technical_population")

vb_adm_pop <- vb_adm_pop %>% mutate(metric = case_when(
  data == "total_admissions"                            ~ "Total",
  data == "total_violation_admissions"                  ~ "Supervision Violations",
  data == "technical_admissions"                        ~ "Technical",

  data == "total_population"                            ~ "Total",
  data == "total_violation_population"                  ~ "Supervision Violations",
  data == "technical_population"                        ~ "Technical"
))

# create pop vs adm variable
vb_adm_pop <- vb_adm_pop %>% mutate(adm_or_pop = ifelse(
  grepl("population", data), "Population", "Admissions"
))

# create change from 2018 to 2019 to 2020
vb_adm_pop <- vb_adm_pop %>%
  group_by(state, data, metric) %>%
  arrange(state, data, year) %>%
  mutate(change = (total/lag(total) - 1) * 100)

# round
vb_adm_pop$change <- round(vb_adm_pop$change, 0)

# create increase or decrease category
vb_adm_pop <- vb_adm_pop %>%
  mutate(change_type = ifelse(
    change > 0, "increase", "decrease"
  ))

# change data types
vb_adm_pop$year <- as.factor(vb_adm_pop$year)

################################################################################
# ADM POP LONG
# Long form
################################################################################

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
  group_by(state, data) %>%
  arrange(state, data, year) %>%
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

################################################################################
# State table under graph
################################################################################

# select variables
state_table <- adm_pop_long %>% select(state,
                                       year,
                                       data,
                                       total,
                                       metric,
                                       adm_or_pop)

# summarise by type
state_table <- state_table %>%
  group_by(state, year, metric, adm_or_pop) %>%
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
state_table <- state_table %>% select(state, text, adm_or_pop, everything()) %>%
  ungroup() %>%
  select(-metric)

# rearrange data
state_table_wide <- state_table_wide %>% select(state, text, `2018`, `2019`, `2020`, three_yr_change, everything()) %>%
  ungroup() %>%
  select(-metric)

################################################################################
# Parole table under graph
################################################################################

# select variables
parole_table <- adm_pop_long %>% select(state,
                                        year,
                                        data,
                                        total,
                                        metric,
                                        adm_or_pop,
                                        prob_vs_parole)

# summarise by type
parole_table <- parole_table %>%
  group_by(state, year, metric, adm_or_pop, prob_vs_parole) %>%
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
parole_table <- parole_table %>% select(state, text, adm_or_pop, everything()) %>%
  ungroup() %>%
  select(-metric)

# rearrange data
parole_table_wide <- parole_table_wide %>% select(state, text, `2018`, `2019`, `2020`, three_yr_change, everything()) %>%
  ungroup() %>%
  select(-metric)

################################################################################
# Probation table under graph
################################################################################

# select variables
prob_table <- adm_pop_long %>% select(state,
                                      year,
                                      data,
                                      total,
                                      metric,
                                      adm_or_pop,
                                      prob_vs_parole)

# summarise by type
prob_table <- prob_table %>%
  group_by(state, year, metric, adm_or_pop, prob_vs_parole) %>%
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
prob_table <- prob_table %>% select(state, text, adm_or_pop, everything()) %>%
  ungroup() %>%
  select(-metric)

# rearrange data
prob_table_wide <- prob_table_wide %>% select(state, text, `2018`, `2019`, `2020`, three_yr_change, everything()) %>%
  ungroup() %>%
  select(-metric)

################################################################################
# National numbers
# get data from website for now
# state data will change so national numbers will change
# https://csgjusticecenter.org/publications/more-community-less-confinement/
################################################################################

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

################################################################################
# clean BJS probation and parole for revocation rate
################################################################################

####
# probation exits
####

prob_exits_20 <- prob_exits_20.csv %>% select(state                = X,
                                              inc_new_sentence     = X.4,
                                              inc_current_sentence = X.5) %>% mutate(year = 2020,
                                                                                     type = "Probation")
prob_exits_19 <- prob_exits_19.csv %>% select(state                = X,
                                              inc_new_sentence     = X.3,
                                              inc_current_sentence = X.4) %>% mutate(year = 2019,
                                                                                     type = "Probation")
# clean bjs data format
prob_exits_20 <- clean_bjs_prob(prob_exits_20)
prob_exits_19 <- clean_bjs_prob(prob_exits_19)

# add incarcerated variable
prob_exits_20 <- incarcerated_bjs_prob(prob_exits_20)
prob_exits_19 <- incarcerated_bjs_prob(prob_exits_19)

####
# probation pop
####

prob_pop_20 <- prob_pop_20.csv %>% select(state = X,
                                          prob_pop_20 = X.1)
prob_pop_19 <- prob_pop_19.csv %>% select(state = X,
                                          prob_pop_19 = X.1)

# clean bjs data format
prob_pop_20 <- clean_bjs_prob(prob_pop_20)
prob_pop_19 <- clean_bjs_prob(prob_pop_19)

# remove commas and make numeric
prob_pop_20$prob_pop_20 <- gsub('[[:punct:]]+','',prob_pop_20$prob_pop_20)
prob_pop_20$prob_pop_20 <- as.numeric(prob_pop_20$prob_pop_20)
prob_pop_19$prob_pop_19 <- gsub('[[:punct:]]+','',prob_pop_19$prob_pop_19)
prob_pop_19$prob_pop_19 <- as.numeric(prob_pop_19$prob_pop_19)

# merge pop and exits together
prob_20 <- merge(prob_pop_20, prob_exits_20, by = "state")
prob_19 <- merge(prob_pop_19, prob_exits_19, by = "state")

# create revocation rate
prob_20 <- prob_20 %>% mutate(prob_rev_rate_20 = incarcerated/prob_pop_20) %>%
  select(state, prob_pop_20, prob_incarcerated_20 = incarcerated, prob_rev_rate_20)
prob_19 <- prob_19 %>% mutate(prob_rev_rate_19 = incarcerated/prob_pop_19) %>%
  select(state, prob_pop_19, prob_incarcerated_19 = incarcerated, prob_rev_rate_19)

####
# parole exits
####
parole_exits_20 <- parole_exits_20.csv %>% select(state = X,
                                                  inc_new_sentence = X.4,
                                                  inc_revocation   = X.5) %>% mutate(year = 2020,
                                                                                     type = "Parole")
parole_exits_19 <- parole_exits_19.csv %>% select(state = X,
                                                  inc_new_sentence = X.3,
                                                  inc_revocation   = X.4) %>% mutate(year = 2019,
                                                                                     type = "Parole")

parole_exits_20 <- clean_bjs_parole(parole_exits_20)
parole_exits_19 <- clean_bjs_parole(parole_exits_19)

# add incarcerated variable
parole_exits_20 <- incarcerated_bjs_parole(parole_exits_20)
parole_exits_19 <- incarcerated_bjs_parole(parole_exits_19)

####
# parole pop
####

parole_pop_20 <- parole_pop_20.csv %>% select(state = X,
                                              parole_pop_20 = X.1)
parole_pop_19 <- parole_pop_19.csv %>% select(state = X,
                                              parole_pop_19 = X.1)

# clean bjs data format
parole_pop_20 <- clean_bjs_parole(parole_pop_20)
parole_pop_19 <- clean_bjs_parole(parole_pop_19)

# remove commas and make numeric
parole_pop_20$parole_pop_20 <- gsub('[[:punct:]]+','',parole_pop_20$parole_pop_20)
parole_pop_20$parole_pop_20 <- as.numeric(parole_pop_20$parole_pop_20)
parole_pop_19$parole_pop_19 <- gsub('[[:punct:]]+','',parole_pop_19$parole_pop_19)
parole_pop_19$parole_pop_19 <- as.numeric(parole_pop_19$parole_pop_19)

# merge pop and exits together
parole_20 <- merge(parole_pop_20, parole_exits_20, by = "state")
parole_19 <- merge(parole_pop_19, parole_exits_19, by = "state")

# create revocation rate
parole_20 <- parole_20 %>% mutate(parole_rev_rate_20 = incarcerated/parole_pop_20) %>%
  select(state, parole_pop_20, parole_incarcerated_20 = incarcerated, parole_rev_rate_20)
parole_19 <- parole_19 %>% mutate(parole_rev_rate_19 = incarcerated/parole_pop_19) %>%
  select(state, parole_pop_19, parole_incarcerated_19 = incarcerated, parole_rev_rate_19)

####
# combine probation and parole
####

bjs_prob_parole <- merge(parole_20, prob_20, by = "state")
bjs_prob_parole <- merge(bjs_prob_parole, prob_19, by = "state")
bjs_prob_parole <- merge(bjs_prob_parole, parole_19, by = "state")

####
# get total comm population
####

comm_sup_pop_20 <- comm_sup_pop_20.csv %>% select(state    = X,
                                                  pop_20 = X.1)
comm_sup_pop_19 <- comm_sup_pop_19.csv %>% select(state    = X,
                                                  pop_19 = X.1)

# clean bjs data format
comm_sup_pop_20 <- clean_bjs_prob(comm_sup_pop_20)
comm_sup_pop_19 <- clean_bjs_prob(comm_sup_pop_19)

# remove comma
comm_sup_pop_20$pop_20 <- as.numeric(gsub(",", "", comm_sup_pop_20$pop_20))
comm_sup_pop_19$pop_19 <- as.numeric(gsub(",", "", comm_sup_pop_19$pop_19))

# merge with bjs data
bjs_prob_parole <- left_join(bjs_prob_parole, comm_sup_pop_20, by = "state")
bjs_prob_parole <- left_join(bjs_prob_parole, comm_sup_pop_19, by = "state")

# create variable for overall rev rate in 2020
bjs_prob_parole <- bjs_prob_parole %>%
  mutate(incarcerated_20 = parole_incarcerated_20 + prob_incarcerated_20)
bjs_prob_parole <- bjs_prob_parole %>%
  mutate(rev_rate_20 = incarcerated_20/pop_20)

# calculate the overall rev rate in 2019
bjs_prob_parole <- bjs_prob_parole %>%
  mutate(incarcerated_19 = parole_incarcerated_19 + prob_incarcerated_19)
bjs_prob_parole <- bjs_prob_parole %>%
  mutate(rev_rate_19 = incarcerated_19/pop_19)

# calculate rev rate change
bjs_prob_parole <- bjs_prob_parole %>%
  mutate(rev_rate_change = rev_rate_20-rev_rate_19)

bjs_prob_parole <- bjs_prob_parole %>%
  dplyr::mutate(rev_rate_20 = ifelse(state == "California" |
                                     state == "Connecticut"|
                                     state == "Illinois"|
                                     state == "Massachusetts"|
                                     state == "Minnesota"|
                                     state == "Nevada"|
                                     state == "New Jersey"|
                                     state == "New Mexico"|
                                     state == "New York"|
                                     state == "Oregon"|
                                     state == "Rhode Island"|
                                     state == "South Dakota"|
                                     state == "Vermont"|
                                     state == "Virginia"|
                                     state == "Wisconsin", NA, rev_rate_20))

bjs_prob_parole <- bjs_prob_parole %>%
  dplyr::mutate(rev_rate_change = ifelse(state == "California" |
                                         state == "Connecticut"|
                                         state == "Illinois"|
                                         state == "Massachusetts"|
                                         state == "Minnesota"|
                                         state == "Nevada"|
                                         state == "New Jersey"|
                                         state == "New Mexico"|
                                         state == "New York"|
                                         state == "Oregon"|
                                         state == "Rhode Island"|
                                         state == "South Dakota"|
                                         state == "Vermont"|
                                         state == "Virginia"|
                                         state == "Wisconsin", NA, rev_rate_change))

################################################################################
# BJS download data
################################################################################

bjs <- bjs_prob_parole %>% select(-rev_rate_change)

# make long form
incarcerated <- bjs %>%
  mutate(incarcerated_19 = parole_incarcerated_19 + prob_incarcerated_19,
         incarcerated_20 = parole_incarcerated_20 + prob_incarcerated_20) %>%
  select(state,
         parole_incarcerated_19,
         parole_incarcerated_20,
         prob_incarcerated_19,
         prob_incarcerated_20,
         incarcerated_19,
         incarcerated_20)

population <- bjs %>% select(state,
                             prob_pop_19,
                             prob_pop_20,
                             parole_pop_19,
                             parole_pop_20,
                             pop_19,
                             pop_20)

revrates <- bjs %>% select(state,
                           parole_rev_rate_19,
                           parole_rev_rate_20,
                           prob_rev_rate_19,
                           prob_rev_rate_20,
                           rev_rate_19,
                           rev_rate_20)

# make long form
incarcerated <- gather(incarcerated, data, incarcerated, parole_incarcerated_19:incarcerated_20)
population <- gather(population, data, population, prob_pop_19:pop_20)
revrates <- gather(revrates, data, rev_rate, parole_rev_rate_19:rev_rate_20)

# assign year
# assign parole vs probation
incarcerated <- incarcerated %>%
  mutate(year = ifelse(grepl("20", data), 2020, 2019),
         type = case_when(grepl("parole", data) ~ "Parole",
                          grepl("prob", data) ~ "Probation",
                          TRUE ~ "Overall")) %>% select(-data)

population <- population %>%
  mutate(year = ifelse(grepl("20", data), 2020, 2019),
         type = case_when(grepl("parole", data) ~ "Parole",
                          grepl("prob", data) ~ "Probation",
                          TRUE ~ "Overall")) %>% select(-data)

revrates <- revrates %>%
  mutate(year = ifelse(grepl("20", data), 2020, 2019),
         type = case_when(grepl("parole", data) ~ "Parole",
                          grepl("prob", data) ~ "Probation",
                          TRUE ~ "Overall")) %>% select(-data)

# add prob, parole, and overall indicators to column headers
incarcerated <- spread(incarcerated, type, incarcerated) %>%
  select(state,
         year,
         overall_incarcerated = Overall,
         parole_incarcerated = Parole,
         prob_incarcerated = Probation)

population <- spread(population, type, population) %>%
  select(state,
         year,
         overall_population = Overall,
         parole_population = Parole,
         prob_population = Probation)

revrates <- spread(revrates, type, rev_rate) %>%
  select(state,
         year,
         overall_rev_rate = Overall,
         parole_rev_rate = Parole,
         prob_rev_rate = Probation)

# combine data
bjs <- merge(population, incarcerated, by = c("state", "year"))
bjs <- merge(bjs, revrates, by = c("state", "year"))

################################################################################
# create bubble chart data
################################################################################

# select rev rates
bjs_bubble <- bjs_prob_parole %>% select(state,
                                         rev_rate_19,
                                         rev_rate_20,
                                         parole_rev_rate_19,
                                         prob_rev_rate_19,
                                         parole_rev_rate_20,
                                         prob_rev_rate_20)

# select pops
bjs_bubble_pop <- bjs_prob_parole %>% select(state,
                                             parole_pop_20,
                                             prob_pop_20,
                                             parole_pop_19,
                                             prob_pop_19,
                                             pop_20,
                                             pop_19)

# make long form
bjs_bubble <- gather(bjs_bubble, type, rate, rev_rate_19:prob_rev_rate_20)
bjs_bubble_pop <- gather(bjs_bubble_pop, type, pop, parole_pop_20:pop_19)

# create year variable
bjs_bubble <- bjs_bubble %>% mutate(year = ifelse(
  grepl("19", type), 2019, 2020
))
bjs_bubble_pop <- bjs_bubble_pop %>% mutate(year = ifelse(
  grepl("19", type), 2019, 2020
))

# create rev type variable
bjs_bubble <- bjs_bubble %>% mutate(type = case_when(
  grepl("prob", type)              ~ "Probation",
  grepl("parole", type)            ~ "Parole",
  grepl("\\<rev_rate_19\\>", type) ~ "Overall",
  grepl("\\<rev_rate_20\\>", type) ~ "Overall"
))
# create rev type variable
bjs_bubble_pop <- bjs_bubble_pop %>% mutate(type = case_when(
  grepl("prob", type)         ~ "Probation",
  grepl("parole", type)       ~ "Parole",
  grepl("\\<pop_20\\>", type) ~ "Overall",
  grepl("\\<pop_19\\>", type) ~ "Overall"
))

# merge regions
bjs_bubble <- merge(bjs_bubble, region, by = "state")

# merge bubble chart dfs
bjs_bubble <- merge(bjs_bubble, bjs_bubble_pop, by = c("state", "year", "type"))

# get census data
state_pop_19 <- get_acs(geography = "state", variables = "B01001_001", year = 2019)
state_pop_20 <- get_acs(geography = "state", variables = "B01001_001", year = 2020)

# add year variables
state_pop_19 <- state_pop_19 %>% mutate(year = 2019) %>% select(state = NAME,
                                                                state_pop = estimate,
                                                                year)
state_pop_20 <- state_pop_20 %>% mutate(year = 2020) %>% select(state = NAME,
                                                                state_pop = estimate,
                                                                year)
# add state pops together
state_pop <- rbind(state_pop_19, state_pop_20)

# add estimated state pops to data
bjs_bubble <- left_join(bjs_bubble, state_pop, by = c("state", "year"))

# make incarcerated df long form
temp <- gather(incarcerated, type, incarcerated, overall_incarcerated:prob_incarcerated)
temp <- temp %>%
  mutate(type = case_when(grepl("parole", type) ~ "Parole",
                          grepl("prob", type) ~ "Probation",
                          TRUE ~ "Overall"))

# merge with incarcerated variable
bjs_bubble <- left_join(bjs_bubble, temp, by = c("state", "year", "type"))

################################################################################
# CSG download data
################################################################################

# filter data
csg <- adm_pop_long %>% rename(state = state) %>% filter(metric != "Other")

# remove dups from regions df
region <- region %>% distinct()

# merge with regions
csg <- merge(csg, region, by = "state")

# create text from data variable
csg <- create_data_text(csg)

# select data and change data types
csg <- csg %>% ungroup() %>% select(state, year, text, total)
csg$state <- as.character(csg$state)
csg$year <- as.numeric(csg$year)

csg <- csg %>% mutate(year = case_when(
  year == 1 ~ 2018,
  year == 2 ~ 2019,
  year == 3 ~ 2020
))

########
# save Rdata
########

save(mclc_explorer,       file="Data/mclc_explorer.Rda")
save(mclc_explorer_table, file="Data/mclc_explorer_table.Rda")

save(adm_pop_long,        file="Data/adm_pop_long.Rda")
save(vb_adm_pop,          file="Data/vb_adm_pop.Rda")
save(state_table,         file="Data/state_table.Rda")
save(state_table_wide,    file="Data/state_table_wide.Rda")
save(parole_table,        file="Data/parole_table.Rda")
save(parole_table_wide,   file="Data/parole_table_wide.Rda")
save(prob_table,          file="Data/prob_table.Rda")
save(prob_table_wide,     file="Data/prob_table_wide.Rda")

save(us_map,              file="Data/us_map.Rda")
save(us,                  file="Data/us.Rda")
save(centers,             file="Data/centers.Rda")
save(combined,            file="Data/combined.Rda")
save(combined_labels,     file="Data/combined_labels.Rda")

save(bjs_prob_parole,     file="Data/bjs_prob_parole.Rda")
save(bjs_bubble,          file="Data/bjs_bubble.Rda")
save(bjs,                 file="Data/bjs.Rda")
save(csg,                 file="Data/csg.Rda")

