#######################################
# Project: MCLCShiny
# File: import.R
# Authors: Mari Roberts
# Date: September 27, 2021
# Description: 
#    Imports data
#    Combines data by year
#    Cleans variable names

# Input:
#    "Data for web team v13.xlsx"
#######################################

# load necessary packages
requiredPackages = c('dplyr',
                     'janitor',
                     'readxl',
                     'DT',
                     'tidyverse',
                     'gdata',
                     'ggthemes',
                     'shiny',
                     'shinydashboard',
                     'shinythemes',
                     'ggplot2',
                     'leaflet',
                     'maps',
                     'geojsonio',
                     'rgdal',
                     'tigris',
                     'tidycensus',
                     'spData',
                     'sf',
                     'tmap',
                     'grid')

# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

#set wd to teams (for collaboration) - change user name to read in data
setwd("C:/Users/mroberts/The Council of State Governments/JC Research - 50 State Revocations Project/50 State Survey (2021)")

# read charge data for 2019 and 2020
adm18 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2018")
adm19 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2019")
adm20 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2020")

# pop
pop18 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2018")
pop19 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2019")
pop20 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2020")

##########
# make wide form data
##########

# add year to end of variable names
vars <- c("Total admissions", "Total violation admissions",                
          "Total probation violation admissions", "New offense probation violation admissions",
          "Technical probation violation admissions", "Total parole violation admissions",         
          "New offense parole violation admissions", "Technical parole violation admissions")
adm18wide <- rename.vars(adm18, from=vars, to=paste0(vars, "_2018"))
adm19wide <- rename.vars(adm19, from=vars, to=paste0(vars, "_2019"))
adm20wide <- rename.vars(adm20, from=vars, to=paste0(vars, "_2020"))
vars <- c("Total population", "Total violation population",                
          "Total probation violation population", "New offense probation violation population",
          "Technical probation violation population", "Total parole violation population",         
          "New offense parole violation population", "Technical parole violation population")
pop18wide <- rename.vars(pop18, from=vars, to=paste0(vars, "_2018"))
pop19wide <- rename.vars(pop19, from=vars, to=paste0(vars, "_2019"))
pop20wide <- rename.vars(pop20, from=vars, to=paste0(vars, "_2020"))

# merge all pop and adm together
wide_data <- merge(adm18wide, adm19wide, by = c("State Abbrev", "States"))
wide_data <- merge(wide_data, adm20wide, by = c("State Abbrev", "States"))
wide_data <- merge(wide_data, pop18wide, by = c("State Abbrev", "States"))
wide_data <- merge(wide_data, pop19wide, by = c("State Abbrev", "States"))
wide_data <- merge(wide_data, pop20wide, by = c("State Abbrev", "States"))

# clean names
wide_data <- clean_names(wide_data)

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

# costs
costs <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Costs")

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
adm_pop$year <- factor(adm_pop$year)
adm_pop <- adm_pop %>% mutate_if(is.character,as.numeric)

# make data long form
mclc <- adm_pop 
mclc <- gather(mclc, metric, total, total_admissions:technical_parole_violation_population)

# add regions
mclc <- merge(mclc, region, by = "states")

# remove dups
mclc <- mclc %>% distinct()

# change data types
mclc$states <- as.factor(mclc$states)
mclc$year <- as.factor(mclc$year)
mclc$metric <- as.factor(mclc$metric)
mclc$region <- as.factor(mclc$region)
mclc$total <- as.numeric(mclc$total)

# reorder data
mclc <- mclc %>% select(states, year, metric, region, total)

# set working directory
setwd("C:/Users/mroberts/OneDrive - The Council of State Governments/Desktop/csgjc/repos/MCLCShiny")

# read state data
states.shp <- readOGR('data/cb_2020_us_all_500k/cb_2020_us_state_500k/cb_2020_us_state_500k.shp',
                      encoding = "UTF-8", verbose = FALSE)

########
# Long form
########

# make data long form
adm_pop_long <- gather(adm_pop, 
                       data,
                       total,
                       total_admissions:technical_parole_violation_population, 
                       factor_key=TRUE)

# change text for metrics
adm_pop_long <- adm_pop_long %>% mutate(metric = case_when(
  data == "total_admissions"                            ~ "Total",
  data == "total_violation_admissions"                  ~ "All Supervision",
  data == "total_probation_violation_admissions"        ~ "All Probation",
  data == "new_offense_probation_violation_admissions"  ~ "New Offense",
  data == "technical_probation_violation_admissions"    ~ "Technical",
  data == "total_parole_violation_admissions"           ~ "All Parole",
  data == "new_offense_parole_violation_admissions"     ~ "New Offense",
  data == "technical_parole_violation_admissions"       ~ "Technical",
  
  data == "total_population"                            ~ "Total",
  data == "total_violation_population"                  ~ "All Supervision",
  data == "total_probation_violation_population"        ~ "All Probation",
  data == "new_offense_probation_violation_population"  ~ "New Offense",
  data == "technical_probation_violation_population"    ~ "Technical",
  data == "total_parole_violation_population"           ~ "All Parole",
  data == "new_offense_parole_violation_population"     ~ "New Offense",
  data == "technical_parole_violation_population"       ~ "Technical"
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
  data == "technical_parole_violation_population"       ~ "Technical",
))

# create pop vs adm variable
adm_pop_long <- adm_pop_long %>% mutate(adm_or_pop = ifelse(
  grepl("population", data), "Population", "Admissions"
))

# create change from 2018 to 2019 to 2020
adm_pop_long <- adm_pop_long %>%
    group_by(states) %>%
    mutate(change = (total/lag(total) - 1) * 100)

# round
adm_pop_long$change <- round(adm_pop_long$change, 0)

#########################################################################
# merge data with shapefile
mclc.df <- merge(states.shp, wide_data, by.x = 'NAME', by.y = "states")

# drop states
mclc.df <- mclc.df[!(mclc.df$NAME == 'Commonwealth of the Northern Mariana Islands' | 
                       mclc.df$NAME == 'American Samoa' |
                       mclc.df$NAME == 'Guam' |
                       mclc.df$NAME == 'District of Columbia' |
                       mclc.df$NAME == 'GUam' | 
                       mclc.df$NAME == 'Puerto Rico' |
                       mclc.df$NAME == 'United States Virgin Islands'), ]

# change data format
# mclc.df$NAME <- as.factor(mclc.df$NAME)
# mclc.df$state_abbrev <- as.factor(mclc.df$state_abbrev)
mclc.df$total_admissions_2018 <- as.numeric(mclc.df$total_admissions_2018)
mclc.df$total_admissions_2019 <- as.numeric(mclc.df$total_admissions_2019)
mclc.df$total_admissions_2020 <- as.numeric(mclc.df$total_admissions_2020)

##################################
# merge data with shapefile
adm_pop_long_shp <- merge(states.shp, adm_pop_long, by.x = 'NAME', by.y = "states", duplicateGeoms = TRUE)

# drop states
adm_pop_long_shp <- adm_pop_long_shp[!(adm_pop_long_shp$NAME == 'Commonwealth of the Northern Mariana Islands' | 
                                       adm_pop_long_shp$NAME == 'American Samoa' |
                                       adm_pop_long_shp$NAME == 'Guam' |
                                       adm_pop_long_shp$NAME == 'District of Columbia' |
                                       adm_pop_long_shp$NAME == 'GUam' | 
                                       adm_pop_long_shp$NAME == 'Puerto Rico' |
                                       adm_pop_long_shp$NAME == 'United States Virgin Islands'), ]
