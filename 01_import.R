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

# add adm and pop data together
adm_pop <- merge(adm, pop, by = c("States", "State Abbrev", "year"))

# clean names
adm_pop <- clean_names(adm_pop)

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

# remove data
mclc <- adm_pop %>% dplyr::select(-state_abbrev)
mclc <- gather(mclc, metric, total, total_admissions:technical_parole_violation_population)

# add regions
mclc <- merge(mclc, region, by = "states")

# set working directory
setwd("C:/Users/mroberts/OneDrive - The Council of State Governments/Desktop/csgjc/repos/MCLCShiny")

# read state data
# states <- readOGR("data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp",
#                   layer = "cb_2016_us_state_500k")
# https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
states.shp <- readOGR('data/cb_2020_us_all_500k/cb_2020_us_state_500k/cb_2020_us_state_500k.shp')
