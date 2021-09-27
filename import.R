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
                     
                     # maps
                     'shiny',
                     'shinydashboard',
                     'shinythemes',
                     'ggplot2',
                     'leaflet',
                     'maps',
                     'geojsonio',
                     'rgdal',
                     'tigris')

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

# remove data
mclc <- adm_pop %>% dplyr::select(-state_abbrev)
mclc <- gather(mclc, metric, total, total_admissions:technical_parole_violation_population)

# costs
costs <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Costs")
