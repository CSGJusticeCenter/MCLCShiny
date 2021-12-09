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

library(readxl)

########
# Import data
########

#set wd to teams (for collaboration) - change user name to read in data
# setwd("C:/Users/mroberts/The Council of State Governments/JC Research - 50 State Revocations Project/50 State Survey (2021)")
# setwd("~/The Council of State Governments/JC Research - 50 State Survey (2021)")
# setwd("C:/Users/jmallett/The Council of State Governments/JC Research - Documents/50 State Revocations Project/50 State Survey (2021)")

# read charge data for 2019 and 2020
adm18 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2018")
adm19 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2019")
adm20 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2020")

# pop
pop18 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2018")
pop19 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2019")
pop20 <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Population 2020")

# costs
costs <- read_xlsx("Data/Data for web team 2021 v13.xlsx", sheet = "Costs")

# read state data
# From https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
us <- readOGR(dsn = "Data/cb_2014_us_state_5m/cb_2014_us_state_5m.shp",
              layer = "cb_2014_us_state_5m", verbose = FALSE)

# BJS data


########
# clean shapefile
# move and rescale hawaii and alaska
########

# convert it to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# extract, then rotate, shrink & move alaska (and reset projection)
# need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
alaska <- us_aea[us_aea$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)

# remove old states and put new ones back in; note the different order
# we're also removing puerto rico in this example but you can move it
# between texas and florida via similar methods to the ones we just used
us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "72"),]
us_aea <- rbind(us_aea, alaska, hawaii)
# transform data again
us_aea2 <- spTransform(us_aea, proj4string(us))

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
  mutate(change = (total/lag(total) - 1) * 100)

# round
mclc$change <- round(mclc$change, 0)

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
mclc$year <- as.numeric(mclc$year)
mclc$metric <- as.factor(mclc$metric)
mclc$data <- as.factor(mclc$data)
mclc$region <- as.factor(mclc$region)
mclc$total <- as.numeric(mclc$total)

# remove 2018 since change is missing
mclc_change <- mclc %>% filter(year != 2018)

# remove inf and NaN
mclc_change[mapply(is.infinite, mclc_change)] <- NA
mclc_change$change[mclc_change$change %in% "NaN"] <- NA

########
# Data for table
########

# select variables
mclc_datatable <- mclc %>% select(State = states,
                                  Year = year,
                                  Data = metric,
                                  Type = adm_or_pop,
                                  Count = total,
                                  Region = region)

# change data types
mclc_datatable$State <- as.factor(mclc_datatable$State)
mclc_datatable$Data <- as.factor(mclc_datatable$Data)
mclc_datatable$Type <- as.factor(mclc_datatable$Type)
mclc_datatable$Region <- as.factor(mclc_datatable$Region)
mclc_datatable$Year <- as.factor(mclc_datatable$Year)

# arrange data
mclc_datatable <- mclc_datatable %>% arrange(State, Year, Data, Type)

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
  data == "technical_parole_violation_population"       ~ "Technical",
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
mclc_report$total1 <- comma(mclc_report$total)

# admissions table
df_adm <- mclc_report %>% 
  filter(metric == "admissions_for_violations" |
           metric == "other_admissions") %>% arrange(desc(metric))
df_adm$year <- as.numeric(df_adm$year)

# order factor for plotting
df_adm$metric <- factor(df_adm$metric, levels=c("other_admissions","admissions_for_violations"))

# population table
df_pop <- mclc_report %>% 
  filter(metric == "violator_population" |
           metric == "other_population") %>% arrange(metric)
df_pop$year <- as.numeric(df_pop$year)

# order factor for plotting
df_pop$metric <- factor(df_pop$metric, levels=c("other_population","violator_population"))

########
# save Rdata
########
save(mclc_datatable, file="mclc_datatable.Rda")
save(us_aea2,        file="us_aea2.Rda")
save(mclc_change,    file="mclc_change.Rda")
save(mclc,           file="mclc.Rda")
save(adm_pop_long,   file="adm_pop_long.Rda")
save(df_adm,         file="df_adm.Rda")
save(df_pop,         file="df_pop.Rda")