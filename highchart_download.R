#######################################
# Project: MCLCShiny
# File: highchart_download.R
# Authors: Mari Roberts, Martha Eichlersmith
# Date last updated: August 31, 2023 (MAR)
# Description:
#    Create and save highcharts WITH LOGO (pngs) so the app loads faster
#######################################

# path to data on research div sharepoint
# make sure sharepoint folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# in your Renviron (usethis::edit_r_environ()), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"

box::use(
  prep/box/admin
  , glue[glue]
  , purrr[...]
  , dplyr[...]
  , highcharter[...]
  , htmlwidgets[saveWidget]
  , webshot2[webshot]
)


obj_list <- c(
  "adm_pop_long.rds"
  , "mclc_explorer.rds"
  , "hex_gj.rds"
)

walk(obj_list, ~load(file = file.path(admin$sp_data, .x), envir = .GlobalEnv))



# assign colors for visualizations
# load functions
source("app/colors.R")
source("fnc_library.R")

# list of states
states <- adm_pop_long$state %>%
  unique() %>%
  sort()

states_list <- list('Alabama',
                    'Alaska',
                    'Arizona',
                    'Arkansas',
                    'California',
                    'Colorado',
                    'Connecticut',
                    'Delaware',
                    'Florida',
                    'Georgia',
                    'Hawaii',
                    'Idaho',
                    'Illinois',
                    'Indiana',
                    'Iowa',
                    'Kansas',
                    'Kentucky',
                    'Louisiana',
                    'Maine',
                    'Maryland',
                    'Massachusetts',
                    'Michigan',
                    'Minnesota',
                    'Mississippi',
                    'Missouri',
                    'Montana',
                    'Nebraska',
                    'Nevada',
                    'New Hampshire',
                    'New Jersey',
                    'New Mexico',
                    'New York',
                    'North Carolina',
                    'North Dakota',
                    'Ohio',
                    'Oklahoma',
                    'Oregon',
                    'Pennsylvania',
                    'Rhode Island',
                    'South Carolina',
                    'South Dakota',
                    'Tennessee',
                    'Texas',
                    'Utah',
                    'Vermont',
                    'Virginia',
                    'Washington',
                    'West Virginia',
                    'Wisconsin',
                    'Wyoming')

# metrics list
metrics <- c("New Offense Violation",
             "Parole Violation",
             "Probation Violation",
             "Supervision Violation",
             "Technical Violation",
             "Total")

# metrics list
metrics_list <- list("New Offense Violation",
                     "Parole Violation",
                     "Probation Violation",
                     "Supervision Violation",
                     "Technical Violation",
                     "Total")

################################################################################

# Save graphs as lists (with logo and data labels - need to fix area chart still)
# Will save as pngs near the end of this R file

################################################################################

####################################

# MAP EXPLORER - Maps

####################################

# generate list of map highcharts to call in app (admissions)
adm_maps_2018_2019 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2018 - 2019",
           metric     == x)
  filename <- paste("Change_", x, "_Admissions_2018 - 2019", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename, "admissions")
  return(highcharts)
})

# generate list of map highcharts to call in app (admissions)
adm_maps_2018_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2018 - 2021",
           metric     == x)
  filename <- paste("Change_", x, "_Admissions_2018 - 2021", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename, "admissions")
  return(highcharts)
})

# generate list of map highcharts to call in app (admissions)
adm_maps_2019_2020 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2019 - 2020",
           metric     == x)
  filename <- paste("Change_", x, "_Admissions_2019 - 2020", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename, "admissions")
  return(highcharts)
})

# generate list of map highcharts to call in app (admissions)
adm_maps_2020_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Admissions",
           year       == "2020 - 2021",
           metric     == x)
  filename <- paste("Change_", x, "_Admissions_2020 - 2021", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename, "admissions")
  return(highcharts)
})

# generate list of map highcharts to call in app (Population)
pop_maps_2018_2019 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2018 - 2019",
           metric     == x)
  filename <- paste("Change_", x, "_Population_2018 - 2019", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename, "population")
  return(highcharts)
})

# generate list of map highcharts to call in app (Population)
pop_maps_2018_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2018 - 2021",
           metric     == x)
  filename <- paste("Change_", x, "_Population_2018 - 2021", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename, "population")
  return(highcharts)
})

# generate list of map highcharts to call in app (Population)
pop_maps_2019_2020 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2019 - 2020",
           metric     == x)
  filename <- paste("Change_", x, "_Population_2019 - 2020", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename, "population")
  return(highcharts)
})

# generate list of map highcharts to call in app (Population)
pop_maps_2020_2021 <- map(.x = metrics,  .f = function(x) {
  df1 <- mclc_explorer %>%
    filter(adm_or_pop == "Population",
           year       == "2020 - 2021",
           metric     == x)
  filename <- paste("Change_", x, "_Population_2020 - 2021", sep = "")
  admin$mylog(glue("hc: {filename}"))
  highcharts <- fnc_highchart_map_logo(df1, filename, "population")
  return(highcharts)
})

# assign map names to list
adm_maps_2018_2019 <- setNames(adm_maps_2018_2019, metrics)
adm_maps_2018_2021 <- setNames(adm_maps_2018_2021, metrics)
adm_maps_2019_2020 <- setNames(adm_maps_2019_2020, metrics)
adm_maps_2020_2021 <- setNames(adm_maps_2020_2021, metrics)

pop_maps_2018_2019 <- setNames(pop_maps_2018_2019, metrics)
pop_maps_2018_2021 <- setNames(pop_maps_2018_2021, metrics)
pop_maps_2019_2020 <- setNames(pop_maps_2019_2020, metrics)
pop_maps_2020_2021 <- setNames(pop_maps_2020_2021, metrics)






####################################

# STATE REPORTS - State area chart for admissions

####################################

######
# Data labels: Adjusted
######

# states with adjustments to their data labels
states <- c(
  "Alabama",
  "Arizona",
  "Arkansas",
  "California",
  "Delaware",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Indiana",
  "Iowa",
  "Kansas",
  "Louisiana",
  "Maine",
  "Minnesota",
  "Mississippi",
  "Montana",
  "Nebraska",
  "South Dakota",
  "Utah",
  "Vermont",
  "Virginia",
  "Wyoming"
)

# generate list of state highcharts to call in app (admissions)
all_state_area_adm_adjusted <- map(.x = states,  .f = function(x) {
  df1 <- fnc_areachart_adm_data_prep(state_name = x)
  admin$mylog(glue("hc: Prison Admissions, {x}"))
  highcharts <- fnc_highchart_state_areachart_logo(df1,
                                                   "Prison Admissions",
                                                   sup_viol_y = 0,
                                                   tech_y = 4,
                                                   new_off_y = -2)
  return(highcharts)
})

all_state_area_adm_adjusted <- setNames(all_state_area_adm_adjusted, states)




######
# Data labels: Regular
######

# States with no changes to their data labels
states <- c(
  "Missouri",
  "Nevada",
  "New Hampshire",
  "New Mexico",
  "North Carolina",
  "North Dakota",
  "Ohio",
  "Oregon",
  "South Carolina",
  "Texas",
  "Wisconsin"
)

# generate list of state highcharts to call in app (admissions)
all_state_area_adm_regular <- map(.x = states,  .f = function(x) {
  df1 <- fnc_areachart_adm_data_prep(state_name = x)
  admin$mylog(glue("hc: Prison Admissions, {x}"))
  highcharts <- fnc_highchart_state_areachart_logo(df1,
                                                   "Prison Admissions",
                                                   sup_viol_y = 0,
                                                   tech_y = 0,
                                                   new_off_y = 0)
  return(highcharts)
})

all_state_area_adm_regular <- setNames(all_state_area_adm_regular, states)




######
# Data labels: Manual changes
######

# States with manual changes to data labels
states <- c(
  "Alaska",
  "Colorado",
  "Connecticut",
  "Florida",
  "Illinois",
  "Kentucky",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "New Jersey",
  "New York",
  "Oklahoma",
  "Pennsylvania",
  "Rhode Island",
  "Tennessee",
  "Washington",
  "West Virginia"
)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Alaska")
Alaska <- fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                             sup_viol_y = -2, tech_y = 0, new_off_y = 10)

# create graph for state ___ ISSUE
df1 <- fnc_areachart_adm_data_prep(state_name = "Colorado")
Colorado <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = -2, tech_y = 0, new_off_y = 12)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Connecticut")
Connecticut <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 10, new_off_y = 5)

# create graph for state ___ ISSUES
df1 <- fnc_areachart_adm_data_prep(state_name = "Florida")
Florida <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 0, new_off_y = 15)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Illinois")
Illinois <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 10, new_off_y = 0)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Kentucky")
Kentucky <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 10, new_off_y = 0)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Maryland")
Maryland <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 10, new_off_y = 0)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Massachusetts")
Massachusetts <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 5, new_off_y = 5)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Michigan")
Michigan <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 0, new_off_y = 10)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "New Jersey")
`New Jersey` <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = -5, tech_y = 5, new_off_y = 10)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "New York")
`New York` <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = -3, tech_y = 0, new_off_y = 0)

# create graph for state ___ ISSUES
df1 <- fnc_areachart_adm_data_prep(state_name = "Oklahoma")
Oklahoma <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 0, new_off_y = 15)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Pennsylvania")
Pennsylvania <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 0, new_off_y = 15)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Rhode Island")
`Rhode Island` <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = -5, tech_y = 10, new_off_y = 0)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Tennessee")
Tennessee <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 10, new_off_y = 0)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Washington")
Washington <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 15, new_off_y = 0)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "West Virginia")
`West Virginia` <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 0, new_off_y = 5)

# combine manual charts into a list
all_state_area_adm_manual <-
  list(Alaska,
       Colorado,
       Connecticut,
       Florida,
       Illinois,
       Kentucky,
       Maryland,
       Massachusetts,
       Michigan,
       `New Jersey`,
       `New York`,
       Oklahoma,
       Pennsylvania,
       `Rhode Island`,
       Tennessee,
       Washington,
       `West Virginia`)

# add state name to the respective graph
all_state_area_adm_manual <- setNames(all_state_area_adm_manual, states)

# combine lists into final area chart list for prison admissions by state
all_state_area_adm <- c(all_state_area_adm_adjusted,
                        all_state_area_adm_regular,
                        all_state_area_adm_manual)







####################################

# STATE REPORTS - State area chart for population

####################################

######
# Data labels: Regular
######

# regular
states <- c(
  "Alabama",
  "Alaska",
  "Arizona",
  "Arkansas",
  "Connecticut",
  "Delaware",
  "Idaho",
  "Iowa",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Michigan",
  "Minnesota",
  "Missouri",
  "Montana",
  "Nevada",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "North Dakota",
  "Ohio",
  "Oklahoma",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Utah",
  "Vermont",
  "Virginia",
  "Washington",
  "Wisconsin",
  "Wyoming"
)

# generate list of state highcharts to call in app (admissions)
all_state_area_pop_regular <- map(.x = states,  .f = function(x) {
  df1 <- fnc_areachart_pop_data_prep(state_name = x)
  admin$mylog(glue("hc: Prison Population, {x}"))
  highcharts <- fnc_highchart_state_areachart_logo(df1,
                                                   "Prison Population",
                                                   sup_viol_y = 0,
                                                   tech_y = 0,
                                                   new_off_y = 0)
  return(highcharts)
})

all_state_area_pop_regular <- setNames(all_state_area_pop_regular, states)





######
# Data labels: Manual changes
######

# manual
states <- c(
  "California",
  "Colorado",
  "Florida",
  "Georgia",
  "Hawaii",
  "Illinois",
  "Indiana",
  "Kansas",
  "Mississippi",
  "Massachusetts",
  "Nebraska",
  "New York",
  "North Carolina",
  "Oregon",
  "Pennsylvania",
  "Tennessee",
  "Texas",
  "West Virginia"
)

# create graph for California
df1 <- fnc_areachart_pop_data_prep(state_name = "California")
California <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 10)

# create graph for Colorado
df1 <- fnc_areachart_pop_data_prep(state_name = "Colorado")
Colorado <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 5)

# create graph for Florida
df1 <- fnc_areachart_pop_data_prep(state_name = "Florida")
Florida <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 5, new_off_y = 0)

# create graph for Georgia
df1 <- fnc_areachart_pop_data_prep(state_name = "Georgia")
Georgia <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 5, new_off_y = 0)

# create graph for Hawaii
df1 <- fnc_areachart_pop_data_prep(state_name = "Hawaii")
Hawaii <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 10)

# create graph for Illinois
df1 <- fnc_areachart_pop_data_prep(state_name = "Illinois")
Illinois <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 10)

# create graph for Indiana ____ ISSUES
df1 <- fnc_areachart_pop_data_prep(state_name = "Indiana")
Indiana <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 10, new_off_y = 0)

# create graph for Kansas ____ ISSUES
df1 <- fnc_areachart_pop_data_prep(state_name = "Kansas")
Kansas <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 15)

# create graph for Massachusetts
df1 <- fnc_areachart_pop_data_prep(state_name = "Massachusetts")
Massachusetts <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = -20, tech_y = -10, new_off_y = 0)

# create graph for Mississippi
df1 <- fnc_areachart_pop_data_prep(state_name = "Mississippi")
Mississippi <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 10)

# create graph for Nebraska
df1 <- fnc_areachart_pop_data_prep(state_name = "Nebraska")
Nebraska <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 5, new_off_y = 0)

# create graph for New York
df1 <- fnc_areachart_pop_data_prep(state_name = "New York")
`New York` <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 8, new_off_y = 5)

# create graph for North Carolina
df1 <- fnc_areachart_pop_data_prep(state_name = "North Carolina")
`North Carolina` <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 10)

# create graph for Oregon ____ ISSUES
df1 <- fnc_areachart_pop_data_prep(state_name = "Oregon")
Oregon <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 12)

df1 <- fnc_areachart_pop_data_prep(state_name = "Pennsylvania")
Pennsylvania <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 10, new_off_y = 0)

# create graph for Tennessee
df1 <- fnc_areachart_pop_data_prep(state_name = "Tennessee")
Tennessee <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = -5, tech_y = 5, new_off_y = 10)

# create graph for Texas
df1 <- fnc_areachart_pop_data_prep(state_name = "Texas")
Texas <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 5, new_off_y = 0)

# create graph for West Virginia
df1 <- fnc_areachart_pop_data_prep(state_name = "West Virginia")
`West Virginia` <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 10, new_off_y = 10)


# combine manual charts into a list
all_state_area_pop_manual <-
  list(California,
       Colorado,
       Florida,
       Georgia,
       Hawaii,
       Illinois,
       Indiana,
       Kansas,
       Massachusetts,
       Mississippi,
       Nebraska,
       `New York`,
       `North Carolina`,
       Oregon,
       Pennsylvania,
       Tennessee,
       Texas,
       `West Virginia`)

# add state name to the respective graph
all_state_area_pop_manual <- setNames(all_state_area_pop_manual, states)

# combine lists into final area chart list for prison population by state
all_state_area_pop <- c(all_state_area_pop_regular,
                        all_state_area_pop_manual)







####################################

# STATE REPORTS - State bar chart

####################################

# list of states for function
states <- adm_pop_long$state %>%
  unique() %>%
  sort()

# generate list of state highcharts to call in app (admissions)
all_state_bar_adm <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Admissions") %>%
    group_by(state, year, metric, adm_or_pop, probation_or_parole) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    ungroup() %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Supervision Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart_logo(df1, "Supervision Violation Admissions by Type")
  return(highcharts)
})

# set names of charts
all_state_bar_adm <- setNames(all_state_bar_adm, states)

# generate list of state highcharts to call in app (population)
all_state_bar_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population") %>%
    group_by(state, year, metric, adm_or_pop, probation_or_parole) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    ungroup() %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Supervision Violation Population by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart_logo(df1, "Supervision Violation Population by Type")
  return(highcharts)
})

# set names of charts
all_state_bar_pop <- setNames(all_state_bar_pop, states)






####################################

# STATE REPORTS - Parole bar chart

####################################

# generate list of state highcharts to call in app (admissions)
parole_bar_adm <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Admissions",
           prob_vs_parole == "Parole") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Parole Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_parole_barchart_logo(df1, "Parole Violation Admissions by Type")
  return(highcharts)
})

# set names of charts
parole_bar_adm <- setNames(parole_bar_adm,states)

# generate list of state highcharts to call in app (population)
parole_bar_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population",
           prob_vs_parole == "Parole") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Parole Violations Population by Type, {x}"))
  highcharts <- fnc_highchart_parole_barchart_logo(df1, "Parole Violation Population by Type")
  return(highcharts)
})

# set names of charts
parole_bar_pop <- setNames(parole_bar_pop,states)






####################################

# STATE REPORTS - Probation bar chart

####################################

# generate list of state highcharts to call in app (admissions)
probation_bar_adm <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Admissions",
           prob_vs_parole == "Probation") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Probation Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_probation_barchart_logo(df1, "Probation Violation Admissions by Type")
  return(highcharts)
})

# set names of charts
probation_bar_adm <- setNames(probation_bar_adm, states)

# generate list of state highcharts to call in app (population)
probation_bar_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population",
           prob_vs_parole == "Probation") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Probation Violation Population by Type, {x}"))
  highcharts <- fnc_highchart_probation_barchart_logo(df1, "Probation Violation Population by Type")
  return(highcharts)
})

# set names of charts
probation_bar_pop <- setNames(probation_bar_pop, states)



################################################################################

# Save graphs as pngs (with logo and data labels)

################################################################################

admin$mylog("!!START SAVING HIGHCHARTS AS PNGS")


# functions

add_st_name <- function(hc_obj, state = NA){

  org_title <- hc_obj$x$hc_opts$title$text
  thisstate <- ifelse(!is.na(state), state, hc_obj$x$hc_opts$series[[1]]$data[[1]]$state)

  new_title <- paste0(thisstate, " ", org_title)

  hc_obj %>%
    hc_title(text = new_title)

}


save_state_png <- function(hc_obj, folderpath, id, title){

  admin$mylog(glue("Save plot: {title} for {id}"))
  saveWidget(hc_obj, file = "temp.html", selfcontained = TRUE)
  webshot2::webshot(
    url = "temp.html"
    , file = file.path(folderpath, glue("{id}_{title}.png"))
    , zoom = 4
    , vwidth = 500
    , vheight = 500
    , delay = 1
  )

}


save_map_png <- function(hc_obj, folderpath, id, title){

  admin$mylog(glue("Save plot: {title} for {id}"))
  saveWidget(hc_obj, file = "temp.html", selfcontained = TRUE)
  webshot2::webshot(
    url = "temp.html"
    , file = file.path(folderpath, glue("Change_{id}_{title}.png"))
    , delay = 1
  )


}


# folders
theseFOLDERS <- c("app" = "app/data/plots",
                  "sharepoint" = file.path(admin$sp_data, "plots"))
savefolder <- theseFOLDERS[1]
copyfolder <- theseFOLDERS[2]


# remove pngs from sharepoint and app
for (folder in theseFOLDERS){

  walk(list.files(folder, pattern = "*.png"), ~file.remove(file.path(folder, .x)))

}



##########
# MAPS Admissions - loops are separate because of timeout issues
##########

admin$mylog("ADMISSIONS MAP 2018-2019")
# 2018-2019
walk(
  metrics_list,
  ~save_map_png(
    adm_maps_2018_2019[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2018 - 2019")
)

admin$mylog("ADMISSIONS MAP 2018-2021")
# 2018-2021
walk(
  metrics_list,
  ~save_map_png(
    adm_maps_2018_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2018 - 2021")
)


admin$mylog("ADMISSIONS MAP 2019-2020")
# 2019-2020
walk(
  metrics_list,
  ~save_map_png(
    adm_maps_2019_2020[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2019 - 2020")
)

admin$mylog("ADMISSIONS MAP 2020-2021")
# 2020-2021
walk(
  metrics_list,
  ~save_map_png(
    adm_maps_2020_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2020 - 2021")
)


##########
# MAPS POPULATION
##########

admin$mylog("POPULATIONS MAP 2018-2019")
# 2018-2019
walk(
  metrics_list,
  ~save_map_png(
    pop_maps_2018_2019[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2018 - 2019")
)

admin$mylog("POPULATIONS MAP 2018-2021")
# 2018-2021
walk(
  metrics_list,
  ~save_map_png(
    pop_maps_2018_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2018 - 2021")
)

admin$mylog("POPULATIONS MAP 2019-2020")
# 2019-2020
walk(
  metrics_list,
  ~save_map_png(
    pop_maps_2019_2020[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2019 - 2020")
)

admin$mylog("POPULATIONS MAP 2020-2021")
# 2020-2021
walk(
  metrics_list,
  ~save_map_png(
    pop_maps_2020_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2020 - 2021")
)


##########
# State Overview Area Chart
##########

admin$mylog("PRISON ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_area_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Prison_Admissions")
)


admin$mylog("PRISON POPULATION")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_area_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Prison_Population")
)



##########
# State Supervision Violation Bar Chart
##########

admin$mylog("SUPERVISION VIOLATION ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_bar_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Supervision_Violation_Admissions_by_Type")
)

admin$mylog("SUPERVISION VIOLATION POPULATION")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_bar_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Supervision_Violation_Population_by_Type")
)


##########
# Probation Bar Chart
##########


admin$mylog("PROBATION VIOLATION ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(probation_bar_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Probation_Violation_Admissions_by_Type")
)


admin$mylog("PROBATION VIOLATION POPULATIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(probation_bar_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Probation_Violation_Population_by_Type")
)



##########
# Parole Bar Chart
##########

admin$mylog("PAROLE VIOLATION ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(parole_bar_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Parole_Violation_Admissions_by_Type")
)

admin$mylog("PAROLE VIOLATION POPULATIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(parole_bar_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Parole_Violation_Population_by_Type")
)

#########################

# copy over pngs from save folder folder to sharepoint
walk(
  list.files(savefolder, pattern = "*.png")
  , ~file.copy(
    from = file.path(savefolder, .x)
    , to = file.path(copyfolder, .x)
    , overwrite = TRUE
  )
)

admin$mylog("!!END SAVING HIGHCHARTS AS PNGS")

