#######################################
# Project: MCLCShiny
# File: import.R
# Authors: Mari Roberts
# Sub-Author: Martha Eichlersmith
# Date last updated: August 1, 2023 (MAR)

# Description:
#    Imports data
#    Combines data by year
#    Cleans variable names
#    Creates data files for app

# Input:
#    mclc_data_2022_v9.xlsx         - 2022 survey data with edits (BJS data or removal)
#    us_states_hexgrid.geojson      - hex map files
#    states_notes_no_data_text.xlsx - formatted notes, sentences about missing data

# Output:
#     Data frames needed to run shiny app
#     Saves data to research SP folder
#######################################

# Load packages and custom functions
source("00_fnc_library.R")

########
# Import data
########

# Load sp file
hex <- read_sf(file.path(admin$sp_data_raw, "us_states_hexgrid.geojson")) %>%
  select(state_abb = iso3166_2) %>%
  filter(state_abb != "DC") %>%
  mutate(state_name = state.name[match(state_abb, state.abb)])

# Load state abb
stateAbb <- read.csv(file.path(admin$sp_data_raw, "stateAbb.csv"))

# Load admissions data
mclc_data <- read_excel(file.path(admin$sp_data_raw, "mclc/mclc_data_2022_v9.xlsx"),
                        sheet = "Sheet 1")

# Load info on missing sentence info
missingness_sentences <- read_excel(file.path(admin$sp_data_raw, "notes/states_notes_no_data_text.xlsx"),
                                    sheet = "Missingness 2022", skip = 1)

# Load states notes
notes_raw <- read.xlsx(file.path(admin$sp_data_raw, "notes/states_notes_no_data_text.xlsx"),
                       sheet = "Formatted Notes 2022")

# Load definitions for disparities
disparities_definitions <- read.xlsx(file.path(admin$sp_data_raw, "notes/states_notes_no_data_text.xlsx"),
                                     sheet = "Disparities Definitions")


################################################################################
# Reformat shapefile for hex map
################################################################################

# Reformat hex data
hex_gj <- hex %>%
  st_transform(3857) %>%
  sf_geojson() %>%
  fromJSON(simplifyVector = FALSE)




# Clean state abbreviations file
stateAbb <- clean_names(stateAbb)






################################################################################
# Reformat data about probation and parole being abolished
################################################################################

missingness_sentences <- missingness_sentences %>%
  clean_names() %>%
  select(state,
         supervision_violation_admissions_graph,
         parole_violation_admissions_graph,
         probation_violation_admissions_graph,
         supervision_violation_population_graph,
         parole_violation_population_graph,
         probation_violation_population_graph) %>%
  distinct() %>%
  mutate(state = gsub('Excel', "", state),
         state = gsub('[()]', "", state),
         state = trimws(state),
         supervision_violation_admissions_graph = gsub('[\"]', '', supervision_violation_admissions_graph),
         parole_violation_admissions_graph      = gsub('[\"]', '', parole_violation_admissions_graph),
         probation_violation_admissions_graph   = gsub('[\"]', '', probation_violation_admissions_graph),
         supervision_violation_population_graph = gsub('[\"]', '', supervision_violation_population_graph),
         parole_violation_population_graph      = gsub('[\"]', '', parole_violation_population_graph),
         probation_violation_population_graph   = gsub('[\"]', '', probation_violation_population_graph))






################################################################################
# Reformat notes file
################################################################################

# format checkbox and x box for parole and probation metrics
# if the state submitted the variable, it gets a green check
# if the state did not submit the variable, it gets a red x
notes <- notes_raw %>%
  clean_names() %>%
  mutate(probation_metrics =
           str_replace_all(probation_metrics,
                           c("☒" = "<br><span style='color: #248A3D;'>&#x2714;&nbsp;&nbsp;&nbsp;</span>",
                             "☐" = "<br><span style='color: #D70015;'>&#x2716;&nbsp;&nbsp;&nbsp;</span>")),
         parole_metrics =
           str_replace_all(parole_metrics,
                           c("☒" = "<br><span style='color: #248A3D;'>&#x2714;&nbsp;&nbsp;&nbsp;</span>",
                             "☐" = "<br><span style='color: #D70015;'>&#x2716;&nbsp;&nbsp;&nbsp;</span>")))

# get probation related notes
probation_notes <- notes %>%
  group_by(state) %>%
  summarize(note_lst = list(probation_metrics)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
      notes = paste(note_lst, collapse = "</p><p class = 'statetxt'>")
    , notes = paste0("<p class = 'statetxt'>", notes, "</p>")
  ) %>%
  ungroup() %>%
  select(state, notes)

# get parole related notes
parole_notes <- notes %>%
  group_by(state) %>%
  summarize(note_lst = list(parole_metrics)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    notes = paste(note_lst, collapse = "</p><p class = 'statetxt'>")
    , notes = paste0("<p class = 'statetxt'>", notes, "</p>")
  ) %>%
  ungroup() %>%
  select(state, notes)

# get parole asteriks notes
# make asteriks bold
parole_asterisks_notes <- notes %>%
  mutate(parole_asterisks = str_replace_all(parole_asterisks, "\\*+", "<b>\\0</b>")) %>%
  mutate(parole_asterisks = str_replace(parole_asterisks, "<b>\\*\\*</b>", "<br><br><b>\\*\\*</b>")) %>%
  group_by(state) %>%
  summarize(note_lst = list(parole_asterisks)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    notes = paste(note_lst, collapse = "</p><p class = 'statetxt'>")
    , notes = paste0("<p class = 'statetxt'>", notes, "</p>")
  ) %>%
  ungroup() %>%
  select(state, notes) %>%
  mutate(notes =
           str_replace_all(notes, "<p class = 'statetxt'>NA</p>", "<p class = 'statetxt'></p>"))


# get probation asteriks notes
# make asteriks bold
probation_asterisks_notes <- notes %>%
  mutate(probation_asterisks = str_replace_all(probation_asterisks, "\\*+", "<b>\\0</b>")) %>%
  mutate(probation_asterisks = str_replace(probation_asterisks, "<b>\\*\\*</b>", "<br><br><b>\\*\\*</b>")) %>%
  group_by(state) %>%
  summarize(note_lst = list(probation_asterisks)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    notes = paste(note_lst, collapse = "</p><p class = 'statetxt'>")
    , notes = paste0("<p class = 'statetxt'>", notes, "</p>")
  ) %>%
  ungroup() %>%
  select(state, notes) %>%
  mutate(notes =
           str_replace_all(notes, "<p class = 'statetxt'>NA</p>", "<p class = 'statetxt'></p>"))

# get additional notes
additional_notes <- notes %>%
  mutate(
    cy_or_fy_notes   = ifelse(is.na(cy_or_fy_notes), "", cy_or_fy_notes),
    additional_notes = ifelse(is.na(additional_notes), "", additional_notes),
    additional_notes = ifelse(is.na(additional_notes),
                              paste(additional_notes, cy_or_fy_notes, sep = " "),
                              paste(additional_notes, cy_or_fy_notes, sep = " "))) %>%
  group_by(state) %>%
  summarize(note_lst = list(additional_notes)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    notes = paste(note_lst, collapse = "</p><p class = 'statetxt'>")
    , notes = paste0("<p class = 'statetxt'>", notes, "</p>")
  ) %>%
  ungroup() %>%
  select(state, notes)



################################################################################
# Admissions and populations dataset
# Wide form of data
################################################################################

# rename variables
# calculate "other" admissions and population
adm_pop <- mclc_data %>%
  select(state = states, year, everything()) %>%
  mutate(state = factor(state)) %>%
  mutate_if(is.character, as.numeric) %>%

  select(state,
         year,
         total_admissions                            = total_prison_admissions,
         total_violation_admissions                  = total_supervision_violation_admissions,
         total_new_offense_violation_admissions,
         total_technical_violation_admissions,

         total_probation_violation_admissions        = probation_violation_admissions,
         new_offense_probation_violation_admissions,
         technical_probation_violation_admissions,

         total_parole_violation_admissions           = parole_violation_admissions,
         new_offense_parole_violation_admissions,
         technical_parole_violation_admissions,

         total_population                            = total_prison_population,
         total_violation_population                  = total_supervision_violation_population,
         total_new_offense_violation_population,
         total_technical_violation_population,

         total_probation_violation_population        = probation_violation_population,
         new_offense_probation_violation_population,
         technical_probation_violation_population,

         total_parole_violation_population           = parole_violation_population,
         new_offense_parole_violation_population,
         technical_parole_violation_population) %>%

  # change data to numeric
  mutate(state = as.factor(state)) %>%
  mutate_if(is.character, as.numeric) %>%

  # replace all zeros with NA - no states should have zeros
  mutate_at(vars(c(-"state")), ~ case_when(.==0 ~ NA, TRUE ~ .)) %>%

  # replace all NaN and zeros with NA
  mutate_at(vars(c(-"state")), ~ case_when(.=="NaN" ~ NA, TRUE ~ .))






################################################################################
# MAP EXPLORER PAGE
# Value box data
################################################################################

# remove prob and parole variables
mclc <- adm_pop %>%
  select(-c(new_offense_probation_violation_admissions, new_offense_parole_violation_admissions,
            new_offense_probation_violation_population, new_offense_parole_violation_population,
            technical_probation_violation_admissions,   technical_parole_violation_admissions,
            technical_probation_violation_population,   technical_parole_violation_population)) %>%
  # change data to numeric
  mutate(state = as.factor(state)) %>%
  mutate_if(is.character, as.numeric) %>%
  # replace all zeros with NA - no states should have zeros
  mutate_at(vars(c(-"state")), ~ case_when(.==0 ~ NA, TRUE ~ .)) %>%
  # make long form
  gather(data, total, total_admissions:total_parole_violation_population)

# create change from 2018 to 2019 to 2020
# remove dups
# create label ready variable called metric
# create pop vs adm variable
# change data types
# add state abbreviations
mclc_all <- mclc %>%
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
                     data == "total_new_offense_violation_admissions"      ~ "New Offense Violation",
                     data == "total_technical_violation_admissions"        ~ "Technical Violation",

                     data == "total_population"                            ~ "Total",
                     data == "total_violation_population"                  ~ "Supervision Violation",
                     data == "total_probation_violation_population"        ~ "Probation Violation",
                     data == "total_parole_violation_population"           ~ "Parole Violation",
                     data == "total_new_offense_violation_population"      ~ "New Offense Violation",
                     data == "total_technical_violation_population"        ~ "Technical Violation"),
         adm_or_pop = ifelse(grepl("population", data), "Population", "Admissions"),
         data = paste0(metric, " " , adm_or_pop)) %>%
  mutate_if(is.character, as.factor) %>%
  left_join(stateAbb, by = "state") %>%
  select(state, year, total, everything())

# save labels
labels <- mclc_all %>% ungroup() %>%
  select(data, metric, adm_or_pop) %>% distinct()

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
         `2020 - 2021` = `2021`)

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

# get trend data and trend line whether negative or positive
# for 2018 to 2021
mclc_explorer_table_18_21 <- mclc_explorer_table %>%
  select(state, data, `2018`, `2019`, `2020`, `2021`) %>%
  pivot_longer(cols=c(`2018`, `2019`, `2020`, `2021`),
               names_to='year',
               values_to='total') %>%
  fnc_create_trend_data() %>%
  rename(trend_data_18_21 = total_new,
         trend_18_21 = trend)

# for 2018 to 2019
mclc_explorer_table_18_19 <- mclc_explorer_table %>%
  select(state, data, `2018`, `2019`) %>%
  pivot_longer(cols=c(`2018`, `2019`),
               names_to='year',
               values_to='total') %>%
  fnc_create_trend_data() %>%
  rename(trend_data_18_19 = total_new,
         trend_18_19 = trend)

# for 2019 to 2020
mclc_explorer_table_19_20 <- mclc_explorer_table %>%
  select(state, data, `2019`, `2020`) %>%
  pivot_longer(cols=c(`2019`, `2020`),
               names_to='year',
               values_to='total') %>%
  fnc_create_trend_data() %>%
  rename(trend_data_19_20 = total_new,
         trend_19_20 = trend)

# for 2020 to 2021
mclc_explorer_table_20_21 <- mclc_explorer_table %>%
  select(state, data, `2020`, `2021`) %>%
  pivot_longer(cols=c(`2020`, `2021`),
               names_to='year',
               values_to='total') %>%
  fnc_create_trend_data() %>%
  rename(trend_data_20_21 = total_new,
         trend_20_21 = trend)

# combine trend data together
mclc_explorer_table_long <- mclc_explorer_table_18_21 %>%
  left_join(mclc_explorer_table_18_19, by = c("state", "data")) %>%
  left_join(mclc_explorer_table_19_20, by = c("state", "data")) %>%
  left_join(mclc_explorer_table_20_21, by = c("state", "data"))

# combine data sets
mclc_explorer_table <- merge(mclc_explorer_table, mclc_explorer_table_long, by = c("state", "data"))

# data for map
# create year range
# create min and max values for legend scale
mclc_explorer <- mclc_all %>%
  filter(year != 2018) %>%
  rename(state_abb = code) %>%
  select(-abbrev) %>%
  full_join(mclc_explorer_table_4_yr, by = c("state",
                                             "data",
                                             "year",
                                             "change",
                                             "state_abb",
                                             "metric",
                                             "adm_or_pop")) %>%
  mutate(year = case_when(year == 2019 ~ "2018 - 2019",
                          year == 2020 ~ "2019 - 2020",
                          year == 2021 ~ "2020 - 2021",
                          year == 2022 ~ "2018 - 2021"),
         change = round(change*100, 1),
         tooltip = paste0("<b>", state, "</b><br>",
                          "Change in ",
                          data, "<br>from ",
                          year, "<br>",
                          change, "%<br>"),
         changelabel = ifelse(is.na(change), "-", paste0(round(change, 0), "%", sep = ""))) %>%
  ungroup() %>%
  group_by(year, data) %>%
  mutate(min_map = round(min(change, na.rm = TRUE), 0), # use -1 to round up to nearest tenth
         max_map = round(max(change, na.rm = TRUE), 0)) %>%
  mutate(# get absolute value for comparison
    min_map_abs = abs(min_map),
    max_map_abs = abs(max_map),
    min_map_type = ifelse(min_map >= 0, "positive", "negative"),
    max_map_type = ifelse(max_map >= 0, "positive", "negative"))





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
                          metric == "Technical Violation"   & adm_or_pop == "Admissions"  ~ "Technical Violation Admissions",
                          metric == "Total"                 & adm_or_pop == "Admissions"  ~ "Total Admissions",

                          metric == "New Offense Violation" & adm_or_pop == "Population"  ~ "New Offense Violation Population",
                          metric == "Supervision Violation" & adm_or_pop == "Population"  ~ "Supervision Violation Population",
                          metric == "Technical Violation"   & adm_or_pop == "Population"  ~ "Technical Population",
                          metric == "Total"                 & adm_or_pop == "Population"  ~ "Total Population")) %>%
  select(state, text, adm_or_pop, everything()) %>%
  pivot_wider(names_from = year, values_from = total) %>%
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
  rowwise() %>%
  mutate(total_new = list(list(c(`2018`, `2019`, `2020`, `2021`)))) %>%
  ungroup()





################################################################################
# Parole table under graph
################################################################################

# make data long form
prob_parole_tables <- gather(adm_pop, data, total, total_admissions:technical_parole_violation_population)

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

parole_table <- prob_parole_tables %>%
  filter(prob_vs_parole == "Parole") %>%
  pivot_wider(names_from = year, values_from = total) %>%
  mutate(four_yr_change = (`2021` - `2018`) / `2018`,
         order = case_when(
           metric == "New Offense Violation" ~ 3,
           metric == "Technical Violation"   ~ 2,
           metric == "Parole Violation"      ~ 1)) %>%
  arrange(order) %>%
  ungroup() %>%
  dplyr::select(state, adm_or_pop, text, `2018`, `2019`, `2020`, `2021`, four_yr_change) %>%
  rowwise() %>%
  mutate(total_new = list(list(c(`2018`, `2019`, `2020`, `2021`)))) %>%
  ungroup()




################################################################################
# Probation table under graph
################################################################################

probation_table <- prob_parole_tables %>%
  filter(prob_vs_parole == "Probation") %>%
  pivot_wider(names_from = year, values_from = total) %>%
  mutate(four_yr_change = (`2021` - `2018`) / `2018`,
         order = case_when(
           metric == "New Offense Violation" ~ 3,
           metric == "Technical Violation"   ~ 2,
           metric == "Probation Violation"   ~ 1)) %>%
  arrange(order) %>%
  ungroup() %>%
  dplyr::select(state, adm_or_pop, text, `2018`, `2019`, `2020`, `2021`, four_yr_change) %>%
  rowwise() %>%
  mutate(total_new = list(list(c(`2018`, `2019`, `2020`, `2021`)))) %>%
  ungroup()





################################################################################
# Whether data is missing parole or probation
################################################################################

# if total is equal to probation or parole, then indicate that the total only includes
#   probation or parole
# admissions
probation_or_parole_adm <- adm_pop %>%
  select(state,
         year,
         total_violation_admissions,
         total_probation_violation_admissions,
         total_parole_violation_admissions) %>%
  mutate(probation_or_parole = case_when(
    total_violation_admissions ==
      total_probation_violation_admissions ~ "(No Parole Data Available)",
    total_violation_admissions ==
      total_parole_violation_admissions    ~ "(No Probation Data Available)")
  ) %>%
  select(state, year, probation_or_parole, total_violation_admissions) %>%
  pivot_longer(cols      = total_violation_admissions,
               names_to  = "data",
               values_to = "total")

# if total is equal to probation or parole, then indicate that the total only includes
#   probation or parole
# population
probation_or_parole_pop <- adm_pop %>%
  select(state,
         year,
         total_violation_population,
         total_probation_violation_population,
         total_parole_violation_population) %>%
  mutate(probation_or_parole = case_when(
    total_violation_population ==
      total_probation_violation_population ~ "(No Parole Data Available)",
    total_violation_population ==
      total_parole_violation_population    ~ "(No Probation Data Available)")
  ) %>%
  select(state, year, probation_or_parole, total_violation_population) %>%
  pivot_longer(cols      = total_violation_population,
               names_to  = "data",
               values_to = "total")

# add probation_or_parole pop and adm together
probation_or_parole <- rbind(probation_or_parole_adm, probation_or_parole_pop)





################################################################################
# Download data tables
################################################################################

########
# CSG download data
########

# make data long form
adm_pop_long <- gather(adm_pop, data, total, total_admissions:technical_parole_violation_population)

# custom function to add text label depending on metric
# custom function to create an adm vs pop variable
# custom function to create a prob vs parole variable
adm_pop_long <- fnc_create_data_metric(adm_pop_long)
adm_pop_long <- fnc_create_adm_pop(adm_pop_long)
adm_pop_long <- fnc_create_prob_vs_parole(adm_pop_long)

# add tooltip
# add info on probation and parole
adm_pop_long <- adm_pop_long %>%
  mutate(tooltip = paste0("<b>", state, " - ", year, "</b><br>",
                          metric, " ",
                          adm_or_pop, "<br>",
                          formattable::comma(total, digits = 0), "<br>")) %>%
  arrange(state) %>%
  left_join(probation_or_parole, by=c("state", "year", "data", "total")) %>%
  group_by(state, year, adm_or_pop) %>%
  mutate(probation_or_parole = case_when(
    any(probation_or_parole
        == "(No Parole Data Available)")    ~ "(No Parole Data Available)",
    any(probation_or_parole
        == "(No Probation Data Available)") ~ "(No Probation Data Available)"
    ))

# create text labels for variable names "total_admissions = Total Admissions"
adm_pop_long <- fnc_create_data_text(adm_pop_long)

# select data and change data types
csg <- adm_pop_long %>% ungroup() %>%
  select(state,
         metric = text,
         year,
         total) %>%
  mutate(state = as.character(state),
         year = as.character(year)) %>%
  filter(!is.na(metric)) # removes total new offense and total technical





################################################################################
# states that don't have graphs because of missing data
################################################################################

# states that are missing data and will not have a graph showing technical and new offense violations
# adm
nt_na_adm1 <- missingness_sentences %>%
  filter(!is.na(supervision_violation_admissions_graph))
nt_na_adm <- nt_na_adm1$state

# states that are missing data and will not have a graph showing technical and new offense violations
# pop
nt_na_pop1 <- missingness_sentences %>%
  filter(!is.na(supervision_violation_population_graph))
nt_na_pop <- nt_na_pop1$state

# states that are NOT missing data and will have a graph showing technical and new offense violations
# adm
nt_not_na_adm1 <- missingness_sentences %>%
  ungroup() %>% select(state) %>% distinct() %>%
  anti_join(nt_na_adm1, by = "state")
nt_not_na_adm <- nt_not_na_adm1$state

# states that are NOT missing data and will have a graph showing technical and new offense violations
# pop
nt_not_na_pop1 <- missingness_sentences %>%
  ungroup() %>% select(state) %>% distinct() %>%
  anti_join(nt_na_pop1, by = "state")
nt_not_na_pop <- nt_not_na_pop1$state

# states that are missing data and will not have a parole graph
# adm
parole_na_adm1 <- missingness_sentences %>%
  filter(!is.na(parole_violation_admissions_graph))
parole_na_adm <- parole_na_adm1$state

# states that are missing data and will not have a parole graph
# pop
parole_na_pop1 <- missingness_sentences %>%
  filter(!is.na(parole_violation_population_graph))
parole_na_pop <- parole_na_pop1$state

# states that are NOT missing data and will have a graph showing technical and new offense parole violations
# adm
parole_not_na_adm1 <- missingness_sentences %>%
  select(state) %>%
  distinct() %>%
  anti_join(parole_na_adm1, by = "state")
parole_not_na_adm <- parole_not_na_adm1$state

# states that are NOT missing data and will have a graph showing technical and new offense parole violations
# pop
parole_not_na_pop1 <- missingness_sentences %>%
  select(state) %>%
  distinct() %>%
  anti_join(parole_na_pop1, by = "state")
parole_not_na_pop <- parole_not_na_pop1$state

# states that are missing data and will not have a probation graph
# adm
probation_na_adm1 <- missingness_sentences %>%
  filter(!is.na(probation_violation_admissions_graph))
probation_na_adm <- probation_na_adm1$state

# states that are missing data and will not have a probation graph
# pop
probation_na_pop1 <- missingness_sentences %>%
  filter(!is.na(probation_violation_population_graph))
probation_na_pop <- probation_na_pop1$state

# states that are NOT missing data and will have a graph showing technical and new offense probation violations
# adm
probation_not_na_adm1 <- missingness_sentences %>%
  select(state) %>%
  distinct() %>%
  anti_join(probation_na_adm1, by = "state")
probation_not_na_adm <- probation_not_na_adm1$state

# states that are NOT missing data and will have a graph showing technical and new offense probation violations
# pop
probation_not_na_pop1 <- missingness_sentences %>%
  select(state) %>%
  distinct() %>%
  anti_join(probation_na_pop1, by = "state")
probation_not_na_pop <- probation_not_na_pop1$state





################################################################################
# STATE REPORTS PAGE
# Value box data
################################################################################

# create subheader for valuebox
# get information on whether probation or parole are excluded from the data
probation_or_parole_info <- adm_pop_long %>%
  select(state, year, metric,
         adm_or_pop,
         subheader = probation_or_parole)

# create value box data
# filter to value box values (total, supervision violations, and technical violations)
# merge info on whether probation or parole are excluded from the data
vb_adm_pop <- mclc_all %>%

  filter(metric == "Total" |
           metric == "Supervision Violation" |
           metric == "Technical Violation" |
           metric == "New Offense Violation") %>%

  # create increase or decrease category for change
  mutate(change      = round(change*100, 0),
         change_type = ifelse(change > 0, "increase", "decrease")) %>%

  # merge with info on whether probation or parole are excluded from the data
  left_join(probation_or_parole_info,
            by = c("state", "year", "metric", "adm_or_pop")) %>%
  distinct() %>%

  mutate(
    # create valuebox text and value that will be displayed depending on data availability
    text        = case_when(
      is.na(change) ~ "",
      change < 0    ~ paste0(HTML("&darr;"), paste0(change, "% from 2020")),
      TRUE          ~ paste0(HTML("&uarr;"), paste0(change, "% from 2020"))
    ),

    value_shown      = case_when(
      is.na(total)  ~ "No Data",
      TRUE          ~ paste0(formattable::comma(total, digits = 0))
    ),

    # change data types
    state       = as.character(state),
    year        = as.character(year),
    metric      = as.character(metric),
    adm_or_pop  = as.character(adm_or_pop)) %>%

  # change subheader if there is no data
  mutate(subheader = case_when(value_shown == "No Data" ~ "<br>",
                               TRUE ~ subheader),
         subheader = case_when(is.na(subheader) ~ "<br>",
                               TRUE ~ subheader))






################################################################################
# save Rdata
################################################################################


theseFOLDERS <- c( "sharepoint" = admin$sp_data, "app"  = "app/data")

for (folder in theseFOLDERS){

  save(disparities_definitions,     file=file.path(folder, "disparities_definitions.rds"))
  save(adm_pop_long,                file=file.path(folder, "adm_pop_long.rds"))
  save(mclc_explorer,               file=file.path(folder, "mclc_explorer.rds"))
  save(mclc_explorer_table,         file=file.path(folder, "mclc_explorer_table.rds"))
  save(vb_adm_pop,                  file=file.path(folder, "vb_adm_pop.rds"))
  save(state_table,                 file=file.path(folder, "state_table.rds"))
  save(parole_table,                file=file.path(folder, "parole_table.rds"))
  save(probation_table,             file=file.path(folder, "probation_table.rds"))
  save(hex_gj,                      file=file.path(folder, "hex_gj.rds"))
  save(parole_notes,                file=file.path(folder, "parole_notes.rds"))
  save(probation_notes,             file=file.path(folder, "probation_notes.rds"))
  save(parole_asterisks_notes,      file=file.path(folder, "parole_asterisks_notes.rds"))
  save(probation_asterisks_notes,   file=file.path(folder, "probation_asterisks_notes.rds"))
  save(additional_notes,            file=file.path(folder, "additional_notes.rds"))
  save(csg,                         file=file.path(folder, "csg.rds"))
  save(missingness_sentences,       file=file.path(folder, "missingness_sentences.rds"))
  save(nt_na_adm,                   file=file.path(folder, "nt_na_adm.rds"))
  save(nt_na_pop,                   file=file.path(folder, "nt_na_pop.rds"))
  save(nt_not_na_adm,               file=file.path(folder, "nt_not_na_adm.rds"))
  save(nt_not_na_pop,               file=file.path(folder, "nt_not_na_pop.rds"))
  save(parole_na_adm,               file=file.path(folder, "parole_na_adm.rds"))
  save(parole_na_pop,               file=file.path(folder, "parole_na_pop.rds"))
  save(parole_not_na_adm,           file=file.path(folder, "parole_not_na_adm.rds"))
  save(parole_not_na_pop,           file=file.path(folder, "parole_not_na_pop.rds"))
  save(probation_na_adm,            file=file.path(folder, "probation_na_adm.rds"))
  save(probation_na_pop,            file=file.path(folder, "probation_na_pop.rds"))
  save(probation_not_na_adm,        file=file.path(folder, "probation_not_na_adm.rds"))
  save(probation_not_na_pop,        file=file.path(folder, "probation_not_na_pop.rds"))

}
