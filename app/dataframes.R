#######################################
# Project: MCLCShiny
# File: dataframes.R
# Authors: Mari Roberts
# Date last updated: May 3, 2023 (MAR)
# Description:
#    Load data files created in import.R, assign colors, and fonts
#######################################

#______________________________________________________
# read in R data
# must be in local repo to publish app
#______________________________________________________

states <- state.name

load(file = "data/adm_pop_long.rds")
load(file = "data/mclc_explorer.rds")
load(file = "data/mclc_explorer_table.rds")
load(file = "data/vb_adm_pop.rds")
load(file = "data/state_table.rds")
load(file = "data/state_table_wide.rds")
load(file = "data/parole_table.rds")
load(file = "data/parole_table_wide.rds")
load(file = "data/probation_table.rds")
load(file = "data/probation_table_wide.rds")
load(file = "data/hex_gj.rds")
load(file = "data/notes.rds")
load(file = "data/csg.rds")

load(file = "data/missingness_sentences.rds")

load(file = "data/nt_na_adm.rds")
load(file = "data/nt_na_pop.rds")
load(file = "data/nt_not_na_adm.rds")
load(file = "data/nt_not_na_pop.rds")

load(file = "data/parole_na_adm.rds")
load(file = "data/parole_na_pop.rds")
load(file = "data/parole_not_na_adm.rds")
load(file = "data/parole_not_na_pop.rds")

load(file = "data/probation_na_adm.rds")
load(file = "data/probation_na_pop.rds")
load(file = "data/probation_not_na_adm.rds")
load(file = "data/probation_not_na_pop.rds")

rridata <- readRDS("data/NCRP_RRI_tables.RDS")

# consistent state note on each state report
state_note <- c('Whether an incarceration is the result of a new offense or technical violation is often difficult and problematic to delineate, even in states with available data. Most states do not consider a supervision violation to be the result of a new offense unless a new felony conviction is present, meaning technical violations may include misdemeanor convictions or new arrests. "Prison" includes county jail if the county was reimbursed by the state for a person’s incarceration, which occurs in some, but not all, states. Supervision violations may include revocations (i.e., unsuccessful terminations of a supervision and completion of a sentence in prison or jail) or short-term sanctions (i.e., probation or parole jurisdiction is maintained and the person is incarcerated for a short period of time in prison or jail). Not all states impose or include short-term sanctions in their count of supervision violations.')

#______________________________________________________
# read in highcharts
# must be in local repo to publish app
#______________________________________________________

load(file = "data/adm_maps_2018_2019.rds")
load(file = "data/adm_maps_2018_2021.rds")
load(file = "data/adm_maps_2019_2020.rds")
load(file = "data/adm_maps_2020_2021.rds")
load(file = "data/pop_maps_2018_2019.rds")
load(file = "data/pop_maps_2018_2021.rds")
load(file = "data/pop_maps_2019_2020.rds")
load(file = "data/pop_maps_2020_2021.rds")

load(file = "data/all_state_area_adm.rds")
load(file = "data/all_state_area_pop.rds")
load(file = "data/all_state_bar_adm.rds")
load(file = "data/all_state_bar_pop.rds")
load(file = "data/parole_bar_adm.rds")
load(file = "data/parole_bar_pop.rds")
load(file = "data/probation_bar_adm.rds")
load(file = "data/probation_bar_pop.rds")

#______________________________________________________
# read in reactable tables
# must be in local repo to publish app
#______________________________________________________

# not working because of library issue (htmlwidgets)
# load(file = "data/state_reactable_adm.rds")
# load(file = "data/state_reactable_pop.rds")
# load(file = "data/parole_reactable_adm.rds")
# load(file = "data/parole_reactable_pop.rds")
# load(file = "data/probation_reactable_adm.rds")
# load(file = "data/probation_reactable_pop.rds")

#______________________________________________________
# colors TBD
#______________________________________________________

source("colors.R")

#______________________________________________________
# fonts
#______________________________________________________

default_fonts <- c("Graphik")
