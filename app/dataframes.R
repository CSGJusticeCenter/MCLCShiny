#######################################
# Project: MCLCShiny
# File: dataframes.R
# Authors: Mari Roberts
# Date last updated: July 20, 2022
# Description:
#    Load data files created in import.R, assign colors, and fonts
#######################################

#______________________________________________________
# read in R data
# must be in local repo to publish app
#______________________________________________________

load(file = "data/adm_pop_long.Rda")
load(file = "data/mclc_explorer.Rda")
load(file = "data/mclc_explorer_table.Rda")
load(file = "data/vb_adm_pop.Rda")
load(file = "data/state_table.Rda")
load(file = "data/state_table_wide.Rda")
load(file = "data/parole_table.Rda")
load(file = "data/parole_table_wide.Rda")
load(file = "data/probation_table.Rda")
load(file = "data/probation_table_wide.Rda")
load(file = "data/hex_gj.Rda")
load(file = "data/notes.Rda")
load(file = "data/csg.Rda")

rridata <- readRDS("data/NCRP_RRI_tables.RDS")

# consistent state note on each state report
state_note <- c('Whether an incarceration is the result of a new offense or technical violation is often difficult and problematic to delineate, even in states with available data. Most states do not consider a supervision violation to be the result of a new offense unless a new felony conviction is present, meaning technical violations may include misdemeanor convictions or new arrests. "Prison" includes county jail if the county was reimbursed by the state for a person’s incarceration, which occurs in some, but not all, states. Supervision violations may include revocations (i.e., unsuccessful terminations of a supervision and completion of a sentence in prison or jail) or short-term sanctions (i.e., probation or parole jurisdiction is maintained and the person is incarcerated for a short period of time in prison or jail). Not all states impose or include short-term sanctions in their count of supervision violations.')

#______________________________________________________
# read in highcharts
# must be in local repo to publish app
#______________________________________________________

load(file = "data/all_state_area_adm.Rda")
load(file = "data/all_state_area_pop.Rda")
load(file = "data/all_state_bar_adm.Rda")
load(file = "data/all_state_bar_pop.Rda")
load(file = "data/parole_bar_adm.Rda")
load(file = "data/parole_bar_pop.Rda")
load(file = "data/probation_bar_adm.Rda")
load(file = "data/probation_bar_pop.Rda")

#______________________________________________________
# read in reactable tables
# must be in local repo to publish app
#______________________________________________________

# not working because of library issue (htmlwidgets)
# load(file = "data/state_reactable_adm.Rda")
# load(file = "data/state_reactable_pop.Rda")
# load(file = "data/parole_reactable_adm.Rda")
# load(file = "data/parole_reactable_pop.Rda")
# load(file = "data/probation_reactable_adm.Rda")
# load(file = "data/probation_reactable_pop.Rda")

#______________________________________________________
# colors TBD
#______________________________________________________

source("colors.R")

#______________________________________________________
# fonts
#______________________________________________________

default_fonts <- c("Graphik")

