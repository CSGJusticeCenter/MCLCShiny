#######################################
# Project: MCLCShiny
# File: dataframes.R
# Authors: Mari Roberts
# Date last updated: June 13, 2022
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

#______________________________________________________
# colors TBD
#______________________________________________________

# assign colors for visualizations
# darkorange  <- "#7b3014"
# orange      <- "#d04a07"
# lightorange <- "#f98c40"
# white       <- "#FFFFFF"
# lightblue   <- "#5aa5cd"
# regblue     <- "#236ca7"
# darkblue    <- "#26456e"
darkorange  <- "#7b3014"
orange      <- "#D25E2D"
lightorange <- "#EDB799"
white       <- "#FFFFFF"
lightblue   <- "#C7E8F5"
regblue     <- "#236ca7"
darkblue    <- "#26456e"
yellow      <- "#D6C246"

gray        <- "#dcdcdc"

# assign colors to data types
# total_co <- lightorange
# viol_co  <- orange
# tech_co  <- regblue
# new_o_co <- darkblue
total_co <- lightblue
viol_co  <- yellow
tech_co  <- orange
new_o_co <- lightorange

# choose colors
colpal_fill <- c("url(#total)",
                 "url(#sup_viols)",
                 "url(#technical)",
                 "url(#new_offense)")
colpal_stroke <- c(total_co, viol_co , tech_co, new_o_co)

#______________________________________________________
# fonts
#______________________________________________________

default_fonts <- c("Graphik")

state_note <- c('Whether an incarceration is the result of a new offense or technical violation is often difficult and problematic to delineate, even in states with available data. Most states do not consider a supervision violation to be the result of a new offense unless a new felony conviction is present, meaning technical violations may include misdemeanor convictions or new arrests. "Prison" includes county jail if the county was reimbursed by the state for a person’s incarceration, which occurs in some, but not all, states. Supervision violations may include revocations (i.e., unsuccessful terminations of a supervision and completion of a sentence in prison or jail) or short-term sanctions (i.e., probation or parole jurisdiction is maintained and the person is incarcerated for a short period of time in prison or jail). Not all states impose or include short-term sanctions in their count of supervision violations.')
