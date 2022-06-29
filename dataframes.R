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

load(file = "Data/adm_pop_long.Rda")
load(file = "Data/mclc_explorer.Rda")
load(file = "Data/mclc_explorer_table.Rda")
load(file = "Data/vb_adm_pop.Rda")
load(file = "Data/state_table.Rda")
load(file = "Data/state_table_wide.Rda")
load(file = "Data/parole_table.Rda")
load(file = "Data/parole_table_wide.Rda")
load(file = "Data/probation_table.Rda")
load(file = "Data/probation_table_wide.Rda")
load(file = "Data/hex_gj.Rda")

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

#______________________________________________________
# fonts TBD
#______________________________________________________

# default_fonts <- c("system-ui", "-apple-system", "Segoe UI", "Roboto",
#                    "Helvetica Neue", "Arial", "Noto Sans", "Liberation Sans",
#                    "sans-serif", "Apple Color Emoji", "Segoe UI Emoji",
#                    "Segoe UI Symbol", "Noto Color Emoji")
default_fonts <- c("Noto Sans")

