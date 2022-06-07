#######################################
# Project: MCLCShiny
# File: data_libraries.R
# Authors: Mari Roberts
# Date last updated: June 7, 2022
# Description:
#    Load data files created in import.R, assign colors and fonts
#######################################

setwd(CSG_SP_PATH)

#______________________________________________________
# read in R data
#______________________________________________________

load(file="Data/mclc_explorer.Rda")
load(file="Data/mclc_explorer_table.Rda")

load(file="Data/adm_pop_long.Rda")
load(file="Data/vb_adm_pop.Rda")
load(file="Data/state_table.Rda")
load(file="Data/state_table_wide.Rda")
load(file="Data/parole_table.Rda")
load(file="Data/parole_table_wide.Rda")
load(file="Data/prob_table.Rda")
load(file="Data/prob_table_wide.Rda")

load(file="Data/hex_gj.Rda")

load(file="Data/bjs_prob_parole.Rda")
load(file="Data/bjs.Rda")
load(file="Data/csg.Rda")

#______________________________________________________
# colors
#______________________________________________________

# assign colors for visualizations
lightorange <- "#fcccac"
orange      <- "#fc9c54"
lightblue   <- "#9cccec"
darkblue    <- "#2c6c9c"
regblue     <- "#3c97da"
brown       <- "#b26e39"
gray        <- "#dcdcdc"
lightgreen  <- "#a8ddb5"

# assign colors to data types
total_co <- lightorange
viol_co  <- orange
tech_co  <- regblue
new_o_co <- darkblue
pp_co    <- lightblue
bjs_co   <- lightgreen

count_colors  <- c("#d1f4ff", lightblue, regblue, darkblue, "#2a6a99")
change_colors <- c("#af4d03", orange, lightorange, lightblue, regblue, darkblue)

#______________________________________________________
# fonts
#______________________________________________________

# default_fonts <- c("system-ui", "-apple-system", "Segoe UI", "Roboto",
#                    "Helvetica Neue", "Arial", "Noto Sans", "Liberation Sans",
#                    "sans-serif", "Apple Color Emoji", "Segoe UI Emoji",
#                    "Segoe UI Symbol", "Noto Color Emoji")
default_fonts <- c("Noto Sans")

setwd(L_PATH)
