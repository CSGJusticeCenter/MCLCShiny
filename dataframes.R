#######################################
# Project: MCLCShiny
# File: dataframes.R
# Authors: Mari Roberts
# Date last updated: June 10, 2022
# Description:
#    Load data files created in import.R, assign colors, and fonts
#######################################

# path to data on research div sharepoint
# make sure SP folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# in your Renviron, set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"
sp_data_path <- csg_sp_path(file.path("MCLC Shiny App"))

#______________________________________________________
# read in R data
#______________________________________________________

load(file=paste0(sp_data_path, "/Data/mclc_explorer.Rda", sep = ""))
load(file=paste0(sp_data_path, "/Data/mclc_explorer_table.Rda", sep = ""))
load(file=paste0(sp_data_path, "/Data/vb_adm_pop.Rda", sep = ""))
load(file=paste0(sp_data_path, "/Data/state_table.Rda", sep = ""))
load(file=paste0(sp_data_path, "/Data/state_table_wide.Rda", sep = ""))
load(file=paste0(sp_data_path, "/Data/parole_table.Rda", sep = ""))
load(file=paste0(sp_data_path, "/Data/parole_table_wide.Rda", sep = ""))
load(file=paste0(sp_data_path, "/Data/probation_table.Rda", sep = ""))
load(file=paste0(sp_data_path, "/Data/probation_table_wide.Rda", sep = ""))

load(file=paste0(sp_data_path, "/Data/hex_gj.Rda", sep = ""))

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