#######################################
# Project: MCLCShiny
# File: colors.R
# Authors: Mari Roberts
# Date last updated: June 5, 2023 (MAR)
# Description:
#    Assign colors
#######################################

# assign colors for plots
darkorange  <- "#7b3014"
orange      <- "#D25E2D"
lightorange <- "#EDB799"
white       <- "#FFFFFF"
lightblue   <- "#C7E8F5"
regblue     <- "#236ca7"
darkblue    <- "#26456e"
yellow      <- "#D6C246"
gray        <- "#dcdcdc"
total_co <- lightblue
viol_co  <- yellow
tech_co  <- orange
new_o_co <- lightorange

# choose colors for line graphs within tables
colpal_fill <- c("url(#total)",
                 "url(#sup_viols)",
                 "url(#technical)",
                 "url(#new_offense)")
colpal_stroke <- c(total_co, viol_co, tech_co, new_o_co)

colpal_fill1 <- c("url(#total)",
                  "url(#technical)",
                  "url(#new_offense)")
colpal_stroke1 <- c(total_co, tech_co, new_o_co)
