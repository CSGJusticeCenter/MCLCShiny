#######################################
# Project: MCLCShiny
# File: app.R
# Authors: Mari Roberts
# Date last updated: June 7, 2022
# Description:
#    Run ui and server
#######################################

# set directory paths
CSG_SP_PATH = "~/The Council of State Governments/JC Research - 50 State Revocations Project/MCLC Shiny App"
L_PATH = "~/csgjc/MCLCShiny"

# run ui and server code
source("ui.R")
source("server.R")

# launch shiny app
shinyApp(ui = ui, server = server)

