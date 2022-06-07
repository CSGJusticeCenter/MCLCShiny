#######################################
# Project: MCLCShiny
# File: app.R
# Authors: Mari Roberts
# Date last updated: June 6, 2022
# Description:
#    Run ui and server
#######################################

CSG_SP_PATH = "~/The Council of State Governments/JC Research - 50 State Revocations Project/MCLC Shiny App"
L_PATH = "~/csgjc/MCLCShiny"
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

