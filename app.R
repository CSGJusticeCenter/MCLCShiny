#######################################
# Project: MCLCShiny
# File: app.R
# Authors: Mari Roberts
# Date last updated: June 6, 2022
# Description:
#    Run ui and server
#######################################

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

