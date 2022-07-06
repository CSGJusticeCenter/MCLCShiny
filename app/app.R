#######################################
# Project: MCLCShiny
# File: app.R
# Authors: Mari Roberts
# Date last updated: June 7, 2022
# Description:
#    Run ui and server
#######################################

# run ui and server code
source("app/ui.R")
source("app/server.R")

# launch shiny app
shinyApp(ui = ui, server = server)
