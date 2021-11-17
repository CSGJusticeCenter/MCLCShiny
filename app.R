#######################################
# Project: MCLCShiny
# File: app.R
# Authors: Mari Roberts
# Date: November 11, 2021
# Description: 
#    Runs app
#######################################

# run R files
source("import.R")
source("ui.R")
source("server.R")

# run app
shinyApp(ui = ui, server = server)

# notes
# https://bootcamp.uxdesign.cc/how-i-built-a-data-visualization-color-palette-for-a-fortune-500-company-cf01d8a66451