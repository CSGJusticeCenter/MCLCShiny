#######################################
# Project: MCLCShiny
# File: app.R
# Authors: Mari Roberts
# Date: September 27, 2021
# Description: 
#    User interface for R Shiny app
#######################################

# load visualization help
source("C:/Users/mroberts/OneDrive - The Council of State Governments/Desktop/csgjc/repos/csgjc_style_guidelines/csgjc_style_guidelines.R")

# load code files 
# ui and server code
source("01_import.R")
# source("02_regional_map.R")
source("03_ui.R")
source("04_server.R")

# run app
shinyApp(ui = ui, server = server)
