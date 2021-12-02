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
# https://stackoverflow.com/questions/61622436/how-do-i-make-a-reactive-palette-which-changes-the-colour-of-polygons-on-a-map
# https://stackoverflow.com/questions/56690624/how-can-i-make-an-horizontal-legend-in-leaflet
# https://shiny.rstudio.com/articles/layout-guide.html
# https://medium.com/ibm-data-ai/center-diverging-colors-on-leaflet-map-515e69d7f81f