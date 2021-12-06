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

# to do
# change locations of total and sup viols to have their own rows
# compare data to BJS
# add donut charts


# blue colors
#DAEAF2
#B5D6E4
#90C1D7
#6BADC9
#4698BC
#387A96
#215B71

# orange
#E18731

# greens
#6BBB5D
#BCDE85

# purples
#A86CC5
#B4ACE3