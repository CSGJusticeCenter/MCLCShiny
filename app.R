#######################################
# Project: MCLCShiny
# File: app.R
# Authors: Mari Roberts
# Date: December 8, 2021
# Description:
#    Runs app
#######################################

# run R files
source("ui.R")
source("server.R")

# run app
shinyApp(ui = ui, server = server)

# notes
# https://bootcamp.uxdesign.cc/how-i-built-a-data-visualization-color-palette-for-a-fortune-500-company-cf01d8a66451
# https://shiny.rstudio.com/articles/layout-guide.html
# https://jokergoo.github.io/2020/05/21/make-circular-heatmaps/

# map notes
# https://cengel.github.io/R-spatial/mapping.html
# https://medium.com/ibm-data-ai/center-diverging-colors-on-leaflet-map-515e69d7f81f
# https://stackoverflow.com/questions/61622436/how-do-i-make-a-reactive-palette-which-changes-the-colour-of-polygons-on-a-map
# https://stackoverflow.com/questions/56690624/how-can-i-make-an-horizontal-legend-in-leaflet
# https://stackoverflow.com/questions/36925152/r-leaflet-legend-colorbin-removing-decimals-in-between-breakspng

# hex map notes
# https://rud.is/b/2015/05/14/geojson-hexagonal-statebins-in-r/
# https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html

# scales
# https://stackoverflow.com/questions/48215003/labels-for-custom-diverging-color-gradient-in-ggplot
# https://github.com/thomasp85/scico/issues/6
# https://chartio.com/learn/charts/how-to-choose-colors-data-visualization/

# csg color
# Red: B05D24 (any type on this color should be white, not black, for accessibility)
# Dark blue: 355DA1 (any type on this color should be white, not black, for accessibility)
# Light Blue: DEF0F6 (any type on this color should be black, dark blue, or bold red, not white, for accessibility)
