#######################################
# Project: MCLCShiny
# File: app.R
# Authors: Mari Roberts
# Date: December 8, 2021
# Description: 
#    Runs app
#######################################

# clear working environment
rm(list=ls())

# load libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)
library(classInt)
library(grid)
library(tmap)
library(sf)
library(spData)
library(tigris)
library(maps)
library(maptools)
library(mapproj)
library(leaflet)
library(rgeos)
library(geojsonio)
library(rgdal)
library(gapminder)
library(ggiraph)
library(plotly)
library(ggplot2)
library(ggthemes)
library(gdata)
library(tidyverse)
library(DT)
library(readxl)
library(janitor)
library(dplyr)

# run R files
# source("import.R")
# source("multiple_imputation.R")
load("mclc.RData")
source("functions.R")
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
# https://stackoverflow.com/questions/36925152/r-leaflet-legend-colorbin-removing-decimals-in-between-breaks

# to do
# change locations of total and sup viols to have their own rows
# compare data to BJS

# csg color
# Red: B05D24 (any type on this color should be white, not black, for accessibility)
# Dark blue: 355DA1 (any type on this color should be white, not black, for accessibility)
# Light Blue: DEF0F6 (any type on this color should be black, dark blue, or bold red, not white, for accessibility)

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