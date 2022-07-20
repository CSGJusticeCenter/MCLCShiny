#######################################
# Project: MCLCShiny
# File: libraries.R
# Authors: Mari Roberts
# Last date updated: July 19, 2022
# Description:
#    Load packages
#######################################

# download older version of highcharter?
# devtools::install_version("highcharter", "0.7.0")
addResourcePath('highcharter', system.file('htmlwidgets/lib/highcharts', package='highcharter'))

# data visualizations
library(dataui)
library(highcharter)
library(purrr)
library(htmlwidgets)
library(glue)

# shiny
library(shiny)
library(shinyWidgets)
library(dashboardthemes)
library(shinydashboard)

# tables
library(reactable)
library(DT)
library(reactablefmtr)
library(formattable)
library(dplyr)

# maps
library(sp)
