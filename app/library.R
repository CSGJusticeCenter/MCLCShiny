#######################################
# Project: MCLCShiny
# File: libraries.R
# Authors: Mari Roberts
# Last date updated: July 20, 2022
# Description:
#    Load packages
#######################################

# Data visualizations
# remotes::install_github("timelyportfolio/dataui")
library(dataui)
library(highcharter)
library(purrr)
library(htmlwidgets)
library(glue)

# Shiny
library(shiny)
library(shinyWidgets)
library(dashboardthemes)
library(shinydashboard)

# Tables
library(reactable)
library(DT)
library(reactablefmtr)
library(formattable)
library(dplyr)

# Maps
library(sp)
library(ggplot2)

# Save highchart
# webshot removes font styles when saving so webshot2 is better
# remotes::install_github("rstudio/webshot2")
library(webshot2)

# Guide
library(conductor)

box::use(
    box/raceethnicity
  , glue[glue]
)

# load specific functions instead of entire packages - not working
# library(box)
# box::use(
#
#   dataui[dui_sparkline],
#   dataui[dui_sparklineseries],
#   dataui[dui_sparkpatternlines],
#
#   dplyr[arrange],
#   dplyr[case_when],
#   dplyr[filter],
#   dplyr[mutate],
#   dplyr[rename],
#   dplyr[rowwise],
#   dplyr[select],
#
#   DT[datatable],
#   DT[dataTableOutput],
#   DT[formatCurrency],
#   DT[formatPercentage],
#   DT[formatRound],
#   DT[JS],
#   DT[renderDataTable],
#   DT[saveWidget],
#
#   formattable[comma],
#   formattable[style],
#
#   highcharter[color_stops],
#   highcharter[hc_add_dependency],
#   highcharter[hc_add_series],
#   highcharter[hc_add_series_map],
#   highcharter[hc_add_theme],
#   highcharter[hc_chart],
#   highcharter[hc_colorAxis],
#   highcharter[hc_exporting],
#   highcharter[hc_plotOptions],
#   highcharter[hc_theme],
#   highcharter[hc_theme_merge],
#   highcharter[hc_theme_smpl],
#   highcharter[hc_title],
#   highcharter[hc_tooltip],
#   highcharter[hc_xAxis],
#   highcharter[hc_yAxis],
#   highcharter[hcaes],
#   highcharter[highchart],
#   highcharter[highchartOutput],
#   highcharter[JS],
#   highcharter[renderHighchart],
#
#   htmlwidgets[JS],
#   htmlwidgets[saveWidget],
#
#   reactable[colDef],
#   reactable[colFormat],
#   reactable[reactable],
#   reactable[reactableOutput],
#   reactable[reactableTheme],
#   reactable[renderReactable],
#
#   scales[comma],
#
#   shiny[a],
#   shiny[br],
#   shiny[column],
#   shiny[dataTableOutput],
#   shiny[div],
#   shiny[downloadButton],
#   shiny[downloadHandler],
#   shiny[fluidPage],
#   shiny[fluidRow],
#   shiny[h1],
#   shiny[HTML],
#   shiny[includeCSS],
#   shiny[navbarPage],
#   shiny[observe],
#   shiny[observeEvent],
#   shiny[p],
#   shiny[reactive],
#   shiny[renderDataTable],
#   shiny[renderText],
#   shiny[selectizeInput],
#   shiny[shinyApp],
#   shiny[span],
#   shiny[tabPanel],
#   shiny[tabsetPanel],
#   shiny[tagList],
#   shiny[textOutput],
#   shiny[updateQueryString],
#
#   shinydashboard[renderValueBox],
#   shinydashboard[updateTabItems],
#   shinydashboard[valueBoxOutput],
#
#   stats[filter],
#
#   tidyr[gather],
#
#   utils[head],
#   utils[write.csv]
# )
