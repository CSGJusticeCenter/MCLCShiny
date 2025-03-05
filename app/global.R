
# R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"

# PACKAGES #####################################################################


# Dataui for lines in reactable table download instructions
# remotes::install_github("timelyportfolio/dataui")

# Daattali for css shiny loaders
# remotes::install_github("daattali/shinycssloaders")

# Highcharter download instructions:
# remove the existing highcharter package from your R session: remove.packages("highcharter")
# restart your R session
# install highcharter with the devtools package (NOT the remotes package):
# install.packages("devtools")
# devtools::install_github("mrjoh3/highcharter")

# Shiny
# library(shiny)
library(shinycssloaders) # loading screen 
library(shinyWidgets) # inputs 
# library(shinyjs)# show/hide/toggle 
library(shinydashboard) #used to create custom URL's for each tab 
library(shinyBS) # tipify(), create tool tips
library(shinyWidgets)
library(dashboardthemes)
library(shinymeta)

# visualizations 
library(highcharter)
options(highcharter.rjson = FALSE) # for hc_boost(enabled = TRUE)

library(dataui)
library(htmlwidgets)


# Tables
library(reactable)
library(DT)
library(reactablefmtr)
library(formattable)

# general 

# Maps
# library(sp)
# library(ggplot2)

# Guide
# library(conductor)
# library(shinyjs)

box::use(
  dplyr[...], 
  glue[glue],
  purrr[set_names], 
  stringr[str_detect, str_remove_all]
)


# Format #######################################################################

source("colors.R") # unsure if this is needed as hc are pre-generated 

# add fonts to shiny linux server
if (Sys.info()[['sysname']] == 'Linux') {
  dir.create('~/.fonts')
  fonts = c(
    "www/fonts/Graphik.ttf",
    "www/fonts/GraphikBold.ttf"
  )
  file.copy(fonts, "~/.fonts")
  system('fc-cache -f ~/.fonts')
}

# import data ##################################################################

source("dataframes.R")
source("functions.R")


metric_opts <- levels(unique(svii_explorer$metric))
type_opts   <- sort(unique(svii_explorer$type))
# choices = purrr::set_names(value to be used in the back-end of app, names to be displayed in dropdown)
yrchg_opts <- purrr::set_names(
  svii_yr$change_name, 
  mutate(svii_yr, display = glue("{str_yr} - {end_yr}, {end_yr-str_yr} year{ifelse(end_yr-str_yr != 1, 's', '')}"))$display
  )


