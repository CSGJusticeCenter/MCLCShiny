
# R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"

# PACKAGES #####################################################################


# Dataui for lines in reactable table download instructions
# install.packages("remotes")
# remotes::install_github("timelyportfolio/dataui")

# Daattali for css shiny loaders
# install.packages("remotes")
# remotes::install_github("daattali/shinycssloaders")

# Highcharter download instructions:
# remove the existing highcharter package from your R session: remove.packages("highcharter")
# restart your R session
# install highcharter with the devtools package (NOT the remotes package):
# install.packages("devtools")
# devtools::install_github("mrjoh3/highcharter")

library(dataui)
library(highcharter)
options(highcharter.rjson = FALSE) # for hc_boost(enabled = TRUE)
library(purrr)
library(htmlwidgets)
library(glue)

# Shiny
library(shiny)
library(shinyWidgets)
library(dashboardthemes)
library(shinydashboard)
library(shinymeta)
library(shinycssloaders)

# Tables
library(reactable)
library(DT)
library(reactablefmtr)
library(formattable)
library(dplyr)

# Maps
library(sp)
library(ggplot2)

# Guide
library(conductor)
library(shinyBS)
# library(shinyjs)

box::use(
  box/raceethnicity
  , glue[glue]
)

# Fonts ########################################################################

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

# run ui and server code
source("ui.R")
source("server.R")

# launch shiny app
profvis::profvis({
  shinyApp(ui = ui, server = server)
})

