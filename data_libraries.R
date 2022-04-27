#______________________________________________________
# load packages
#______________________________________________________

# deployment issues
library(evaluate)
library(highr)
library(markdown)
library(scico)

# highcharter
library(tidyverse)
library(highcharter)
library(scales)
library(gapminder)
library(tidycensus)
library(tidyverse)

# shiny
library(shiny)
library(shinyWidgets)
library(dashboardthemes)
library(shinydashboard)

# data manipulation
library(readxl)
library(dplyr)
library(tidyverse)
library(janitor)
library(openxlsx)
library(writexl)
library(formattable)

# tables
library(reactable)
library(DT)

# maps
library(leaflet)
library(leaflegend)
library(mapproj)
library(geojsonio)
library(rgdal)
library(rgeos)
library(viridis)
library(sp)
library(webshot)
library(sf)
library(geojsonsf)
library(jsonlite)

# data visualizations
library(dataui)
library(plotly)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(scico)
library(ggiraph)

# don't need?
# library(classInt)
# library(broom)
# library(labelled)

#______________________________________________________
# read in R data
#______________________________________________________

load(file="Data/mclc_explorer.Rda")
load(file="Data/mclc_explorer_table.Rda")

load(file="Data/adm_pop_long.Rda")
load(file="Data/vb_adm_pop.Rda")
load(file="Data/state_table.Rda")
load(file="Data/state_table_wide.Rda")
load(file="Data/parole_table.Rda")
load(file="Data/parole_table_wide.Rda")
load(file="Data/prob_table.Rda")
load(file="Data/prob_table_wide.Rda")

load(file="Data/us_map.Rda")
load(file="Data/us.Rda")
load(file="Data/centers.Rda")
load(file="Data/combined.Rda")
load(file="Data/combined_labels.Rda")

load(file="Data/bjs_prob_parole.Rda")
load(file="Data/bjs_bubble.Rda")
load(file="Data/bjs.Rda")
load(file="Data/csg.Rda")

#______________________________________________________
# colors
#______________________________________________________

# assign colors for visualizations
lightorange <- "#fcccac"
orange      <- "#fc9c54"
lightblue   <- "#9cccec"
darkblue    <- "#2c6c9c"
regblue     <- "#3c97da"
brown       <- "#b26e39"
gray        <- "#dcdcdc"
lightgreen  <- "#a8ddb5"

# assign colors to data types
total_co <- lightorange
viol_co  <- orange
tech_co  <- regblue
new_o_co <- darkblue
pp_co    <- lightblue
bjs_co   <- lightgreen

count_colors  <- c("#d1f4ff", lightblue, regblue, darkblue, "#2a6a99")
change_colors <- c("#af4d03", orange, lightorange, lightblue, regblue, darkblue)

#______________________________________________________
# fonts
#______________________________________________________

default_fonts <- c("system-ui", "-apple-system", "Segoe UI", "Roboto",
                   "Helvetica Neue", "Arial", "Noto Sans", "Liberation Sans",
                   "sans-serif", "Apple Color Emoji", "Segoe UI Emoji",
                   "Segoe UI Symbol", "Noto Color Emoji")

