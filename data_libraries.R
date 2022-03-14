#______________________________________________________
# load packages
#______________________________________________________

# deployment issues
# install.packages("http://cran.r-project.org/src/contrib/Archive/evaluate/evaluate_0.13.tar.gz", repos=NULL, type="source")
# install.packages("http://cran.r-project.org/src/contrib/Archive/highr/highr_0.8.tar.gz", repos=NULL, type="source")
# install.packages("http://cran.r-project.org/src/contrib/Archive/markdown/markdown_0.9.tar.gz", repos=NULL, type="source")
# install.packages("http://cran.r-project.org/src/contrib/Archive/markdown/scico_1.2.0.tar.gz", repos=NULL, type="source")
library(evaluate)
library(highr)
library(markdown)
library(scico)

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

load(file="mclc_explorer.Rda")

load(file="adm_pop_long.Rda")
load(file="vb_adm_pop.Rda")
load(file="state_table.Rda")
load(file="state_table_wide.Rda")
load(file="parole_table.Rda")
load(file="parole_table_wide.Rda")
load(file="prob_table.Rda")
load(file="prob_table_wide.Rda")

load(file="bjs_prob_parole.Rda")

load(file="us_map.Rda")
load(file="us.Rda")
load(file="centers.Rda")

load(file="bjs_all.Rda")
load(file="bjs.Rda")
load(file="csg.Rda")

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
# custom theme
#______________________________________________________
customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "#2D2D2D"
  ,primaryFontColor = "#0F0F0F"
  ,infoFontColor = "#0F0F0F"
  ,successFontColor = "#0F0F0F"
  ,warningFontColor = "#0F0F0F"
  ,dangerFontColor = "#0F0F0F"
  ,bodyBackColor = "#FFFFFF"
  
  ### header
  ,logoBackColor = "#3C3C3C"
  
  ,headerButtonBackColor = "#3C3C3C"
  ,headerButtonIconColor = "#FFFFFF"
  ,headerButtonBackColorHover = "#DCDCDC"
  ,headerButtonIconColorHover = "#3C3C3C"
  
  ,headerBackColor = "#3C3C3C"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "#3C3C3C"
  ,sidebarPadding = "0"
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#737373"
  
  ,sidebarSearchBackColor = "#FFFFFF"
  ,sidebarSearchIconColor = "#646464"
  ,sidebarSearchBorderColor = "#DCDCDC"
  
  ,sidebarTabTextColor = "#FFFFFF"
  ,sidebarTabTextSize = "14"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "0"
  
  ,sidebarTabBackColorSelected = "#E6E6E6"
  ,sidebarTabTextColorSelected = "#2D2D2D"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "#F5F5F5"
  ,sidebarTabTextColorHover = "#2D2D2D"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#C8C8C8"
  ,sidebarTabBorderWidthHover = "4"
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "#FFFFFF"
  ,boxBorderRadius = "3"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "14"
  ,boxDefaultColor = "#2D2D2D"
  ,boxPrimaryColor = "#5F9BD5"
  ,boxInfoColor = "#C8C8C8"
  ,boxSuccessColor = "#70AD47"
  ,boxWarningColor = "#2D2D2D"
  ,boxDangerColor = "#2D2D2D"
  
  ,tabBoxTabColor = "#F8F8F8"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#646464"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#FFFFFF"
  ,tabBoxHighlightColor = "#C8C8C8"
  ,tabBoxBorderRadius = "2"
  
  ### inputs
  ,buttonBackColor = "#D7D7D7"
  ,buttonTextColor = "#2D2D2D"
  ,buttonBorderColor = "#969696"
  ,buttonBorderRadius = "5"
  
  ,buttonBackColorHover = "#BEBEBE"
  ,buttonTextColorHover = "#000000"
  ,buttonBorderColorHover = "#969696"
  
  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#767676"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#F5F5F5"
  ,textboxBorderColorSelect = "#6C6C6C"
  
  ### tables
  ,tableBackColor = "#F8F8F8"
  ,tableBorderColor = "#EEEEEE"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)
