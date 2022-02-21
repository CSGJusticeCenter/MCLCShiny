#______________________________________________________
# load packages
#______________________________________________________
library(shinyWidgets)
library(RColorBrewer)
library(classInt)
library(plotly)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(shinythemes)
library(ggiraph)
library(shinydashboard)
library(leaflet)
library(DT)
library(tidyverse)
library(geojsonio)
library(rgdal)
library(broom)
library(rgeos)
library(viridis)
library(readxl)
library(janitor)
library(dashboardthemes)
library(labelled)
library(reactable)
library(shiny)
library(scico)
library(leaflegend)
library(mapproj)

#______________________________________________________
# read in R data
#______________________________________________________
load("adm_pop_long.Rda")
load("mclc.Rda")
load("mclc_change.Rda")
load("mclc_datatable.Rda")
load("mclc_explorer.Rda")
load("df_adm.Rda")
load("df_pop.Rda")
load("df_pop.Rda")
load("us.Rda")
load("us_map.Rda")
load("centers.Rda")
load("df_prob_parole.Rda")
load("csg.Rda")
load("bjs.Rda")

#______________________________________________________
# colors
#______________________________________________________

change_colors = c("#264d59", "#43978d", "#f9e07f", "#f9ad6a", "#d46c4e")
count_colors = c("#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#08589e")

# assign colors for visualizations
# blue2  <- "#9ed4ef"
# blue3  <- "#71cfee"
# blue4  <- "#007392"
# blue5  <- "#00475d"
# red    <- "#B05D24"
# green3 <- "#7fc241"
# green4 <- "#5c922f"
# green5 <- "#315c15"
# yellow <- "#f0de0b"
# orange <- "#f89c1b"

# blue1 <- "#DEF0F6"
# blue3 <- "#355DA1"
# orange <- "#f89c1b"
# red <- "#B05D24"
# yellow <- "#F5CB5C"
# drab <- "#3C362A"

lightorange <- "#FFBB78"
orange <- "#FF7F0E"
lightblue <- "#AEC7E8"
darkblue <- "#1F77B4"

total_co <- lightorange
viol_co <- orange
tech_co <-  lightblue
new_o_co <- darkblue

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