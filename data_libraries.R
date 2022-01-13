# load libraries
library(shiny)
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

#______________________________________________________
#read in R data
#______________________________________________________
load("adm_pop_long.Rda")
load("mclc.Rda")
load("mclc_change.Rda")
load("mclc_datatable.Rda")
load("us_aea2.Rda")
load("df_adm.Rda")
load("df_pop.Rda")
load("df_pop.Rda")
load("spdf_fortified.Rda")
load("centers.Rda")
load("prob_parole.Rda")

par_cols <- c("Parole Entries"="#7B898F",
              "Parole Exits"="#FA9F8D",
              "Parole Population"="#ECE9E9")

prob_cols <- c("Probation Entries"="#7B898F",
               "Probation Exits"="#FA9F8D",
               "Probation Population"="#ECE9E9")

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
  ,boxTitleSize = "16"
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