#######################################
# Project: MCLCShiny
# File: import.R
# Authors: Mari Roberts
# Date: December 8, 2021
# Description: 
#    Defines custom functions
#######################################

##########################
# custom functions
##########################

theme_csgjc_donut_plot <- function(){ 
  
  # assign font family up front
  font <- "Arial"   
  
  # replace elements we want to change
  theme_minimal() %+replace%    
    
    theme(
      legend.position = "none", 
      plot.title = element_text(hjust = 0.5, 
                                # face = "bold",
                                size = 25,
                                colour = "#000000"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      # axis.line.x.bottom = element_line(size = 0.75, 
      #                                   linetype = "solid", 
      #                                   colour = "#000000"),
      axis.line.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()     
    )
}

theme_csgjc_areaplot <- function(){ 
  
  # assign font family up front
  font <- "Arial"   
  
  # replace elements we want to change
  theme_minimal() %+replace%    
    
    theme(
      legend.position = "top", 
      legend.text = element_text(size=14),
      plot.title = element_text(hjust = 0.5, 
                                # face = "bold",
                                size = 16,
                                colour = "#000000"),
      plot.caption = element_text(hjust = 0.5, face = "italic"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 14, 
                                 colour = "#000000"),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()     
    )
}

theme_csgjc_plot_legend <- function(){ 
  
  # assign font family up front
  font <- "Arial"   
  
  # replace elements we want to change
  theme_minimal() %+replace%    
    
    theme(
      legend.position = "top", 
      legend.text = element_text(size=14),
      plot.title = element_text(hjust = 0.5, 
                                # face = "bold",
                                size = 16,
                                colour = "#000000"),
      plot.caption = element_text(hjust = 0.5, face = "italic"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 14, 
                                 colour = "#000000"),
      axis.text.y = element_blank(),
      # axis.line.x.bottom = element_line(size = 0.75, 
      #                                   linetype = "solid", 
      #                                   colour = "#000000"),
      axis.line.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()     
    )
}

theme_csgjc_horizontal_legend <- function(){ 
  
  # assign font family up front
  font <- "Arial"   
  
  # replace elements we want to change
  theme_minimal() %+replace%    
    
    theme(
      legend.direction="horizontal",
      legend.position = "top",
      legend.background = element_blank(),
      legend.title = element_blank(),
      legend.box = "horizontal",
      legend.text = element_text(size=14),
      plot.title = element_text(hjust = 0.5, 
                                # face = "bold",
                                size = 16,
                                colour = "#000000"),
      plot.caption = element_text(hjust = 0.5, face = "italic"),
      axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 14, 
                                 colour = "#000000"),
      axis.text.y =  element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()    
    )
}

theme_csgjc_right_legend <- function(){ 
  
  # assign font family up front
  font <- "Arial"   
  
  # replace elements we want to change
  theme_minimal() %+replace%    
    
    theme(
      legend.position = "top",
      legend.background = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size=14),
      plot.title = element_text(hjust = 0.5, 
                                # face = "bold",
                                size = 16,
                                colour = "#000000"),
      plot.caption = element_text(hjust = 0.5, face = "italic"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 14, 
                                 colour = "#000000"),
      axis.text.y =  element_text(size = 14, 
                                  colour = "#000000"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()    
    )
}

valueBox2 <- function(value, title, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      h3(value),
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}
