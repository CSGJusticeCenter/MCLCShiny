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

theme_csgjc_areaplot <- function(){ 
  
  # assign font family up front
  font <- "Arial"   
  
  # replace elements we want to change
  theme_minimal() %+replace%    
    
    theme(
      legend.position = "top", 
      legend.text = element_text(size=16),
      plot.title = element_text(hjust = 0.5, 
                                # face = "bold",
                                size = 18,
                                colour = "#000000"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 16, 
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
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 14, 
                                 colour = "#000000"),
      axis.text.y = element_blank(),
      axis.line.x.bottom = element_line(size = 0.75, 
                                        linetype = "solid", 
                                        colour = "#000000"),
      axis.line.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()     
    )
}
