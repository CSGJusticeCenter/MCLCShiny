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

create_data_text <- function(df){
df <- df %>% 
  mutate(text = case_when(
    data == "total_admissions"                            ~  "Total Admissions",
    data == "total_violation_admissions"                  ~  "Supervision Violation Admissions",
    data == "total_probation_violation_admissions"        ~  "Probation Violation Admissions",
    data == "new_offense_probation_violation_admissions"  ~  "Probation New Offense Admisisons",
    data == "technical_probation_violation_admissions"    ~  "Probation Technical Admisisons",
    data == "total_parole_violation_admissions"           ~  "Parole Violation Admissions",
    data == "new_offense_parole_violation_admissions"     ~  "Parole New Offense Admisisons",
    data == "technical_parole_violation_admissions"       ~  "Parole Technical Admisisons",
    
    data == "total_population"                            ~  "Total Population",
    data == "total_violation_population"                  ~  "Supervision Violation Population",
    data == "total_probation_violation_population"        ~  "Probation Violation Population",
    data == "new_offense_probation_violation_population"  ~  "Probation New Offense Population",
    data == "technical_probation_violation_population"    ~  "Probation Technical Population",
    data == "total_parole_violation_population"           ~  "Parole Violation Population",
    data == "new_offense_parole_violation_population"     ~  "Parole New Offense Population",
    data == "technical_parole_violation_population"       ~  "Parole Technical Population"
  ))
}


clean_bjs_prob <- function(df){
  
  df$state <- gsub('/c','',df$state)
  df$state <- gsub('/b','',df$state)
  df$state <- gsub('[[:punct:]]+','',df$state)
  df$state <- gsub('[[:digit:]]+', '', df$state)
  df$state <- gsub('†','',df$state)
  df$state <- gsub('*','',df$state)
  df$state <- trimws(df$state, whitespace = "[\\h\\v]")

  # remove DC
  df <- df %>% filter(state != "District of Columbia" & state != "US total")

  # get indices for alabama and wyoming to subset rows to rows with values
  alabama <- which(df$state == "Alabama")
  wyoming <- which(df$state == "Wyoming")

  # remove NA rows
  df <- df[alabama:wyoming,]
  
}

clean_bjs_parole <- function(df){
  
  df$state <- gsub('/c','',df$state)
  df$state <- gsub('/b','',df$state)
  df$state <- gsub('[[:punct:]]+','',df$state)
  df$state <- gsub('[[:digit:]]+', '', df$state)
  df$state <- gsub('†','',df$state)
  df$state <- gsub('*','',df$state)
  df$state <- trimws(df$state, whitespace = "[\\h\\v]")
  
  # remove DC
  df <- df %>% filter(state != "District of Columbia" & state != "US total")
  
  # get indices for alabama and wyoming to subset rows to rows with values
  alabama <- which(df$state == "Alabama")
  wyoming <- which(df$state == "Wyoming")
  
  # remove NA rows
  df <- df[alabama:wyoming,]
  
}

# create incarcerated variable
incarcerated_bjs_prob <- function(df){
  df[] <- lapply(df, gsub, pattern = ".", replacement = "", fixed = TRUE)
  df[] <- lapply(df, gsub, pattern = ",", replacement = "", fixed = TRUE)
  df$inc_new_sentence <- as.numeric(df$inc_new_sentence)
  df$inc_current_sentence <- as.numeric(df$inc_current_sentence)
  df <- df %>% rowwise() %>% mutate(incarcerated = sum(inc_current_sentence, inc_new_sentence, na.rm = TRUE))
  df <- data.frame(df)
  df <- df %>% select(state, year, type, incarcerated)
}

incarcerated_bjs_parole <- function(df){
  df[] <- lapply(df, gsub, pattern = ".", replacement = "", fixed = TRUE)
  df[] <- lapply(df, gsub, pattern = ",", replacement = "", fixed = TRUE)
  df$inc_new_sentence <- as.numeric(df$inc_new_sentence)
  df$inc_revocation <- as.numeric(df$inc_revocation)
  df <- df %>% rowwise() %>% mutate(incarcerated = sum(inc_revocation, inc_new_sentence, na.rm = TRUE))
  df <- data.frame(df)
  df <- df %>% select(state, year, type, incarcerated)
}

# make parole data long form
bjs_parole_long_form <- function(df){
  
  df <- gather(df, 
               data,
               total,
               incarcerated, 
               factor_key=TRUE)
}

# make probation data long form
bjs_prob_long_form <- function(df){
  
  df <- gather(df, 
               data,
               total,
               incarcerated, 
               factor_key=TRUE)
}

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
