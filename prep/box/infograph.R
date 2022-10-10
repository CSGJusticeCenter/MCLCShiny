
box::use(
    ./admin
  , png[...]
  , ggplot2[...]
  , stringr[str_to_title]
  , cowplot[plot_grid, ggdraw, draw_label]
  , dplyr[case_when]
  , reshape2[melt]
  , glue[glue]
)


###############
#plotting setup
blankitout <- function(){
  list(
    theme_void(), 
    theme(
      legend.position = "none", 
      aspect.ratio = (453/201) #pixels of humans2.png  
    ) #end theme 
  ) #end list 
}




###########################
################IMAGE setup
# Load png file with human
rawimg <- readPNG(file.path(admin$sp_data_raw, "human/human2.png"))
#make all values binary (0/1)
#use  if/else statements to retain array structure 
# if rawimg == 0 ~ 0; anything else == 1 
img <- ifelse(rawimg == 0, 0, 1)


#######COLORS, can be in plain hex codes 
bg_color    <- "#FFFFFF"
empty_color <- "#A7A9AC" #csg grey

# rri_raw = 1.5
# rri_digits = 2 
# fillcolor = "#0055B8" 
# emptyhumans = TRUE
# emptycolor = "white"
# infogs  = 3 
# fillHoriz = FALSE


#####################################
#create function to call infographics


#' Create single infographic with as-needed number of humans (i.e. if RRI = 1.2
#' use 2 humans only)
#'
#' @param rri_raw input the RRI for the race/ethnicity being plotted
#' @param rri_digits how many digits to round RRI to, default is 2 digits 
#' @param emptyhumans the default will plot empty humans (TRUE). If set to FALSE, no empty humans will plot
#' @param infogs  how many humans to plot - if the value of infogs is too small to plot due to RRI, function corrects the value for plotting - default is 3 humans
#' @param fillHoriz et direction of the color fill for the infographic - default is vertical (FALSE) 
#' @param fillcolor set color of what the RRI color will fill with. Default is CSG Blue. Colors can be set with R color names or HEX values
#'
#' @return
#' @export
#'
#' @examples
create_humans <- function(
    rri_raw, 
    rri_digits = 2, 
    fillcolor = "#0055B8", 
    emptyhumans = TRUE, 
    emptycolor = "white", 
    infogs  = 3, 
    fillHoriz = FALSE
) {
  
  # set colors 
  cols0 <- c(bg_color, emptycolor) 
  cols1 <- c(bg_color, fillcolor)              #full human 
  cols2 <- c(bg_color, empty_color, fillcolor) #not full human
  
  # set RRI
  RRI       <- round(rri_raw, digits = rri_digits) #RRI, rounded to number of digits 
  numfull   <- floor(RRI)    #foor RRI to determine how many filled infographics
  numremain <- RRI - numfull #find partial fill for single infographic
  
  # starting position of blank infographic humans
  blank <- ifelse(numremain != 0, numfull + 2, numfull + 1)
  
  # Find the rows where left arm starts and right arm ends
  if (fillHoriz==FALSE) {
    pos1 <- which(apply(img[,,1], 2, function(y) any(y==1)))
    max  <- 182 #max position must be adjusted due to issues with finding max PNG fill
  } else {
    pos1 <- which(apply(img[,,1], 1, function(y) any(y==1)))
    max  <- 437 #max position must be adjusted due to issues with finding max PNG fill
  }
  h     <- dim(img)[1]
  w     <- dim(img)[2]
  min   <- min(pos1)
  
  # set colors, plots, and RRIs for looping graphics
  finalcolors <- c('cols2',       'cols0', 'cols1')
  finalplots  <- c('plot2',       'plot0', 'plot1')
  finalpcts   <- c(numremain*100, 0,       100)
  
  
  # configure how many plots to create based on user request
  if (RRI>=1) {
    numplots<-1:3
  } else {
    numplots<-1:2
  }
  
  #create three types of plots (not full, empty, full human)
  for (j in numplots) {  
    #percent of interest
    pcts    <- finalpcts[j]
    pospct  <- round((max-min)*pcts/100+min)
    
    # Fill bodies with a different color according to percentages
    finalimg                 <- img[h:1,,1]
    bkgr                     <- (finalimg==1)
    colfill                  <- matrix(rep(FALSE,h*w),nrow=h)
    if (fillHoriz==FALSE) {
      colfill[1:h,max:pospct]  <- TRUE
    } else {
      colfill[max:pospct,1:w]  <- TRUE
    }
    finalimg[bkgr & colfill] <- 0.5
    
    #convert matrix into  df for ggplot
    df <- reshape2::melt(finalimg)
    
    #plot df
    plot <- ggplot(df, aes(x = Var2, y = Var1, fill = factor(value))) +
      geom_raster() +
      scale_fill_manual(values = unlist(mget(finalcolors[j]), use.names=FALSE)) +
      blankitout()
    assign(finalplots[j],plot)
  } # end  for (j in numplots)
  
  
  ## SET UP PLOTTING LIST
  # reate grid of RRIs
  plot_list <- list()
  
  
  if (RRI>1 & numremain != 0) { # multiple humans, 1 partial 
    #print("multiple humans, 1 parital")
    for (i in 1:numfull){ 
      plot_list[[i]] <- plot1 
    }
    plot_list[[numfull+1]] <- plot2 
    
  } else if (RRI>1 & numremain == 0) { #multiple humans, all complete 
    #print("multiple humans, all complete")
    for (i in 1:numfull){ 
      plot_list[[i]] <- plot1 
    }
    
  } else if (RRI == 1){ # 1 human, complete 
    #print("1 human, complete")
    plot_list[[1]] <- plot1 
    
  } else if (RRI < 1) { #1 human, partial 
    #print("1 human, partial")
    plot_list[[1]] <- plot2 
    
  } else {
    stop("RRI and numremain did not meet expected assumptions")
  }
  
  if (emptyhumans == TRUE){
    for (i in blank:infogs){
      plot_list[[i]] <- plot0
    }
  }
  
  #plot the infographics!  
  plot_grid(plotlist=plot_list,nrow=1)
}




#' Create and SAVE full info graphic 
#'
#' @param race 
#' @param rri_raw 
#' @param rri_digits 
#' @param fillcolor 
#' @param infogs 
#' @param state 
#' @param savefile 
#'
#' @return
#' @export
#'
#' @examples
create_infograph <- function(
    race, 
    rri_raw, 
    state, 
    rri_digits = 2, 
    fillcolor = "#0055B8", 
    infogs  = 3, 
    savefile = FALSE 
) {
  
  title.rel <- 0.175
  value.rel <- 1.75
  
  title <- ggdraw() + 
    draw_label(
      glue("{str_to_title(race)} Clients")
      , fontface = "italic"
      , x = 0
      , hjust = 0
      , vjust = 0.25
      , color = "grey50"
      , size = 36
    ) 
  
  value <- ggdraw() + 
    draw_label(
      glue("{round(rri_raw, rri_digits)}x")
      , fontface = "bold"
      , x = 0
      , hjust = 0
      , vjust = 0.5
      , size = 56
    ) 
  
  ggtemp_justpeople <- create_humans(
    rri_raw = rri_raw
    , rri_digits = rri_digits
    , infogs = infogs
    , fillcolor = fillcolor
    , emptyhumans = TRUE
    , emptycolor = "white"
    , fillHoriz = FALSE 
  )
  
  
  ggtemp_wclient <- plot_grid(
    ggtemp_justpeople
    , title
    , ncol = 1
    , rel_heights = c(1, title.rel)
  )
  
  
  fullinfog <- plot_grid(
    value
    , ggtemp_wclient
    , nrow = 1
    , rel_widths = c(value.rel, infogs)
  )
  
  
  
  if (savefile == TRUE){
    
    baseval<- 3
    h.full <- (baseval*(1+title.rel))
    w.full <- (((201/453)*baseval))*(infogs+value.rel)
    
    ggsave(
      file.path(admin$sp_data, "infographs", glue("{state}_{race}.png"))
      , plot = fullinfog
      , height = h.full
      , width = w.full
      , bg = "white"
    )
  }

  return(fullinfog)
  
  
} 





