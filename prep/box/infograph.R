
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


# install.packages("extrafont")
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# 
# extrafont::font_import(
#   paths = csgjcr::csg_sp_path("50 State Revocations Project/MCLC Shiny App/background/Fonts/Graphik_ttf")
#   , prompt = FALSE
# )

extrafont::loadfonts(device = "win", quiet = TRUE) 


gcolors <- c("#666666", "#D25E2D", "#236ca7", "#D6C246", "#EDB799", "#C7E8F5")


###########################
################IMAGE setup
# Load png file with human
#make all values binary (0/1)
#use  if/else statements to retain array structure 


whichimage <- "person-2745706-bw"

if (whichimage == "person-2745706-bw"){
  # black icon, white background 
  img_ar_hw <- (521/323)
  img_ar_wh <- (323/521)
  rawimg <- readPNG(file.path(admin$sp_data_raw, glue("human/{whichimage}.png")))
  img <- ifelse(rawimg == 0, 1, 0) #need to invert 
}

if (whichimage == "human2"){
  #white icon, black background 
  img_ar_hw <- (453/201)
  img_ar_wh <- (201/453)
  rawimg <- readPNG(file.path(admin$sp_data_raw, glue("human/{whichimage}.png")))
  img <- ifelse(rawimg == 0, 0, 1)
}






#######COLORS, can be in plain hex codes 
bg_color    <- "#FFFFFF"
empty_color <- "#A7A9AC" #csg grey


###############
#plotting setup
blankitout <- function(){
  list(
    theme_void(), 
    theme(
      legend.position = "none", 
      aspect.ratio = img_ar_hw, #pixels of humans2.png  
      panel.background = element_rect(color = "green")
    ) #end theme 
  ) #end list 
}




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
    rri_digits = 1, 
    fillcolor = "#0055B8", 
    emptyhumans = TRUE, 
    emptyhumanscolor = "white", 
    infogs  = 3, 
    fillHoriz = FALSE
) {
  
  if (infogs-rri_raw<0) {
    infogs<-floor(rri_raw)+1;
    warning(paste0("There are not enough infographics to plot! Number of infographics reset to ",floor(rri_raw)+1))
  }
  
  # set colors 
  cols0 <- c(bg_color, emptyhumanscolor)        #empty human  
  cols1 <- c(bg_color, fillcolor)              #full human 
  cols2 <- c(bg_color, empty_color, fillcolor) #partial human
  
  # set RRI
  RRI       <- round(rri_raw, digits = rri_digits) #RRI, rounded to number of digits 
  numfull   <- floor(RRI)    #foor RRI to determine how many filled infographics
  numremain <- RRI - numfull #find partial fill for single infographic
  
  # starting position of blank infographic humans
  blank <- ifelse(numremain != 0, numfull + 2, numfull + 1)
  
  # Find the rows where left arm starts and right arm ends
  if (fillHoriz==FALSE) {
    pos1 <- which(apply(img[,,1], 2, function(y) any(y==1)))
    #max  <- 182 #max position must be adjusted due to issues with finding max PNG fill
    max <- max(pos1)
  } else {
    pos1 <- which(apply(img[,,1], 1, function(y) any(y==1)))
    #max  <- 437 #max position must be adjusted due to issues with finding max PNG fill
    max <- max(pos1)
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
    
    ### issue with halfperson, group of 20 rows where value = 0.5, should only be 1 & 2
    if (finalplots[j] == "plot1"){
      df[df$value == 0.5, ] <- 0
    }
    
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
  
  if (infogs>10) {
    rows<-2
  } else {
    rows<-1
  }
  
  #plot the infographics!  
  plot_grid(plotlist=plot_list,nrow=rows)
}



### FOR TESTING 

rri_raw = 1.5
race = "Hispanic"
state = "tst"
rri_digits = 1
fillcolor = "#236ca7"
infogs  = 4
savefile = TRUE
returngg = FALSE


#' Create and SAVE full info graphic 

#' @param rri_raw
#'
#' @param race 
#' @param rri_digits 
#' @param fillcolor 
#' @param infogs 
#' @param state 
#' @param savefile 
#' @param returngg 
#'
#' @return
#' @export
#'
#' @examples
create_infograph <- function(
    rri_raw, 
    race = "tst",
    state = "tst", 
    rri_digits = 1, 
    fillcolor = "#0055B8", 
    infogs  = 4, 
    savefile = FALSE, 
    returngg = FALSE
) {
  
  if (whichimage == "person-2745706-bw"){
    title.rel <- 0.4
    value.rel <- 2.5
    title.size <- 60
    value.size <- 110
  }
  
  
  if (whichimage == "human2"){
    title.rel <- 0.175
    value.rel <- 1.75
    title.size <- 36
    value.size <- 56
  }
  
  
  if (infogs>10) {
    rows<-ceiling(rri_raw/10)
    cols <- 10
  } else {
    rows<-1
    cols <- infogs
  }
  
  
  title <- ggdraw() + 
    draw_label(
      glue("{str_to_title(race)} Clients")
      , x = 0
      , hjust = 0
      , vjust = 0.25
      , color = "#666666"
      , size = title.size
      , fontfamily = "Graphik Regular"
    ) + 
    theme(panel.background = element_rect(color = "red"))
  
  display_value <- ifelse(
    race == admin$lev_RACE[1]
    , sprintf(glue("%.0fx"),            round(rri_raw, rri_digits))
    , sprintf(glue("%.{rri_digits}fx"), round(rri_raw, rri_digits))
  )
  
  value <- ggdraw() + 
    draw_label(
      display_value
      , x = 0.9
      , hjust = 1
      , vjust = 0.5
      , color = "#666666"
      , size = value.size
      , fontfamily = "Graphik Medium"
    ) + 
    theme(
      panel.background = element_rect(color = "blue")
    )
  
  ggtemp_justpeople <- create_humans(
      rri_raw = rri_raw
    , rri_digits = rri_digits
    , infogs = infogs
    , fillcolor = fillcolor
    , emptyhumans = TRUE
    , emptyhumanscolor = "white"
    , fillHoriz = FALSE 
  )
  
  
  ggtemp_wclient <- plot_grid(
    ggtemp_justpeople
    , title
    , ncol = 1
    , rel_heights = c(1, title.rel/rows)
  )
  
  
  fullinfog <- plot_grid(
    value
    , ggtemp_wclient
    , nrow = 1
    , rel_widths = c(value.rel, cols)
  )
  
  
  
  if (savefile == TRUE){
    
    
    baseval<- 3
    h.full <- ((baseval*rows)*(1+title.rel))
    w.full <- ((img_ar_wh*baseval))*(cols+value.rel)
    
    #ggsave will not save fonts unless uninstall {ragg}
    ggsave(
      file.path(admin$sp_data, "infographs", glue("{state}_{race}.png"))
      , plot = fullinfog
      , height = h.full
      , width = w.full
      , bg = "white"
    )
    file.show(file.path(admin$sp_data, "infographs", glue("{state}_{race}.png")))
    admin$mylog(glue("Save image for {state} {race} - {display_value}, h={h.full}, w={w.full}"))
    
  }
  
  if (returngg == TRUE){
    return(fullinfog)
  }

  
  
  
} 





