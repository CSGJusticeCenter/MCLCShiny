
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


## set up fonts 
# install.packages("extrafont")
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import(paths = csgjcr::csg_sp_path("50 State Revocations Project/MCLC Shiny App/background/Fonts/Graphik_ttf"), prompt = FALSE)
extrafont::loadfonts(device = "win", quiet = TRUE) 


## set up colors 
bg_color      <- "#FFFFFF"
empty_color   <- "#A7A9AC" #csg grey
mclc_dk_grey  <- "#333333" #"#666666"
mclc_dk_orange<- "#D25E2D"
mclc_dk_blue  <- "#004270" # "#236ca7"
mclc_dk_yellow<- "#D6C246"
mclc_lt_orange<- "#EDB799"
mclc_lt_blue  <- "#C7E8F5"
default_ncols <- 12



##image set up 
#make all values binary (0/1)
#use  if/else statements to retain array structure 
whichimage <- "person-2745706-bw"

if (whichimage == "person-2745706-bw"){
  # black icon, white background 
  px_h <- 521 #height of image in pixels
  px_w <- 323 #width of image in pixels
  ex_h <- 0.005 #expand height by this (affect vertical padding, top/bottom)
  ex_w <- 0.02  #expand width by this (affect horizontal padding, left/right) 
  img_ar_hw <- (px_h*(1+ex_h)) / (px_w*(1+ex_w))
  img_ar_wh <- (px_w*(1+ex_w)) / (px_h*(1+ex_h)) 
  rawimg <- readPNG(file.path(admin$sp_data_raw, glue("human/{whichimage}.png")))
  img <- ifelse(rawimg == 0, 1, 0) #need to invert 
}




## plotting setup
blankitout <- function(){
  list(
    theme_void()
  , scale_x_continuous(expand = expansion(mult = ex_w, add = 0))
  , scale_y_continuous(expand = expansion(mult = ex_h, add = 0))
  , theme(
        legend.position = "none"
      , aspect.ratio = img_ar_hw 
      #, panel.background = element_rect(color = "green")
    ) #end theme 
  ) #end list 
}





#' Create Plot list of empty, full, and partial icons 
#'
#' @param partialval 
#' @param empty   empty   filled icon color (default is white, so they are not shown)
#' @param fill    fully   filled icon color (default is MCLC dark blue)
#' @param partial partial filled icon color (default is MCLC light blue)
#' @param bg  #background color (default is white) 
#' @param fillHoriz 
#'
#' @return
#' @export
#'
#' @examples
icon_options <- function(
    partialval 
  , empty   = "#FFFFFF"
  , fill    = mclc_dk_blue
  , partial = mclc_lt_blue
  , bg = "#FFFFFF"
  , fillHoriz = FALSE
){
  
  if (partialval <0 | partialval >= 1){
    stop("partialvalue must be between 0 and 1")
  }
  
  cols_lst <- list(
      "empty"   = c(bg, empty)
    , "full"    = c(bg, fill)
    , "partial" = c(bg, partial, fill)
  )
  
  pcts_lst <- list(
      "empty"   = 0
    , "full"    = 100
    , "partial" = partialval*100
  )
  
  
  plot_lst <- list(
      "empty"   = NULL
    , "full"    = NULL
    , "partial" = NULL
  )
  
  # Find the rows where left arm starts and right arm ends
  if (fillHoriz==FALSE) {
    pos1 <- which(apply(img[,,1], 2, function(y) any(y==1)))
    max <- max(pos1)
  } else {
    pos1 <- which(apply(img[,,1], 1, function(y) any(y==1)))
    max <- max(pos1)
  }
  h     <- dim(img)[1]
  w     <- dim(img)[2]
  min   <- min(pos1)

  
  #create three types of plots (not full, empty, full human)
  for (j in names(plot_lst)) {  
    #percent of interest
    pcts    <- pcts_lst[[j]]
    pospct  <- round((max-min)*pcts/100+min)
    
    # Fill bodies with a different color according to percentages
    finalimg  <- img[h:1,,1]
    bkgr      <- (finalimg==1)
    colfill   <- matrix(rep(FALSE,h*w),nrow=h)
    if (fillHoriz==FALSE) {
      colfill[1:h,max:pospct]  <- TRUE
    } else {
      colfill[max:pospct,1:w]  <- TRUE
    }
    finalimg[bkgr & colfill] <- 0.5
    
    #convert matrix into df for ggplot
    df <- reshape2::melt(finalimg)
    
    #issue with halfperson, group of 20 rows where value = 0.5, should only be 1 & 2
    if (j == "full"){
      df[df$value == 0.5, ] <- 0
    }
    
    
    #plot df
    plot <- ggplot(df, aes(x = Var2, y = Var1, fill = factor(value))) +
      geom_raster() +
      scale_fill_manual(values = cols_lst[[j]]) +
      blankitout()
    
    plot_lst[[j]] <- plot
    
  } # end  for (j in numplots)
  
  return(plot_lst)
  
  
}


#' Create just the icon part of the infographic

#'
#' @param rri_raw 
#' @param rri_digits 
#' @param fillcolor 
#' @param partialcolor 
#' @param emptyhumans 
#' @param emptycolor 
#' @param infogs 
#' @param fillHoriz 
#'
#' @return
#' @export
#'
#' @examples
create_icons <- function(
    rri_raw 
  , rri_digits = 1 
  , fillcolor    = mclc_dk_blue 
  , partialcolor = mclc_lt_blue
  , emptyhumans = TRUE
  , emptycolor = "white" 
  , infogs  = default_ncols
  , infogs_ncol = default_ncols
  , fillHoriz = FALSE
) {
  
  
  if (infogs-rri_raw<0) {
    infogs<-floor(rri_raw)+1;
    warning(paste0("There are not enough infographics to plot! Number of infographics reset to ",floor(rri_raw)+1))
  }
  
  # set RRI
  RRI       <- round(rri_raw, digits = rri_digits) #RRI, rounded to number of digits 
  #if rounded RRI = 0, change to digit value 
  if (RRI == 0 & rri_raw > 0){
    RRI <- as.numeric(glue("1e-{rri_digits}"))
  }
  numfull   <- floor(RRI)    #floor RRI to determine how many filled infographics
  numremain <- RRI - numfull #find partial fill for single infographic
  
  plot_opts <- icon_options(
      partialval= numremain
    , empty     =  emptycolor
    , fill      =  fillcolor 
    , partial   = partialcolor 
    , fillHoriz = fillHoriz
  )
  
  ## SET UP PLOTTING LIST
  # create grid of RRIs
  plot_list <- list()
  
  
  if (RRI>1 & numremain != 0) { # multiple humans, 1 partial 
    #print("multiple humans, 1 partial")
    for (i in 1:numfull){ 
      plot_list[[i]] <- plot_opts$full 
    }
    plot_list[[numfull+1]] <- plot_opts$partial 
    
  } else if (RRI>1 & numremain == 0) { #multiple humans, all complete 
    #print("multiple humans, all complete")
    for (i in 1:numfull){ 
      plot_list[[i]] <- plot_opts$full 
    }
    
  } else if (RRI == 1){ # 1 human, complete 
    #print("1 human, complete")
    plot_list[[1]] <- plot_opts$full 
    
  } else if (RRI < 1) { # 1 human, partial 
    #print("1 human, partial")
    plot_list[[1]] <- plot_opts$partial 
    
  } else {
    stop("RRI and numremain did not meet expected assumptions")
  }
  
  # add additional empty humans, check to make sure the partial isn't the last person 
  if (emptyhumans == TRUE & length(plot_list) != infogs){
    # starting position of empty infographic humans
    st_empty <- ifelse(numremain != 0, numfull + 2, numfull + 1)
    
    for (i in st_empty:infogs){
      plot_list[[i]] <- plot_opts$empty 
    }
  }
  
  if (infogs>infogs_ncol) {
    rows<-ceiling(rri_raw/infogs_ncol)
  } else {
    rows<-1
  }
  
  #plot the infographics!  
  plot_grid(plotlist=plot_list,nrow=rows)
}





#' Create and SAVE full info graphic 

#' @param rri_raw
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
    rri_raw 
  , data_type #Admissons or Year-End Population 
  , suppress = 0
  , race ="tst"
  , label = "tst"
  , rri_digits = 1 
  , fillcolor = mclc_dk_blue
  , partialcolor = mclc_lt_blue
  , infogs  = default_ncols
  , infogs_ncol = default_ncols
  , savefile = FALSE 
  , returngg = FALSE
) {
  
  
  if (whichimage == "person-2745706-bw"){
    title.rel  <- 0.5
    value.rel  <- 2.25
    title.size <- 30
    value.size <- 110
  }
  
  if (infogs-rri_raw<0) {
    infogs<-floor(rri_raw)+1;
    warning(paste0("There are not enough infographics to plot! Number of infographics reset to ",floor(rri_raw)+1))
  }
  
  
  if (infogs>infogs_ncol) {
    rows<-ceiling(rri_raw/infogs_ncol)
    cols <- infogs_ncol
  } else {
    rows<-1
    cols <- infogs
  }
  
  display_value <- case_when(
    
    #RRI approx. 0
    rri_raw > 0               & round(rri_raw, rri_digits) == 0 ~ as.character(as.numeric(glue("1e-{rri_digits}")))
    
    #RRI > 0, black/hispanic
    , race != admin$lev_RACE[1] & round(rri_raw, rri_digits) > 0 ~ sprintf(glue("%.{rri_digits}f"), round(rri_raw, rri_digits))

  )
  
  graphic_text <- case_when(
    
    #NOT SUPPRESSED
    #RRI>1
      data_type %in% c("A", "Admissions") & as.numeric(display_value) > 1 & suppress == 0 ~ paste0(glue("{str_to_title(race)} people are "), display_value," times more likely to be admitted to prison for a parole revocation than White people.")
    , data_type %in% c("N", "Population") & as.numeric(display_value) > 1 & suppress == 0 ~ paste0(glue("{str_to_title(race)} people are "), display_value," times more likely to be incarcerated for a parole revocation than White people.")
    #RRI<1
    , data_type %in% c("A", "Admissions") & as.numeric(display_value) < 1 & suppress == 0 ~ paste0(glue("{str_to_title(race)} people are "), (1-as.numeric(display_value))*100,"% less likely to be admitted to prison for a parole revocation than White people.")
    , data_type %in% c("N", "Population") & as.numeric(display_value) < 1 & suppress == 0 ~ paste0(glue("{str_to_title(race)} people are "), (1-as.numeric(display_value))*100,"% less likely to be incarcerated for a parole revocation than White people.")
    #RRI=1
    , data_type %in% c("A", "Admissions") & as.numeric(display_value) == 1 & suppress == 0 ~ glue("{str_to_title(race)} people are equally likely to be admitted to prison for a parole revocation as White people.")
    , data_type %in% c("N", "Population") & as.numeric(display_value) == 1 & suppress == 0 ~ glue("{str_to_title(race)} people are equally likely to be incarcerated for a parole revocation as White people.")
    
    #SUPPRESSED
    #RRI>1
    , data_type %in% c("A", "Admissions") & as.numeric(display_value) > 1 & suppress == 1 ~ paste0(glue("{str_to_title(race)} people are "), display_value," times more likely to be admitted to prison for a parole revocation than White people. \nThis estimate should be interpreted with caution because one racial or ethnic group \nincluded in its calculation had fewer than 5 people. See data tables below for details.")
    , data_type %in% c("N", "Population") & as.numeric(display_value) > 1 & suppress == 1 ~ paste0(glue("{str_to_title(race)} people are "), display_value," times more likely to be incarcerated for a parole revocation than White people. \nThis estimate should be interpreted with caution because one racial or ethnic group \nincluded in its calculation had fewer than 5 people. See data tables below for details.")
    #RRI<1
    , data_type %in% c("A", "Admissions") & as.numeric(display_value) < 1 & suppress == 1 ~ paste0(glue("{str_to_title(race)} people are "), (1-as.numeric(display_value))*100,"% less likely to be admitted to prison for a parole revocation than White people. \nThis estimate should be interpreted with caution because one racial or ethnic group \nincluded in its calculation had fewer than 5 people. See data tables below for details.")
    , data_type %in% c("N", "Population") & as.numeric(display_value) < 1 & suppress == 1 ~ paste0(glue("{str_to_title(race)} people are "), (1-as.numeric(display_value))*100,"% less likely to be incarcerated for a parole revocation than White people. \nThis estimate should be interpreted with caution because one racial or ethnic group \nincluded in its calculation had fewer than 5 people. See data tables below for details.")
    #RRI=1
    , data_type %in% c("A", "Admissions") & as.numeric(display_value) == 1 & suppress == 1 ~ glue("{str_to_title(race)} people are equally likely to be admitted to prison for a parole revocation as White people. \nThis estimate should be interpreted with caution because one racial or ethnic group \nincluded in its calculation had fewer than 5 people. See data tables below for details.")
    , data_type %in% c("N", "Population") & as.numeric(display_value) == 1 & suppress == 1 ~ glue("{str_to_title(race)} people are equally likely to be incarcerated for a parole revocation as White people. \nThis estimate should be interpreted with caution because one racial or ethnic group \nincluded in its calculation had fewer than 5 people. See data tables below for details.")

    )
  
  
  title <- ggdraw() + 
    draw_label(
      graphic_text
      , x = 0
      , hjust = 0
      , vjust = 0.5
      , color = mclc_dk_grey
      , size = title.size
      , fontfamily = "Graphik Regular"
    ) #+ theme(panel.background = element_rect(color = "red"))
  
  # display_value <- ifelse(
  #   race == admin$lev_RACE[1]
  #   , sprintf(glue("%.0f"),            round(rri_raw, rri_digits))
  #   , sprintf(glue("%.{rri_digits}f"), round(rri_raw, rri_digits))
  # )
  
  value <- ggdraw() + 
    draw_label(
      display_value
      , x = 0.85
      , hjust = 1
      , vjust = 0.5
      , color = mclc_dk_grey
      , size = value.size
      , fontfamily = "Graphik Medium"
    ) #+ theme(panel.background = element_rect(color = "blue"))
  
  ggtemp_justpeople <- create_icons(
      rri_raw = rri_raw
    , rri_digits = rri_digits
    , infogs  = infogs
    , infogs_ncol = infogs_ncol
    , fillcolor    = fillcolor 
    , partialcolor = partialcolor
    , emptyhumans = TRUE
    , emptycolor = "white" 
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
    
    #ggsave will not save unless specify device = png 
    # since this is within a box module need to specify device = grDevices::png
    ggsave(
        file.path(admin$sp_data, "infographs", paste0(label, ".png"))
      , device = grDevices::png
      , plot = fullinfog
      , height = h.full
      , width = w.full
      , bg = "white"
    )
    admin$mylog(glue("Save image for {label} - {display_value}, h={h.full}, w={w.full}"))
    
  }
  
  if (returngg == TRUE){
    return(fullinfog)
  }

} 


