
box::use(
    ./admin
  , png[...]
  , ggplot2[...]
  , stringr[str_to_title, str_detect, str_wrap, str_sub, str_locate, str_replace, str_replace_all]
  , cowplot[plot_grid, ggdraw, draw_label]
  , dplyr[case_when]
  , reshape2[melt]
  , glue[glue]
)


## set up fonts
csgjcr::csg_font_hoist("Graphik")

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



#' Short-hand code for what text to display
#'
#' @param whichNCRP Admissions or Population NCRP data set
#' @param RRI Rounded rRRI value 
#' @param SUPPRESS 0 or 1, whether the data is suppressed due to low counts
#' @param whichPOP denominator population either BJS or CEN 
#'
#' @export
title_text_type <- function(whichNCRP, rRRI, SUPPRESS, whichPOP){
  paste(
    c(
      whichNCRP, 
      case_when(
        rRRI > 1 ~ "gt1", 
        rRRI < 1 ~ "lt1", 
        rRRI ==1 ~ "eq1"
      ), 
      SUPPRESS, 
      whichPOP
    ), 
    collapse = "_"
  )
}


#' Create the title in-graphic text for infogrpahic 
#'
#' @param titleTextType 
#' @param race 
#' @param display_value 
#'
#' @return
#' @export
#'
title_text_create <- function(titleTextType, race, display_value){

  n_value <- as.numeric(display_value) 
  
  these_people_text <- case_when(
    str_detect(titleTextType, "CEN") == TRUE ~ paste(str_to_title(race), "people are"), 
    str_detect(titleTextType, "BJS") == TRUE ~ paste(str_to_title(race), "people on parole are")
  )
  
  compare_val_text <- case_when(
    str_detect(titleTextType, "gt1") == TRUE ~ paste0(display_value, " times more likely"), 
    str_detect(titleTextType, "lt1") == TRUE ~ paste0((1-n_value)*100, "% less likely"), 
    str_detect(titleTextType, "eq1") == TRUE ~ "equally likely"
  )
  
  preposition <- case_when(
    str_detect(titleTextType, "gt1") == TRUE ~ "than",
    str_detect(titleTextType, "lt1") == TRUE ~ "than",
    str_detect(titleTextType, "eq1") == TRUE ~ "as"
  )
  
  situation_text <- case_when(
    str_detect(titleTextType, "Admissions") == TRUE ~ "to be admitted to prison", 
    str_detect(titleTextType, "Population") == TRUE ~ "to be incarcerated" 
  ) |> paste("for a parole revocation", preposition, "White people.")
  
  
  suppress_text <- case_when(
    str_detect(titleTextType, "_0") == TRUE ~ "", 
    str_detect(titleTextType, "_1") == TRUE ~ paste(
        "This estimate should be interpreted with caution because one",
        "racial or ethnic group included in its calculation had fewer", 
        "than 5 people. See data tables below for details."
    )
  )
  
  full_text <- paste(these_people_text, compare_val_text, situation_text, suppress_text)  ## str_replace_all(full_text, c(`White `="White \n", `group `="group \n"))
  
  case_when(
    ##TOTAL DISPARITIES
    #################################################################################################################### num of infogrpahs| race groups  | test infograph with:  
      titleTextType == "Admissions_gt1_0_CEN" ~ str_replace(full_text, "admitted to", "admitted \nto")                 # n infographs: 59 | black & hisp | AZ (loop #6) & MN (loop #46)
    , titleTextType == "Admissions_lt1_0_CEN" ~ str_replace(full_text, "admitted to", "admitted \nto")                 # n infographs: 15 | hisp only    | AR (loop #8) 
    , titleTextType == "Admissions_eq1_0_CEN" ~ str_replace(full_text, "admitted to", "admitted \nto")                 # n infographs:  1 | hisp only    | TX (loop #86) 
    , titleTextType == "Population_gt1_0_CEN" ~ str_replace(full_text, "incarcerated for", "incarcerated \nfor")       # n infographs: 57 | black & hisp | AZ (loop #106) & CA (loop #110)
    , titleTextType == "Population_lt1_0_CEN" ~ str_replace(full_text, "incarcerated for", "incarcerated \nfor")       # n infographs: 14 | hisp only    | AR (loop #108) 
    , titleTextType == "Population_eq1_0_CEN" ~ str_replace(full_text, "incarcerated for", "incarcerated \nfor")       # n infographs:  3 | hisp only    | DE (loop #116) 
    , titleTextType == "Admissions_gt1_1_CEN" ~ str_replace_all(full_text, c( White  = "\nWhite",  group  = "\ngroup"))# n infographs:  2 | black only   | ME (loop #38) & WV (loop #96) 
    , titleTextType == "Population_gt1_1_CEN" ~ str_replace_all(full_text, c( White  = "\nWhite",  group  = "\ngroup"))# n infographs:  2 | black & hisp | ND-hisp (loop #168) & WV-black (loop #196) 
    , titleTextType == "Admissions_lt1_1_CEN" ~ str_replace_all(full_text, c( White  = "\nWhite",  group  = "\ngroup"))# n infographs:  3 | hisp only    | MS (loop #48)
    , titleTextType == "Population_lt1_1_CEN" ~ str_replace_all(full_text, c( White  = "\nWhite",  group  = "\ngroup"))# n infographs:  2 | hisp only    | MS (loop #148)
    , titleTextType == "Admissions_eq1_1_CEN" ~ str_replace_all(full_text, c( White  = "\nWhite",  group  = "\ngroup"))# NO INFOGRAPHICS USE THIS OPTION 
    , titleTextType == "Population_eq1_1_CEN" ~ str_replace_all(full_text, c( White  = "\nWhite",  group  = "\ngroup"))# NO INFOGRAPHICS USE THIS OPTION 
    
    , titleTextType == "Admissions_gt1_0_BJS" ~ str_replace(full_text, "admitted to", "admitted \nto")                 # n infographs: 26 | black & hisp | CA (loop #9)
    , titleTextType == "Admissions_lt1_0_BJS" ~ str_replace(full_text, "admitted to", "admitted \nto")                 # n infographs: 35 | black & hisp | GA (loop #19) 
    , titleTextType == "Admissions_eq1_0_BJS" ~ str_replace(full_text, "admitted to", "admitted \nto")                 # n infographs: 11 | black & hisp | AZ-hisp (loop #5) & AR-black (loop #7)
    , titleTextType == "Population_gt1_0_BJS" ~ str_replace(full_text, "incarcerated for", "incarcerated \nfor")       # n infographs: 35 | black & hisp | AZ (loop #105)
    , titleTextType == "Population_lt1_0_BJS" ~ str_replace(full_text, "incarcerated for", "incarcerated \nfor")       # n infographs: 22 | black & hisp | KY (loop #133)
    , titleTextType == "Population_eq1_0_BJS" ~ str_replace(full_text, "incarcerated for", "incarcerated \nfor")       # n infographs: 12 | black & hisp | TX (loop #185)
    , titleTextType == "Admissions_gt1_1_BJS" ~ str_replace_all(full_text, c(`than W`= "\nthan W", group  = "\ngroup"))# n infographs:  1 | hisp only    | WV-hisp (loop #95)
    , titleTextType == "Admissions_lt1_1_BJS" ~ str_replace_all(full_text, c(`than W`= "\nthan W", group  = "\ngroup"))# n infographs:  1 | hisp only    | ID-hisp (loop #23) 
    , titleTextType == "Admissions_eq1_1_BJS" ~ str_replace_all(full_text, c(`than W`= "\nthan W", group  = "\ngroup"))# NO INFOGRAPHICS USE THIS OPTION 
    , titleTextType == "Population_gt1_1_BJS" ~ str_replace_all(full_text, c( White  = "\nWhite",  group  = "\ngroup"))# n infographs:  1 | black only   | WV-black (loop #195)
    , titleTextType == "Population_lt1_1_BJS" ~ str_replace_all(full_text, c( White  = "\nWhite",  group  = "\ngroup"))# n infographs:  2 | hisp only    | MS-hisp (loop #147)
    , titleTextType == "Population_eq1_1_BJS" ~ str_replace_all(full_text, c( White  = "\nWhite",  group  = "\ngroup"))# NO INFOGRAPHICS USE THIS OPTION 
  )

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
  , data_type #Admissions or Year-End Population
  , pop_type #CEN or BJS
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
    title.rel  <- 0.7
    value.rel  <- 1.8
    title.size <- 38
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
  
  titleTextType <- title_text_type(data_type, as.numeric(display_value), suppress, pop_type)
  
  graphic_text <- title_text_create(titleTextType, race, display_value)
  
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
  
  subtitle <- ggdraw() +
    draw_label(
      "* Hispanic RRI should be interpreted with caution due to inconsistencies in how each state collects and reports data on ethnicity"
      , x = 0
      , hjust = 0
      , vjust = 0.5
      , color = mclc_dk_grey
      , size = title.size*0.75
      , fontfamily = "Graphik Light Italic"
    ) #+ theme(panel.background = element_rect(color = "green"))
  
  value <- ggdraw() +
    draw_label(
      display_value
      , x = 0.85
      , hjust = 1
      , vjust = 0.3
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
  
  
  maininfo <- plot_grid(
    value
    , ggtemp_wclient
    , nrow = 1
    , rel_widths = c(value.rel, cols)
  )
  
  if (race == "Hispanic"){
    fullinfog <- plot_grid(
      maininfo
      , plot_grid(ggplot() + theme_void(), subtitle, nrow = 1, rel_widths = c(value.rel, cols))
      , ncol = 1
      , rel_heights = c(1 + title.rel/rows, 0.2)
    )
  } else {
    
    fullinfog <- maininfo 
  }


  if (savefile == TRUE){

    baseval<- 3
    h.full <- ((baseval*rows)*(1+title.rel)) + ifelse(race == "Hispanic", 0.6, 0)
    w.full <- ((img_ar_wh*baseval))*(cols+value.rel+1)
    

    #ggsave will not save unless specify device = png
    # since this is within a box module need to specify device = grDevices::png
    ggsave(
        file.path(admin$sp_data, "infographs", paste0(label, ".png"))
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


