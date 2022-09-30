#####################################
#create function to call infographics
#function does not currently account for RRI of 1!!!!!
#setrri = input the RRI for the race/ethnicity being plotted
#infogs = how many humans to plot - if the value of infogs is too small to plot due to RRI, function corrects the value for plotting - default is 9 humans
#emptyhumans = the default will plot empty humans (TRUE). If set to FALSE, no empty humans will plot
#fillcolor = set color of what the RRI color will fill with. Default is CSG Blue. Colors can be set with R color names or HEX values
create_infograph <- function(setrri,infogs=9,emptyhumans=TRUE,fillcolor="#0055B8") {
  
  #######COLORS
  #not full human
  cols3 <- c(rgb(255,255,255,maxColorValue = 255), #white
             rgb(167,169,172,maxColorValue = 255), #CSGJC gray
             rgb(col2rgb(fillcolor)[1],col2rgb(fillcolor)[2],col2rgb(fillcolor)[3],
                 maxColorValue = 255))    #CSGJC blue
  #full human
  cols2 <- c(rgb(255,255,255,maxColorValue = 255), 
             rgb(col2rgb(fillcolor)[1],col2rgb(fillcolor)[2],col2rgb(fillcolor)[3],
                 maxColorValue = 255))
  
  #########set RRI
  RRI       <- setrri        #RRI
  numfull   <- floor(RRI)    #round RRI to determine how many filled infographics
  numremain <- RRI - numfull #find partial fill for single infographic
  
  #########set number of rows to plot infographics
  if (infogs-setrri<1) {
    infogs<-floor(setrri)+2;
    warning(paste0("There are not enough infographics to plot! Number of infographics reset to ",floor(setrri)+2))}
  if (infogs>=10) {rows<-2} else {rows<-1}
  
  #########starting position of blank infographic humans
  blank <- numfull + 2     
  
  ############NOT FULL HUMAN 
  #percent of interest
  pcts2 <- numremain*100
  h     <- dim(img)[1]
  w     <- dim(img)[2]
  
  # Find the rows where feet starts and head ends
  pos1    <- which(apply(img[,,1], 2, function(y) any(y==1)))
  min     <- min(pos1)
  max     <- 182 #max position must be manually set due to issues with finding top of PNG fill
  pospct  <- round((max-min)*pcts2/100+min)
  
  # Fill bodies with a different color according to percentages
  finalimg                 <- img[h:1,,1]
  bkgr                     <- (finalimg==1)
  colfill                  <- matrix(rep(FALSE,h*w),nrow=h)
  colfill[1:h,max:pospct]  <- TRUE
  finalimg[bkgr & colfill] <- 0.5
  
  #convert matrix into  df for ggplot
  df2 <- reshape2::melt(finalimg)
  
  #plot df2
  plot2<-ggplot(df2, aes(x = Var2, y = Var1, fill = factor(value)))+
    geom_tile() +
    scale_fill_manual(values = cols3) +
    blankitout
  
  ############EMPTY HUMAN 
  #percent of interest
  pcts0 <- 0
  
  # Find the rows where feet starts and head ends
  pospct <- round((max-min)*pcts0/100+min)
  
  # Fill bodies with a different color according to percentages
  finalimg                  <- img[h:1,,1]
  bkgr                      <- (finalimg==1)
  colfill                   <- matrix(rep(FALSE,h*w),nrow=h)
  colfill[1:h,max:pospct]   <- TRUE
  finalimg[bkgr & colfill]  <- 0.5
  
  #convert matrix into  df for ggplot
  df0 <- reshape2::melt(finalimg)
  
  #plot df0
  plot0<-ggplot(df0, aes(x = Var2, y = Var1, fill = factor(value)))+
    geom_tile() +
    scale_fill_manual(values = cols0) +
    blankitout
  
  ############FULL HUMAN
  #percent of interest
  pcts <- 100
  
  # Find the rows where feet starts and head ends
  pospct <- round((max-min)*pcts/100+min)
  
  # Fill bodies with a different color according to percentages
  finalimg                 <- img[h:1,,1]
  bkgr                     <- (finalimg==1)
  colfill                  <- matrix(rep(FALSE,h*w),nrow=h)
  colfill[1:h,max:pospct]  <- TRUE
  finalimg[bkgr & colfill] <- 0.5
  
  #convert matrix into  df for ggplot
  df1 <- reshape2::melt(finalimg) 
  
  #plot df1
  plot1<-ggplot(df1, aes(x = Var2, y = Var1, fill = factor(value)))+
    geom_tile() +
    scale_fill_manual(values = cols2) +
    blankitout
  
  ############create grid of RRIs
  plot_list <- list()
  
  ############SET UP PLOTTING LIST
  #plot empty humans
  if (emptyhumans==TRUE) {
    
    #for RRI>1, create full human(s), not full human, empty human(s)
    if (RRI>1) {
      
      #create initial list of filled in infographics
      for (i in 1:numfull){
        #RRI>1, full human
        plot_list[[i]] <- plot1
      }
      #RRI>1, not full human
      plot_list[[numfull+1]] <- plot2
      
      #RRI>1, empty human
      for (i in blank:infogs){
        plot_list[[i]] <- plot0
      }
      
      #for RRI<1, not full human, empty humans
    } else {
      plot_list[[1]] <- plot2
      
      for (i in blank:infogs){
        plot_list[[i]] <- plot0
      }  
    }
    
    #otherwise, DO NOT plot empty humans  
  } else{
    
    #for RRI>1, create full human(s), not full human
    if (RRI>1) {
      
      #create initial list of filled in infographics
      for (i in 1:numfull){
        #RRI>1, full human
        plot_list[[i]] <- plot1
      }
      #RRI>1, not full human
      plot_list[[numfull+1]] <- plot2
      
      #for RRI<1, not full human
    } else {
      plot_list[[1]] <- plot2
    }
  }
  
  #plot the infographics!  
  plot_grid(plotlist=plot_list,nrow=rows)
}