

box::use(
    reactable[...]
  , dplyr[...]
)


#' Create table that will be shown in shiny app 
#'
#' @param RRIDATA 
#' @param whichPOP 
#' @param whichSTATE 
#' @param whichVAL 
#' @param whichTABLE 
#'
#' @return
#' @export
#'
#' @examples
create_tabledf <- function(RRIDATA, whichNCRP, whichPOP, whichSTATE, whichVAL, whichTABLE = "table_suppress"){
  
  
  table <- RRIDATA[[whichNCRP]][[whichPOP]][[whichSTATE]][[whichVAL]][[whichTABLE]] 
  yrs <- colnames(table)[-c(1:2)]
  
  df <- table %>% 
    mutate(
        OFFGENERALB = OFFGENERAL
      , OFFGENERAL  = as.character(OFFGENERAL)
      , OFFGENERAL  = ifelse( OFFGENERAL == lag(OFFGENERAL) & !is.na(lag(OFFGENERAL)), "", OFFGENERAL)
    )
  
  
  nodata <- df %>% filter(if_all(all_of(yrs), ~ is.na(.))) 
  
  if (nrow(df) == nrow(nodata)){
    #all data missing 
    out <-df[0, c("OFFGENERAL", "RACE", "OFFGENERALB")]
  } 
  
  if (nrow(df) != nrow(nodata)){
    #some or none data missing 
    out <-df 
  } 
  

  
  return(out)
  
}


# TESTING
# RRIDATA <- readRDS("app/data/NCRP_RRI_tables.RDS")
# whichPOP <- "BJS"
# whichSTATE <- "Arizona"
# whichVAL <- "RRI"
# whichTABLE <- "table_asis"
# DF <- create_tabledf(RRIDATA, whichPOP, whichSTATE, whichVAL, whichTABLE)


#' Create reactable tables for Shiny 
#'
#' @param DF 
#'
#' @return
#' @export
#'
#' @examples
create_reactable <- function(DF){
  
  
  reactable(
    DF
    , style = list(fontFamily = "Graphik, sans-serfit", fontsize = "1.4rem")
    , theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"))
    , defaultColDef = colDef(
        minWidth = 55
      , align = "right"
      , headerVAlign = "bottom"
    )
    , compact = TRUE
    , fullWidth = TRUE
    , searchable = FALSE
    , pagination = FALSE
    , sortable = FALSE
    , columns = list(
        OFFGENERAL = colDef(
            name = "Offense Category"
          , align = "left"
          , minWidth = 105
          #no border when witin offense category 
          , style = JS( 
            "function(rowInfo) {
              var value = rowInfo.row['OFFGENERAL']
              if (value == '') {
                var borderTop = 'none'
              } 
              return {borderTop: borderTop}
              }")
        )
      , RACE       = colDef(name = "Race/Ethnicity", align = "left", minWidth = 105)
      , OFFGENERALB = colDef(show = FALSE)
      
    ) #end columns list
    , rowStyle = function(index) {
           if ( DF$OFFGENERALB[index] == "All categories"){list(`background`="rgba(0,0,0,0.05)")} #, fontWeight="bold"
      else if ( DF$OFFGENERAL [index] != "" & index != 1 ){list(`border-top`   = "1.25px solid rgba(0,0,0,0.10)") } 
      else if (index == nrow(DF)                         ){list(`border-bottom`= "1.75px solid rgba(0,0,0,0.10)") } 
      else    { } 
    } #end function of index
    
  ) #end reactable 
  
  
  
  
}



#' Text to display the pop_denom 
#'
#' @param pop_denom 
#'
#' @return
#' @export
#'
#' @examples
pop_denom_text <- function(pop_denom){
  
  rlang::arg_match(pop_denom, c("BJS", "CEN"))
  
  prefix <- "<b>"
  suffix <- "</b>"
  
  fill1 <- case_when(
      pop_denom == "BJS" ~ paste0(prefix, "disparities in parole revocations"                        , suffix)
    , pop_denom == "CEN" ~ paste0(prefix, "cumulative disparities across the criminal justice system", suffix)
  )
  
  fill2 <- case_when(
      pop_denom == "BJS" ~ paste0(prefix, "parole population", suffix) 
    , pop_denom == "CEN" ~ paste0(prefix, "community"        , suffix)
  )
  
  outtext <- paste0(
      "<div class = 'notetxt'>"
     , "To highlight "
     , fill1
     , " for each racial and ethnic group in the "
     , fill2
     , ", rates are shown relative to White individuals to highlight higher or lower than expected representation.<br>"
     , "</div>"
  )
  
}



#' Text to display the pop_denom 
#'
#' @param pop_denom 
#'
#' @return
#' @export
#'
#' @examples
infographic_header <- function(dataavail, note){
  
  char <- as.character(dataavail)
  rlang::arg_match(char, c("1", "0"))
  
  
  if (dataavail == 1){
    outtext <- paste0("<h3 class = 'reh3'>For every White individual that is revoked ...<h3>")
  }
  
  if (dataavail == 0){
    outtext <- paste0("<h3 class='nodata'>No data</h3>"
                      , "<div class = 'notetxt'>"
                      , "Data to calculate disparites in parole revocations are not available.<br>"
                      , note
                      , "</div>"
                     )
  }
  
  return(outtext)
  
}



#' CReate alt text for infogrpahic
#'
#' @param RRIDATA 
#' @param whichNCRP 
#' @param whichPOP 
#' @param whichRE 
#' @param whichSTATE 
#'
#' @export
infograph_alt <- function(RRIDATA, whichNCRP, whichPOP, whichRE, whichSTATE){
  
  
  thisdata <- RRIDATA[[whichNCRP]][[whichPOP]][[whichSTATE]]$INFOGRAPH$DF |> filter(RACE == whichRE)
  
  suppress <- thisdata$SUPPRESS
  rri_val <- thisdata$S_RRI
  
  pop_name <- case_when(
      whichPOP == "BJS" ~ "parole"
    , whichPOP == "CEN" ~ "community" 
  )
  
  suppress_pre <- ifelse(suppress == 0, NA, "less than")
  suppress_suf <- ifelse(suppress == 0, NA, "Note that revocation counts have been suppressed.")
  
  # The info-graphic for [state] shows that when looking at prison
  # [admissions/population] as part of the [parole/community] population, for
  # every White individual revoked there are *less than* [RRI] [Black/Hispanic]
  # individuals revoked.  *Note that the revocation counts have been
  # suppressed.*
  
  
  string_vec <- c(
    "The info-graphic for"
    , whichSTATE
    , "shows that when looking at prison"
    , tolower(whichNCRP)
    , "as part of the"
    , pop_name 
    , "population," 
    , "for every White individual revoked there are"
    , suppress_pre
    , sprintf("%.1f", round(rri_val, 1))
    , whichRE
    , "individuals revoked."
    , suppress_suf
  )
  
  
  alt_text <- paste(string_vec[!is.na(string_vec)], collapse = " ")
  
  return(alt_text)
  
}



#' CReate alt text when there isn't an infographic 
#'
#' @param whichNCRP 
#' @param whichPOP 
#' @param whichRE 
#' @param whichSTATE 
#'
#' @export
infograph_alt_noinfog <- function( whichNCRP, whichPOP, whichRE, whichSTATE){
  
  pop_name <- case_when(
      whichPOP == "BJS" ~ "parole"
    , whichPOP == "CEN" ~ "community" 
  )
  
  
  #[state] does not have enough data to show disparities in prison 
  # [admissions/population] as part of the [parole/community] population 
  # for [Black/Hispanic] individuals revoked. 

  string_vec <- c(
      whichSTATE
    , "does not have enough data to show disparities in prison"
    , tolower(whichNCRP)
    , "as part of the"
    , pop_name 
    , "population for" 
    , whichRE
    , "individuals revoked."
  )
  
  
  alt_text <- paste(string_vec[!is.na(string_vec)], collapse = " ")
  
  return(alt_text)
  
}




