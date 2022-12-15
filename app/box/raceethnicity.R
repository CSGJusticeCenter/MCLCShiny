

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
pop_denom_text <- function(pop_denom, pop_or_adm_data){
  
  rlang::arg_match(pop_denom, c("BJS", "CEN"))
  rlang::arg_match(pop_or_adm_data, c("Admissions", "Population"))
  
  prefix <- "<b>"
  suffix <- "</b>"
  
  fill <- case_when(
      pop_denom == "BJS" & pop_or_adm_data == "Admissions" ~ paste0(        prefix, "disparities in prison admissions for parole revocations,"           , suffix,                  " rates")
    , pop_denom == "CEN" & pop_or_adm_data == "Admissions" ~ paste0("the ", prefix, "cumulative disparities accrued through the criminal justice system,", suffix,     " re-admission rates")
    , pop_denom == "BJS" & pop_or_adm_data == "Population" ~ paste0(        prefix, "disparities in people serving time for parole revocations,"         , suffix,                  " rates")
    , pop_denom == "CEN" & pop_or_adm_data == "Population" ~ paste0("the ", prefix, "cumulative disparities accrued through the criminal justice system,", suffix, " re-incarceration rates")
  )
  
  
  outtext <- paste0(
      "<div class = 'notetxt'>"
     , "To highlight "
     , fill
     , " are shown relative to White individuals to highlight higher or lower than expected representation."
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
  
  suppress_pre <- ifelse(suppress == 0, NA, "less than")
  suppress_suf <- ifelse(suppress == 0, NA, "Note that revocation counts have been suppressed.")
  
  thistxt <- case_when(
      whichPOP == "BJS" & whichNCRP == "Admissions" ~ "disparities in prison admissions for parole revocations"
    , whichPOP == "CEN" & whichNCRP == "Admissions" ~ "the cumulative disparities accrued through the criminal justice system for re-admissions rates"
    , whichPOP == "BJS" & whichNCRP == "Population" ~ "disparities in people serving time for parole revocations"
    , whichPOP == "CEN" & whichNCRP == "Population" ~ "the cumulative disparities accrued through the criminal justice system for re-incarceration rates"
  )
  
  
  # The info-graphic for [state] highlights [text].  For every White individual
  # revoked there are *less than* [RRI] [Black/Hispanic] individual revoked.
  # *Note that the revocation counts have been suppressed.*
  
  
  string_vec <- c(
    "The info-graphic for"
    , whichSTATE
    , "highlights"
    , thistxt
    , "."
    , "For every White individual revoked there are"
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
  
  
  # [state] does not have enough data to show [text] for [Black/Hispanic]
  # individuals revoked.
  
  thistxt <- case_when(
      whichPOP == "BJS" & whichNCRP == "Admissions" ~ "disparities in prison admissions for parole revocations"
    , whichPOP == "CEN" & whichNCRP == "Admissions" ~ "the cumulative disparities accrued through the criminal justice system for re-admissions rates"
    , whichPOP == "BJS" & whichNCRP == "Population" ~ "disparities in people serving time for parole revocations"
    , whichPOP == "CEN" & whichNCRP == "Population" ~ "the cumulative disparities accrued through the criminal justice system for re-incarceration rates"
  )
  

  string_vec <- c(
      whichSTATE
    , "does not have enough data to show"
    , thistxt
    , "for" 
    , whichRE
    , "individuals revoked."
  )
  
  
  alt_text <- paste(string_vec[!is.na(string_vec)], collapse = " ")
  
  return(alt_text)
  
}




