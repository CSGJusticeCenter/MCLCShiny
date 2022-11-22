

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
create_tabledf <- function(RRIDATA, whichPOP, whichSTATE, whichVAL, whichTABLE = "table_suppress"){
  
  
  table <- RRIDATA[[whichPOP]][[whichSTATE]][[whichVAL]][[whichTABLE]] 
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
        minWidth = 70
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
          , minWidth = 130
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
      , RACE       = colDef(name = "Race/Ethnicity", align = "left", minWidth = 130)
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
  
  prefix <- "<span style='font-family: Graphik-Bold !important;'>"
  suffix <- "</span>"
  
  fill1 <- case_when(
      pop_denom == "BJS" ~ paste0(prefix, "disparities in parole revocations"                        , suffix)
    , pop_denom == "CEN" ~ paste0(prefix, "cumulative disparities across the criminal justice system", suffix)
  )
  
  fill2 <- case_when(
      pop_denom == "BJS" ~ paste0(prefix, "parole population", suffix) 
    , pop_denom == "CEN" ~ paste0(prefix, "community"        , suffix)
  )
  
  outtext <- paste0(
      "<div class = 'retxt'>"
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
    outtext <- paste0("<h3 class = 'reh3'>For every White Client that is revoked ...<h3>")
  }
  
  if (dataavail == 0){
    outtext <- paste0("<h3 class='reh3'>No data</h3>"
                      , "<div class = 'retxt'>"
                      , "Data to calculate disparites in parole revocations is not available.<br>"
                      , note
                      , "</div>"
                     )
  }
  
  return(outtext)
  
}



