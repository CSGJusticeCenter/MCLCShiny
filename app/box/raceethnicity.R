

box::use(
    reactable[...]
  , dplyr[...]
)


create_tabledf <- function(RRIDATA, whichPOP, whichSTATE, whichVAL){
  
  
  df <- RRIDATA[[whichPOP]][[whichSTATE]][[whichVAL]]
  
  df$OFFGENERALB <- df$OFFGENERAL
  df$OFFGENERAL <- ifelse(  df$OFFGENERAL == lag(df$OFFGENERAL) & !is.na(lag(df$OFFGENERAL)), "", df$OFFGENERAL)
  
  return(df)
  
  
}


## TESTING
# rridata <- readRDS("data/NCRP_RRI_tables.RDS")
# DF <- create_tabledf(rridata, "BJS", "Arizona", "RRI")
# whichVAL <- "RRI"

create_reactable <- function(DF, whichVAL){
  
  ndigits <- case_when(
      whichVAL == "RRI"       ~ 2
    , whichVAL == "RATE_100K" ~ 0
    , whichVAL == "RATE_1K"   ~ 0
    , whichVAL == "REVCNT"    ~ 0
    , TRUE                    ~ 0 
  )
  
  
  reactable(
    DF
    , style = list(fontFamily = "Graphik, sans-serfit", fontsize = "1.4rem")
    , theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"))
    , defaultColDef = colDef(
      format = colFormat(separators = TRUE, digits = ndigits)
      , minWidth = 100
      , align = "right"
      , headerVAlign = "bottom"
    )
    , compact = TRUE
    , fullWidth = FALSE
    , searchable = FALSE
    , pagination = FALSE
    , columns = list(
        OFFGENERAL = colDef(
            name = "Offense Category"
          , align = "left"
          , minWidth = 150
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
      , RACE       = colDef(name = "Race/Ethnicity", align = "left", minWidth = 150)
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




