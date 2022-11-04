

box::use(
    reactable[...]
  , dplyr[...]
)


create_tabledf <- function(RRIDATA, whichPOP, whichSTATE, whichVAL, whichTABLE = "table_asis"){
  
  
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


create_reactable <- function(DF){
  
  
  reactable(
    DF
    , style = list(fontFamily = "Graphik, sans-serfit", fontsize = "1.4rem")
    , theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"))
    , defaultColDef = colDef(
        minWidth = 100
      , align = "right"
      , headerVAlign = "bottom"
    )
    , compact = TRUE
    , fullWidth = FALSE
    , searchable = FALSE
    , pagination = FALSE
    , sortable = FALSE
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




