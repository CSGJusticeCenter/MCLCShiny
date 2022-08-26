
box::use(
    glue[glue]
  , dplyr[...]
)



#' Log Message 
#'
#' @param text string 
#'
#' @return log message 
#' @export
mylog <- function(text){ 
  #log_info(text) #logger outputs DO NOT show in knitted RMD 
  message(paste0(Sys.time(), " - ", glue(text))) #use this if want to show in knitted rmd 
}

#' @export
idcols <- c("STATE", "FIPS", "ABB")

#' @export
groupcols <- c("STATE", "FIPS", "ABB", "RPTYEAR")



#' @export
lev_RACE <- c(
    "White"    # "(1) White, non-Hispanic"
  , "Black"    # "(2) Black, non-Hispanic"
  , "Hispanic" # "(3) Hispanic, any race"
  , "Other"    # "(4) Other race(s), non-Hispanic"
  , "Missing"
)

#' @export
fctnum_white <- 1

#' @export
lev_OFFGENERAL <- c(
    "Violent"      # "(1) Violent"
  , "Property"     # "(2) Property"
  , "Drugs"        # "(3) Drugs"
  , "Public order" # "(4) Public order"
  , "Other"        # "(5) Other/unspecified"
  , "Missing"
)


#' NCRP levels with New levels as names 
#' @export
NCRPlev_RACE <- function(){
  
  org_lev<-  c(
      "(1) White, non-Hispanic"
    , "(2) Black, non-Hispanic"
    , "(3) Hispanic, any race"
    , "(4) Other race(s), non-Hispanic"
  )
  
  #add new levels as names 
  names(org_lev) <- lev_RACE[1:4]
  
  return(org_lev)
  
}


#' NCRP levels with New levels as names 
#' @export
SC_RE <- function(DF){
  
  DF |>
    mutate(
      NCRP_RACE = case_when(
         ORIGIN == 1 & RACE == 1        ~ lev_RACE[1] 
       , ORIGIN == 1 & RACE == 2        ~ lev_RACE[2] 
       , ORIGIN == 1 & RACE %in% c(3:6) ~ lev_RACE[4]
       , ORIGIN == 2 & RACE %in% c(1:6) ~ lev_RACE[3] 
        )
    ) |> 
    #remove old RACE/ETHNICITY columns 
    select(-c(ORIGIN, RACE)) |> 
    #rename new column to 'RACE' 
    rename(RACE = NCRP_RACE)
  
}



#' NCRP levels with New levels as names 
#' @export
NCRPlev_OFFGENERAL <- function(){
  
  org_lev<-  c(
      "(1) Violent"
    , "(2) Property"
    , "(3) Drugs"
    , "(4) Public order"
    , "(5) Other/unspecified"
  )
  
  #add new levels as names 
  names(org_lev) <- lev_OFFGENERAL[1:5]
  
  return(org_lev)
  
}



#' Quick check that a value includes 'white', used for calc rel rate
#'
#' @param VAL 
#' @export
isWhite <- function(VAL){
  
  #see if the word 'WHITE' is detected in value
  #DONE in case cateogry label changes form White to White, non-hispanic or White, alone 
  isWhite <- stringr::str_detect(toupper(VAL), "WHITE")
  error_message <- "RACE category is NOT WHITE, please check"
  if(isWhite == FALSE) stop(error_message) 
  
}



