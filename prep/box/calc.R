
box::use(
    ./admin
  , ./clean_NCRP
  , ./clean_SC
  , ./clean_APS
  , dplyr[...]
  , purrr[...]
  , glue[glue]
)




calcrate_race <- function(DF, POP_R){
  
  R_RR <- DF %>% 
    #add population estimates 
    left_join(., POP_R  , by = c(admin$groupcols, "RACE")) %>% 
    #add column for just white population estimate 
    #calculate rates 
    mutate(RATE = REVCNT/POPEST) 
  
  #check that 1st factor is white 
  admin$isWhite(sort(unique(R_RR$RACE))[admin$fctnum_white])
  
  RR_W <- R_RR %>% 
    #only include White 
    filter(as.numeric(RACE) == admin$fctnum_white) %>% 
    #select factor columns (STATE, ABB, FIPS, RACE, and possible OFFGENERAL)
    select(where(is.factor), RPTYEAR, WHITERATE = RATE) %>% 
    #remove RACE
    select(-RACE)
  
  OUT <- left_join(R_RR, RR_W) %>% 
    #calculate relative rate index 
    mutate(RRI = RATE/WHITERATE)
  
  return(OUT)
  
}



calcrate_total <- function(DF, POP_t){
  
  DF %>% 
    #add population estimates  
    left_join(., POP_t, by = c(admin$groupcols)) %>% 
    #calculate rate 
    mutate(RATE = REVCNT/POPEST)
  
}

 
#' Calculate Rates and Relative rates by combining NCRP and SC data
#'
#' @return list of 4 df's with rates
#' @export
combine_and_calcrates <- function(pop_denom = "SC"){
  
  admin$mylog(glue("Import and clean NCRP revocations data"))
  
  NCRP <- clean_NCRP$prep()
  
  if (pop_denom == "SC"){
    admin$mylog(glue("Import and clean SC for population (denominator)"))
    
    POP  <- clean_SC$prep()
  } else if (pop_denom == "APS"){
    admin$mylog(glue("Import and clean APS for population (denominator)"))
    POP <- clean_APS$prep()
  } else {
    stop("Invalid population denom")
  }
  
  
  admin$mylog(glue("Start - rate/RRI calcuations"))
  CNTRT_DF <-c(
      map(NCRP[c("OR", "R")], calcrate_race,  POP_R = POP$R)
    , map(NCRP[c("O" , "t")], calcrate_total, POP_t = POP$t)
    ) %>% 
    map(., ~mutate(., RATE_1K = RATE*1E3, RATE_100K = RATE*1E5, RATE_1MIL = RATE*1E6))
  
  admin$mylog(glue("End   - rate/RRI calcuations"))
  
  return(CNTRT_DF)
}
