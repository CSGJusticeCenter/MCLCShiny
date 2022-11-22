
box::use(
    ./admin
  , ./clean_NCRP
  , ./clean_SC
  , ./clean_APS
  , ./clean_PUMS
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
    mutate(
          RATE =   REVCNT/POPEST
      , S_RATE = S_REVCNT/POPEST
    ) 
  
  #check that 1st factor is white 
  admin$isWhite(sort(unique(R_RR$RACE))[admin$fctnum_white])
  
  RR_W <- R_RR %>% 
    #only include White 
    filter(as.numeric(RACE) == admin$fctnum_white) %>% 
    #select factor columns (STATE, ABB, FIPS, RACE, and possible OFFGENERAL)
    select(where(is.factor), RPTYEAR, WHITERATE = RATE, S_WHITERATE = S_RATE) %>% 
    #remove RACE
    select(-RACE)
  
  OUT <- left_join(R_RR, RR_W) %>% 
    #calculate relative rate index 
    mutate(
          RRI =   RATE/  WHITERATE
      , S_RRI = S_RATE/S_WHITERATE
    )
  
  return(OUT)
  
}



calcrate_total <- function(DF, POP_t){
  
  DF %>% 
    #add population estimates  
    left_join(., POP_t, by = c(admin$groupcols)) %>% 
    #calculate rate 
    mutate(
          RATE =   REVCNT/POPEST
      , S_RATE = S_REVCNT/POPEST
    )
  
}


pullrecentyr <- function(YR_VEC, POPEST_VEC, REVCNT_VEC){
  
  YR_NA  <- ifelse(is.na(POPEST_VEC) | is.na(REVCNT_VEC), NA, YR_VEC)
  YR     <- YR_NA[!is.na(YR_NA)]
  cnt_NA <- sum(is.na(YR_NA))
  
  if (cnt_NA == length(YR_VEC)){
    RECENT_YR <- NA 
  } else {
    RECENT_YR <- max(YR)
  }
  
  return(RECENT_YR)
  
}

 
#' Calculate Rates and Relative rates by combining NCRP and SC data
#'
#' @return list of 4 df's with rates
#' @export
combine_and_calcrates <- function(pop_denom, NCRPLET){
  
  admin$mylog(glue("Import and clean NCRP revocations data"))
  
  NCRP <- clean_NCRP$prep(data = NCRPLET) 
  
  if (pop_denom == "SC"){
    admin$mylog(glue("Import and clean SC for COMMUNITY population (denominator)"))
    POP  <- clean_SC$prep()
  } else if (pop_denom == "BJS"){
    admin$mylog(glue("Import and clean BJS-APS for PAROLE population (denominator)"))
    POP <- clean_APS$prep()
  } else if (pop_denom == "PUMS"){
    admin$mylog(glue("Import and clean PUMS (ACS) for COMMUNITY population (denominator)"))
    POP <- clean_PUMS$prep()
  } else {
    stop("Invalid population denom")
  }
  
  
  admin$mylog(glue("Calc - rate/RRI calcuations"))
  CNTRT_DF <-c(
      map(NCRP[c("OR", "R")], calcrate_race,  POP_R = POP$R)
    , map(NCRP[c("O" , "t")], calcrate_total, POP_t = POP$t)
    ) 

  
  admin$mylog(glue("Calc - add most recent year of data (for a given STATE/RACE cross-section)"))
  FULL_DF <- map2(CNTRT_DF, names(CNTRT_DF), 
    ~.x %>% 
      group_by(across(admin$varcs(.y, combinelst = TRUE, includeYR = FALSE))) %>% 
      mutate(RECENT_YR = pullrecentyr(RPTYEAR, POPEST, REVCNT)) %>% 
      ungroup()
  )
  
  admin$SPsaveRDS(FULL_DF, glue("NCRP_{NCRPLET}_REV_{pop_denom}.RDS"))
  
  return(FULL_DF)
}
