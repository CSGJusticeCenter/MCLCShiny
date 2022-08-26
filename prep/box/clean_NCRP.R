
box::use(
    ./admin
  , ./import
  , ./STCNVRT
  , dplyr[...]
  , stringr[str_sub]
  , forcats[fct_recode, fct_explicit_na]
  , purrr[map]
)

#' Recode RACE/OFFGENERAL categories to shorter text
#'
#' @param DF NCRP data set
#'
#' @return NCRP data set with re-coded factor levels for RACE and OFFGENERAL 
refct <- function(DF){
  
  DF |> 
    mutate( # recode factors to shorter names 
        RACE       = fct_recode(RACE      , !!!admin$NCRPlev_RACE())
      , OFFGENERAL = fct_recode(OFFGENERAL, !!!admin$NCRPlev_OFFGENERAL())
    ) |> 
    mutate( # combine NA into the 'Other/Missing' categories 
        RACE       = fct_explicit_na(RACE      , na_level = admin$lev_RACE[5])
      , OFFGENERAL = fct_explicit_na(OFFGENERAL, na_level = admin$lev_OFFGENERAL[6])
    ) 
  
}


#' Add additional state id's to NCRP data
#'
#' @param DF NCRP data set that has a STATE factor variable
#'
#' @return Same DF with ABB
addSTATEids <- function(DF){
  
  DF %>% 
    #create other state id columns: FIPS, NAME, ABB | rowwise requried for STCNVRT$cnvrt()
    rowwise() %>%
    mutate(
        FIPS    = as.factor(str_sub(STATE, 2, 3))
      , FCT_NUM = as.numeric(as.character(FIPS))
      , ABB     = STCNVRT$cnvrt(FIPS, "fips", "abb_usps")
      , STATE   = STCNVRT$cnvrt(FIPS, "fips", "name")
    ) %>% 
    ungroup() %>% 
    #make state id columns the same factor levels (based on FIPS)
    mutate_at(vars(ABB, FIPS, STATE), ~forcats::fct_reorder(factor(.), FCT_NUM)) %>% 
    #remove numeric column and fips column 
    select(-FCT_NUM) %>% 
    #put id's column at the beginning 
    select(any_of(admin$idcols), everything())
  
}



#' Prepare NCRP data
#'
#' @return list of different cross section fo counts 
#' @export
prep <- function(){
  
  admin$mylog("Import NCRP data")
  RAW <- import$NCRP_A()
  
  admin$mylog("Filter data to include reovations and 2015+ and recode RACE")
  FILTERDF <- RAW %>% 
    #filter down to revocations from 2015+ 
    filter(ADMTYPE == "(2) Parole return/revocation", RPTYEAR >= 2015) %>% 
    #re-factor RACE and OFFGENERAL cateogires 
    refct() 
  
  admin$mylog("Create diffrent cross sections by OFFGENERAL and RACE")
  cs_OR <- FILTERDF %>% count(STATE, RPTYEAR, OFFGENERAL, RACE) %>% rename("REVCNT" = n)
  cs_O  <- FILTERDF %>% count(STATE, RPTYEAR, OFFGENERAL)       %>% rename("REVCNT" = n)
  cs_R  <- FILTERDF %>% count(STATE, RPTYEAR, RACE)             %>% rename("REVCNT" = n)
  cs_t  <- FILTERDF %>% count(STATE, RPTYEAR)                   %>% rename("REVCNT" = n)
  
  
  cs <- list(
      "OR" = cs_OR #cross section by OFFGENERAL and RACE
    , "O"  = cs_O  #cross section by OFFGENERAL
    , "R"  = cs_R  #cross section by RACE
    , "t"  = cs_t  #no cross section, total by STATE/RPTYEAR 
  )
  
  out <- map(cs, addSTATEids)
  
  admin$mylog("Complete NCRP prep")
  
  return(out)
  
}




