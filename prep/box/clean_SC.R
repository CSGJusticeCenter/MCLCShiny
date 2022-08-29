
box::use(
    ./admin
  , ./import
  , ./STCNVRT
  , dplyr[...]
  , stringr[str_sub]
  , tidyr[pivot_longer]
)




#' Add additional state id's to NCRP data
#'
#' @param DF NCRP data set that has a STATE factor variable
#'
#' @return Same DF with ABB
addSTATEids <- function(DF){
  
  DF %>% 
    rename(FIPS = STATE, STATE = NAME) %>% 
    #create other state id columns: FIPS, NAME, ABB | rowwise requried for STCNVRT$cnvrt()
    rowwise() %>%
    mutate(
        FCT_NUM = as.numeric(FIPS)
      , ABB     = STCNVRT$cnvrt(FIPS, "fips", "abb_usps")
    ) %>% 
    ungroup() %>% 
    #make state id columns the same factor levels (based on FIPS)
    mutate_at(vars(ABB, FIPS, STATE), ~forcats::fct_reorder(factor(.), FCT_NUM)) %>% 
    #remove numeric column and fips column 
    select(-FCT_NUM) %>% 
    #put id's column at the beginning 
    select(any_of(admin$idcols), everything())
  
}


#' Prepare SC data
#'
#' @return list of different cross section fo counts 
#' @export
prep <- function(){
  
  admin$mylog("Import State Charactersitics (SC) data")
  RAW <- import$SC()
  
  admin$mylog(paste0("Data prep:"
    , "\n   -- Filter data to be 18+, combined sex, and combine ORIGIN"
    , "\n   -- recode RACE/ORIGIN categories"
    , "\n   -- sum across categories"
    , "\n   -- add state ids "
    ))
  cs_R <-RAW %>% 
    filter(
        AGE    >= 18 #only include ages 18+
      , ORIGIN != 0  #remove rows that combine Hispanic and not-Hispanic
      , SEX    == 0  #only total sex (not looking at data by male/female)
    ) %>% 
    #recode ORIGIN/RACE --> NCRP RACE categories 
    admin$SC_RE() %>% 
    #pivot-longer on years interested in 
    pivot_longer(
        cols = all_of(paste0("POPESTIMATE", 2015:2020))
      , names_to = "POPESTYR"
      , values_to = "POPEST"
    ) %>% 
    #combine/sum over rows 
    group_by(STATE, NAME, POPESTYR, RACE, POPTYPE) %>% 
    summarise(POPEST = sum(POPEST), .groups = "keep") %>% 
    #add/factor state ids 
    addSTATEids() %>% 
    #add RPTYEAR, remove POPESTYR column
    mutate(RPTYEAR = as.numeric(str_sub(POPESTYR, -4, -1)), .after = ABB) %>% 
    select(-POPESTYR) %>% 
    #add factor levels for RACE 
    mutate(RACE = factor(RACE, levels = admin$lev_RACE)) %>% 
    #arrange
    arrange(STATE, RPTYEAR, RACE) 
  
  
  admin$mylog("Sum accross state and year")
  cs_t <- cs_R %>% 
    group_by(STATE, FIPS, ABB, RPTYEAR, POPTYPE) %>%
    summarise(POPEST = sum(POPEST), .groups = "keep") %>% 
    ungroup()
  
  
  out <- list(
      "R"  = cs_R  #cross section by RACE
    , "t"  = cs_t  #no cross section, total by STATE/RPTYEAR 
  )
  
  admin$mylog("Complete SC prep")
  
  return(out)
  
  
}





