
box::use(
    ./admin
  , ./import
  , dplyr[...]
  , stringr[str_sub]
  , tidyr[pivot_longer, drop_na]
)




#' Add additional state id's to APS data
#'
#' @param DF NCRP data set that has a STATE factor variable
#'
#' @return Same DF with ABB
addSTATEids <- function(DF){
  
  DF %>% 
    rename(ABB = STATE) %>% 
    #create other state id columns: FIPS, NAME, ABB 
    rowwise() %>%
    mutate(
        FIPS    = csgjcr::csg_state_convert(ABB, "abbr", "fips")
      , STATE   = csgjcr::csg_state_convert(ABB, "abbr", "name")
      , FCT_NUM = as.numeric(FIPS)
    ) %>% 
    ungroup() %>% 
    #make state id columns the same factor levels (based on FIPS)
    mutate_at(vars(ABB, FIPS, STATE), ~forcats::fct_reorder(factor(.), FCT_NUM)) %>% 
    #remove numeric column and fips column 
    select(-FCT_NUM) %>% 
    #put id's column at the beginning 
    select(any_of(admin$idcols), everything())
  
}





#' Prepare APS data
#'
#' @return list of different cross section of counts 
#' @export
prep <- function(){
  
  
  RAW <- import$APS()
  
  admin$mylog("STart - APS prep")

  admin$mylog(paste0("APS prep - "
    , "\n   -- Drop Federal total row"
    , "\n   -- calculate other race category, pivot longer, recode into single race variable"
    , "\n   -- make new race variable a factor "
    , "\n   -- drop rows where POPEST is NA (covered in UNKRACE)"
    , "\n   -- add state id columns"
    ))
  
  cs_R <- RAW %>%
    #select variables of interest 
    select(RPTYEAR, POPTYPE, STATE, WHITE, BLACK, HISP, UNKRACE, TOTRACE) %>% 
    #remove federal total raw
    filter(STATE != "FE") %>% 
    #create race variable with NCRP categories 
    admin$APS_RE() %>% 
    #add factor levels for RACE 
    mutate(RACE = factor(RACE, levels = admin$lev_RACE)) %>% 
    #drop NA popest 
    drop_na(POPEST) %>% 
    #add/factor state ids 
    addSTATEids() %>% 
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
  
  admin$mylog("End   - APS prep")
  
  return(out)
  
  
}





