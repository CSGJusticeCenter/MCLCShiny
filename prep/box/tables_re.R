
box::use(
    ./admin
  , dplyr[...]
  , tidyr[pivot_wider]
  , purrr[...]
)





#' Create a table for a state of a single metric 
#' years are column names 
#' single metric are values 
#'
#' @param DATA 
#' @param whichYEARS 
#' @param whichRACE 
#' @param whichABB 
#' @param whichMETRIC 
#'
state_table_single_metric <- function(DATA, whichYEARS, whichRACE, whichABB, whichMETRIC){

  bind_rows(
    DATA$R %>% 
      filter(
          RPTYEAR %in% whichYEARS
        , RACE    %in% whichRACE
        , ABB      ==  whichABB
      ) %>% 
      mutate(OFFGENERAL = admin$lev_OFFGENERAL2[1]) %>% 
      select(OFFGENERAL, RACE, RPTYEAR, all_of(whichMETRIC)) %>% 
      pivot_wider(names_from = RPTYEAR, values_from = all_of(whichMETRIC))
    , 
    DATA$OR %>% 
      filter(
          RPTYEAR %in% whichYEARS
        , RACE    %in% whichRACE
        , ABB      ==  whichABB
        , OFFGENERAL %in% admin$lev_OFFGENERAL[1:4]
      ) %>% 
      select(OFFGENERAL, RACE, RPTYEAR, all_of(whichMETRIC)) %>% 
      pivot_wider(names_from = RPTYEAR, values_from = all_of(whichMETRIC))
  )

}


#' Create and Save tables for the Racial and Ethnic Disparities Tab 
#'
#' @return
#' @export
#'
#' @examples
create_tables <- function(){
  
  REV_APS <- readRDS(file.path(admin$sp_data, "NCRP_REV_APS.RDS"))
  REV_SC  <- readRDS(file.path(admin$sp_data, "NCRP_REV_SC.RDS"))
  
  
  tables <- list(
    "APS" = map(
      levels(REV_APS$t$ABB) %>% set_names(),
      ~list(
        "RRI"       = state_table_single_metric(REV_APS, 2015:2018, admin$lev_RACE[2:3], .x, "RRI")
        , "RATE_100K" = state_table_single_metric(REV_APS, 2015:2018, admin$lev_RACE[1:3], .x, "RATE_100K")
        , "REVCNT"    = state_table_single_metric(REV_APS, 2015:2018, admin$lev_RACE[1:3], .x, "REVCNT")
      ) #end list 
    ) #end map BJS
    ,   "SC" = map(
      levels(REV_SC$t$ABB) %>% set_names(),
      ~list(
        "RRI"       = state_table_single_metric(REV_SC,  2015:2019, admin$lev_RACE[2:3], .x, "RRI")
        , "RATE_100K" = state_table_single_metric(REV_SC,  2015:2019, admin$lev_RACE[1:3], .x, "RATE_100K")
        , "REVCNT"    = state_table_single_metric(REV_SC,  2015:2019, admin$lev_RACE[1:3], .x, "REVCNT")
      ) #end list 
    ) #end map BJS
  )
  
  admin$SPsaveRDS(tables, "NCRP_RE_tables.RDS")
  
}



# 
# 
# REV_SC$t %>% 
#   filter(ABB %in% c("AK", "CT", "OR", "VT"))



