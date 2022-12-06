
#tables <- readRDS("C:/Users/meichlersmith/The Council of State Governments/JC Research - Documents/50 State Revocations Project/MCLC Shiny App/data_new/analysis/NCRP_N_RRI_tables.RDS")


box::use(
    ./admin
  , dplyr[...]
  , purrr[...]
  , glue[glue]
  , tidyr[pivot_wider]
)



# flags 

# 2 - no data 
# 1M - some missing data 
# 1S - some suppressed data 
# 1MS - some missing and suppressed data 
# 0 - no issues 


autoflags <- function(tables, state, data, var){
  
  
  #NUMBER OF SUPPRESSED CELLS 
  n_supp_val <- ifelse(var == "POPEST", 0, tables[[data]][[state]][[var]]$longdf$SUPPRESS %>% sum())
  
  #number of missing cells 
  n_miss_val <- tables[[data]][[state]][[var]]$longdf[[var]] %>% is.na(.) %>% sum()
  
  exp_yr <- tables[[data]][[state]][[var]]$longdf %>% count(RPTYEAR) %>% pull(n) %>% .[1]
  
  full_year_missing_val <-  tables[[data]][[state]][[var]]$longdf %>% 
    group_by(RPTYEAR) %>% 
    summarise(n_na = sum(is.na(across(all_of(var))))) %>% 
    filter(n_na == exp_yr) %>% 
    pull(RPTYEAR) %>% 
    paste(., collapse = ", ") 
  
  exp_re <- tables[[data]][[state]][[var]]$longdf %>% count(RACE) %>% pull(n) %>% .[1]
  
  full_race_missing_val <- tables[[data]][[state]][[var]]$longdf %>%
    group_by(RACE) %>%
    summarise(n_na = sum(is.na(across(all_of(var))))) %>%
    ungroup() %>%
    filter(n_na == exp_re) %>%
    pull(RACE) %>%
    paste(., collapse = ", ") 
  
  tibble(
      STATE       = state
    , data        = data
    , ncrp        = tables$NCRPLET
    , var         = var
    , n_cells     = nrow(tables[[data]][[state]][[var]]$longdf)
    , n_supp = n_supp_val
    , n_miss = n_miss_val
    , full_year_missing = full_year_missing_val
    , full_race_missing = full_race_missing_val
    
  )
  
  
  
}


pull_flag <- function(tables, state, data){
  
  tibble(
      STATE = state
    , data = data
    , ncrp = tables$NCRPLET
    , var = "INFOGRAPH"
    , flag  = tables[[data]][[state]]$INFOGRAPH$FLAG 
  )
  
}




#' Assign flags and export csv 
#'
#' @param tables 
#'
#' @return
#' @export
#'
#' @examples
export <- function(tables, popdenom){
  
  
  admin$mylog(glue("Flags {tables$NCRPLET} {popdenom} - Start"))
  
  long <- bind_rows(
      map(tables$STATEVEC, ~autoflags(tables, .x, var = "REVCNT", data = popdenom))
    , map(tables$STATEVEC, ~autoflags(tables, .x, var = "POPEST", data = popdenom))
    , map(tables$STATEVEC, ~autoflags(tables, .x, var = "RATE"  , data = popdenom))
    , map(tables$STATEVEC, ~autoflags(tables, .x, var = "RRI"   , data = popdenom))
    , map(tables$STATEVEC, ~pull_flag(tables, .x,                 data = popdenom))
  ) %>% 
    mutate(
      flag = case_when(
        n_miss == n_cells ~ "2"
        , n_supp == 0 & n_miss != 0 ~ "1M"
        , n_supp != 0 & n_miss == 0 ~ "1S"
        , n_supp != 0 & n_miss != 0 ~ "1MS"
        , n_supp == 0 & n_miss == 0 ~ "0"
        , var == "INFOGRAPH"        ~ flag 
      )
    ) %>% 
    rowwise() %>% 
    mutate(
      note_miss = case_when(
        n_miss == 0 & flag != "2" ~ ""
        , flag == "2" ~ "All data missing. "
        , n_miss != 0 & full_year_missing == "" & full_race_missing == "" ~ paste0(n_miss, " cell(s) are missing. ")
        , n_miss != 0 & full_year_missing != "" & full_race_missing == "" ~ paste0("Full year(s) missing: ", full_year_missing, ". ")
        , n_miss != 0 & full_year_missing == "" & full_race_missing != "" ~ paste0("Full race(s) missing: ", full_race_missing, ". ")
        , n_miss != 0 & full_year_missing != "" & full_race_missing != "" ~ paste0("Full year(s) missing: ", full_year_missing, " and full race(s) missing: ", full_race_missing, ". ")
      )
    ) %>% 
    mutate(
      note_supp = case_when(
        n_supp == 0 ~ ""
        , n_supp != 0 ~ paste0(n_supp, " cell(s) are suppressed. ")
      )
    ) %>% 
    mutate(
      note = paste(c(note_miss, note_supp), collapse = "")
    ) %>% 
    ungroup() %>% 
    select(-note_miss, -note_supp)
  
  
  short <- long %>% 
    select(STATE, ncrp, data, var, flag, note) %>% 
    pivot_wider(names_from = var, values_from = c(flag, note)) %>% 
    select(STATE, ncrp, data, contains("flag"), note_RATE) 
  
  
  readr::write_csv(
      x = short
    , file = file.path(admin$sp_data, glue("FLAGS_{tables$NCRPLET}_{popdenom}.csv"))
  )
  
  
  admin$mylog(glue("Flags {tables$NCRPLET} {popdenom} - End"))
  
  
}



#' Text on Flag meanings 
#'
#' @param flag 
#'
#' @return
#' @export
#'
#' @examples
flag_meaning <- function(flag){
  case_when(
      flag == "0" ~ "no issues"
    , flag == "1M" ~ "missing data (year, race, or cells in a table)"
    , flag == "1S" ~ "suppressed data"
    , flag == "1MS"~ "missing data (year, race, or cells in a table) and suppressed data"
    , flag == "2"  ~ "completely missing, can't create table/infographic"
  )
}


