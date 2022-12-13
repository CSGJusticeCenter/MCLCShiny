
library(tidyverse)

box::use(box/ROOT)


## qa grids 


# map explorer 

expand.grid(
    `Select Data` = c("1 - Total", "2 - New Offense", "3 - Supervision Violation", "4 - Probation Violation", "5 - Parole Violation", "6 - Technical Violation")
  , `Select Type` = c("1 - Admissions", "2 - Population")
  , `Select Year Change` = c('1- 2018 - 2019, 1 year', '2- 2019 - 2020, 1 year', '3- 2020 - 2021, 1 year', '4 - 2018 - 2021, 3 years')
  , check_item = c("1 - map & download", "2 - table viz")
) %>% write_csv(file = file.path(csgjcr::csg_sp_path(ROOT$sp), "MCLC Shiny App/background/blank_QAgrid_mapexplorer.csv"))


# state reports 

expand.grid(
    `Select State` = state.name
  , `Select Type` = c("1 - Admissions", "2 - Population")
  , check_item = c(
      "tab0 | 4 boxes" 
    , "tab1 Overview | 1 - Prison admissions plot & download"
    , "tab1 Overview | 2 - Supervison Violation Admissions plot & download"
    , "tab1 Overview | 3 - table"
    , "tab1 Overview | 4 - notes"
    , "tab2 Parole | 1 - plot & download"
    , "tab2 Parole | 2 - table"
    , "tab3 Probation | 1 - plot & download"
    , "tab3 Probation | 2 - table" 
    , "tab4 Race/Ethnicity | BJS 1 - infographic/note"
    , "tab4 Race/Ethnicity | BJS 2 - rri table/note"
    , "tab4 Race/Ethnicity | BJS 3 - rate table/note"
    , "tab4 Race/Ethnicity | BJS 4 - rev cnts table/note"
    , "tab4 Race/Ethnicity | CEN 1 - infographic/note"
    , "tab4 Race/Ethnicity | CEN 2 - rri table/note"
    , "tab4 Race/Ethnicity | CEN 3 - rate table/note"
    , "tab4 Race/Ethnicity | CEN 4 - rev cnts table/note"
  )
) %>% write_csv(file = file.path(csgjcr::csg_sp_path(ROOT$sp), "MCLC Shiny App/background/blank_QAgrid_statereports.csv"))





## data exploration fo determining notes 

box::use(prep/box/import)


RAW_A <- import$NCRP_A() 
RAW_N <- import$NCRP_N() 



RAW_A %>% count(STATE)


state <- "Vermont"
POP <- "BJS"

NCRP_RRI_tables$Admissions[[POP]][[state]]$POPEST

RAW_N %>% 
  filter(STATE == 
  glue::glue("({csgjcr::csg_state_convert(state, 'name', 'fips')}) {state}")
  ) %>% 
  count(STATE, RPTYEAR, RACE, ADMTYPE) %>% 
  filter(RPTYEAR > 2014) %>% 
  tibble() %>% 
  tidyr::pivot_wider(names_from = RPTYEAR, values_from = n) %>% 
  arrange(ADMTYPE, RACE) %>% 
  select(STATE, ADMTYPE, RACE, everything()) %>% 
  print(n=150)

RAW_A %>% count(STATE) %>% arrange(STATE)
