

library(tidyverse)

box::use(
  box/admin
  , glue[glue]
)

NCRPLET <- "N"
CENPOP  <- "PUMS"

REV_BJS <- readRDS(file.path(admin$sp_data, glue("NCRP_{NCRPLET}_REV_BJS.RDS")))
REV_CEN <- readRDS(file.path(admin$sp_data, glue("NCRP_{NCRPLET}_REV_{CENPOP}.RDS")))


bind_rows(
  REV_BJS$R %>% 
    filter(RACE %in% admin$lev_RACE[1:3]) %>% #only look at White, Black, Hispanic
    filter(RPTYEAR == RECENT_YR) %>%  #most recent year
    mutate(OFFGENERAL = "All categories", data = glue("{NCRPLET} - BJS")) %>% 
    select(data, STATE, RPTYEAR, OFFGENERAL, RACE, REVCNT, SUPPRESS, POPEST, S_REVCNT, S_RATE, S_RRI)
  , 
  REV_BJS$OR %>% 
    filter(RACE %in% admin$lev_RACE[1:3], OFFGENERAL %in% admin$lev_OFFGENERAL[1:4]) %>% #only look at White, Black, Hispanic
    filter(RPTYEAR == RECENT_YR) %>%  #most recent year
    mutate(data = glue("{NCRPLET} - BJS")) %>% 
    select(data, STATE, RPTYEAR, OFFGENERAL, RACE, REVCNT, SUPPRESS, POPEST, S_REVCNT, S_RATE, S_RRI)
) %>% write_csv(file.path(admin$sp_data, "RRI_rates_BJSdenom.csv"))


bind_rows(
  REV_CEN$R %>% 
    filter(RACE %in% admin$lev_RACE[1:3]) %>% #only look at White, Black, Hispanic
    filter(RPTYEAR == RECENT_YR) %>%  #most recent year
    mutate(OFFGENERAL = "All categories", data = glue("{NCRPLET} - {CENPOP}")) %>% 
    select(data, STATE, RPTYEAR, OFFGENERAL, RACE, REVCNT, SUPPRESS, POPEST, S_REVCNT, S_RATE, S_RRI)
  , 
  REV_CEN$OR %>% 
    filter(RACE %in% admin$lev_RACE[1:3], OFFGENERAL %in% admin$lev_OFFGENERAL[1:4]) %>% #only look at White, Black, Hispanic
    filter(RPTYEAR == RECENT_YR) %>%  #most recent year
    mutate(data = glue("{NCRPLET} - {CENPOP}")) %>% 
    select(data, STATE, RPTYEAR, OFFGENERAL, RACE, REVCNT, SUPPRESS, POPEST, S_REVCNT, S_RATE, S_RRI)
) %>% write_csv(file.path(admin$sp_data, "RRI_rates_Censusdenom.csv"))
