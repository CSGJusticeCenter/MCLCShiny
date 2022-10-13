

library(tidyverse)

box::use(
  box/admin
)

REV_APS <- readRDS(file.path(admin$sp_data, "NCRP_REV_APS.RDS"))
REV_SC <- readRDS(file.path(admin$sp_data, "NCRP_REV_SC.RDS"))


bind_rows(
  REV_APS$R %>% 
    filter(RACE %in% admin$lev_RACE[1:3]) %>% #only look at White, Black, Hispanic
    filter(RPTYEAR == RECENT_YR) %>%  #most recent year
    mutate(OFFGENERAL = "All categories", denom = "BJS - APS") %>% 
    select(denom, STATE, RPTYEAR, OFFGENERAL, RACE, REVCNT, POPEST, RATE_100K, RRI)
  , 
  REV_APS$OR %>% 
    filter(RACE %in% admin$lev_RACE[1:3], OFFGENERAL %in% admin$lev_OFFGENERAL[1:4]) %>% #only look at White, Black, Hispanic
    filter(RPTYEAR == RECENT_YR) %>%  #most recent year
    mutate(denom = "BJS - APS") %>% 
    select(denom, STATE, RPTYEAR, OFFGENERAL, RACE, REVCNT, POPEST, RATE_100K, RRI)
) %>% write_csv(file.path(admin$sp_data, "RRI_rates_BJSdenom.csv"))


bind_rows(
  REV_SC$R %>% 
    filter(RACE %in% admin$lev_RACE[1:3]) %>% #only look at White, Black, Hispanic
    filter(RPTYEAR == RECENT_YR) %>%  #most recent year
    mutate(OFFGENERAL = "All categories", denom = "Census - SC") %>% 
    select(denom, STATE, RPTYEAR, OFFGENERAL, RACE, REVCNT, POPEST, RATE_100K, RRI)
  , 
  REV_SC$OR %>% 
    filter(RACE %in% admin$lev_RACE[1:3], OFFGENERAL %in% admin$lev_OFFGENERAL[1:4]) %>% #only look at White, Black, Hispanic
    filter(RPTYEAR == RECENT_YR) %>%  #most recent year
    mutate(denom = "Census - SC") %>% 
    select(denom, STATE, RPTYEAR, OFFGENERAL, RACE, REVCNT, POPEST, RATE_100K, RRI)
) %>% write_csv(file.path(admin$sp_data, "RRI_rates_Censusdenom.csv"))
