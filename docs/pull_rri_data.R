
# load box modules 
box::use(../prep/box/admin)

library(tidyverse)
library(csgjcr) 

svii_demo <- readRDS("prep/svii_demo.rds")


# want sex/gender data 

svii_demo |> 
  filter(group_cat == "sex_gender") |> 
  mutate(n_avg = n/2) |> 
  select(state_name, type, data, group, n_avg, prop, comparison_source, 
         ncomp, rate, rri) |> 
  readr::write_csv(file = file.path(admin$sp_data, "svii_rri_sex_gender.csv"))


