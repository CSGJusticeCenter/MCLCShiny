# Code written by Matt Herman, original file name: get-ncrp-base-rate-acs-pums.R
# Slight edits by Martha Eichlersmith (add 2015, export to sharepoint using admin$sp_data_raw path)
# Created:   2022-11-22
# Last edit: 2022-12-05
# run this script once to create the PUMS data that will be used 
# save output to SharePoint 

#takes ~12-16 minutes 

################
## PACKAGES 

box::use(
    tidycensus[...]
  , dplyr[...]
  , purrr[...]
  , 
)

# library(tidycensus)
# library(tidyverse)


################
## SET YEARS  

admin$mylog("Start PUMS PUll\n\n")

theseYears <- c(2015)

admin$mylog(glue("pull data for {paste(theseYears, collapse = ', ')}\n\n"))


################
## FUNCTIONS AND PULL DATA 

# get state code and names from pums to recode after we download

st_recode <- tidycensus::pums_variables |>
  filter(survey == "acs1", var_code == "ST", year == "2019") |>
  select(ST = val_min, val_label) |>
  separate(val_label, into = "state", sep = "/", extra = "drop")  # this is formatted weirdly, so let's just use the state name

# define function to pull race and ethnicity variables from pums api

get_pums_race_eth <- function(year, state = "all") {
  get_pums(
    variables = c("AGEP", "HISP", "RACBLK", "RAC1P"),
    state = state,
    year = year,
    survey = "acs1"
    )
}


# define function to calculate new combined race/eth var for ncrp base rate

calc_ncrp_race <- function(df) {
  df |>
    filter(AGEP >= 18) |>                                     # adults only
    mutate(
      ncrp_race = case_when(
         HISP %in% c("1", "01") & RAC1P  == "1" ~ "White",    # not hispanic and white alone
        !HISP %in% c("1", "01") & RACBLK == "0" ~ "Hispanic", # hispanic, not black
        RACBLK == "1"                           ~ "Black",    # black alone or in combination and any ethnicity
        TRUE                                    ~ "Other"     # all others
      )
    ) |>
    count(ST, ncrp_race, wt = PWGTP) |>                       # count number of people by new race, using survey person weight
    group_by(ST) |>
    mutate(pct = n / sum(n)) |>                               # pct race/eth by state
    ungroup()
}



# for each year, download race and eth vars from pums
# warning: this takes a long time because it hits the api for 
# each state for each year
# (could set up an ipums download alternatively, 
# but i don't love the ipums interface)


pums <- map(theseYears, get_pums_race_eth)

# calculate new race variable for each year and combine into a single df
# join with state lookup to get state names from pums state codes


ncrp_race_by_state <- pums |>
  set_names(theseYears) |>
  map_dfr(calc_ncrp_race, .id = "year") |>
  mutate(
    ST = str_pad(ST, 2, pad = "0"),   # 2016 has some wonky codes that need to be padded with 0s to recode
    year = as.numeric(year)
    ) |>
  left_join(st_recode, by = "ST") |>
  select(-ST) |>
  arrange(year, state, ncrp_race)

# write to file

#write_rds(ncrp_race_by_state, file.path(admin$sp_data_raw, "PUMS/ncrp_race_by_state.RDS"))
admin$mylog("Save data in raw data folder: PUMS/ncrp_race_by_state.RDS")

admin$mylog("End PUMS PUll")
