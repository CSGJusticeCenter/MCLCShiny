
# read data frames of data (count/change) and text #############################
# must be in local repo to publish app

states <- state.name

obj_names <-  c(
  # data (counts/changes/etc)  
  "svii_explorer", 
  "svii_explorer_table", 
  "svii_table", 
  "svii_par", 
  "svii_prob", 
  "svii_valbox", 
  "svii_download", 
  "svii_yr", 
  # text - state overview notes 
  "formatted_notes", 
  "missingness_sentences" 
)

for (x in obj_names){
  df <- readRDS(paste0("data/", x, ".rds")) |> tibble::as_tibble()
  assign(x, df)
  rm(df)
  rm(x)
}


# consistent state note on each state report
state_note <- c('Whether an incarceration is the result of a new offense or technical violation is often difficult and problematic to delineate, even in states with available data. Most states do not consider a supervision violation to be the result of a new offense unless a new felony conviction is present, meaning technical violations may include misdemeanor convictions or new arrests. "Prison" includes county jail if the county was reimbursed by the state for a person’s incarceration, which occurs in some, but not all, states. Supervision violations may include revocations (i.e., unsuccessful terminations of a supervision and completion of a sentence in prison or jail) or short-term sanctions (i.e., probation or parole jurisdiction is maintained and the person is incarcerated for a short period of time in prison or jail). Not all states impose or include short-term sanctions in their count of supervision violations.')

# read highchart objects #######################################################

hc_obj <-  c(
  "natl_hex_lst", #adm_pop_maps
  "state_area_lst", #all_state_area_adm/all_state_area_pop
  "state_bar_lst" #all_state_bar_adm/all_state_bar_pop/parole_bar_adm/parole_bar_pop/probation_bar_adm/probation_bar_pop
)

for (x in hc_obj){
  df <- readRDS(paste0("data/", x, ".rds")) 
  assign(x, df)
  rm(df)
  rm(x)
}


#______________________________________________________
# read in reactable tables
# must be in local repo to publish app
#______________________________________________________

# not working because of library issue (htmlwidgets)
# load(file = "data/state_reactable_adm.rds")
# load(file = "data/state_reactable_pop.rds")
# load(file = "data/parole_reactable_adm.rds")
# load(file = "data/parole_reactable_pop.rds")
# load(file = "data/probation_reactable_adm.rds")
# load(file = "data/probation_reactable_pop.rds")

# 

default_fonts <- c("Graphik")
