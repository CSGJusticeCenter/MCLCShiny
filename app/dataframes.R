
# read data frames of data (count/change) and text #############################
# must be in local repo to publish app

states <- state.name

obj_names <-  c(
  # data (counts/changes/etc)  
  "svii_explorer", 
  "svii_explorer_table", 
  "svii_table", 
  "svii_demo_table", 
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


# read highchart objects #######################################################

hc_obj <-  c(
  "natl_hex_lst", 
  "state_area_lst", 
  "state_bar_lst" 
)

for (x in hc_obj){
  df <- readRDS(paste0("data/", x, ".rds")) 
  assign(x, df)
  rm(df)
  rm(x)
}

# hex map notes ################################################################

# link to another shiny tab 
# https://stackoverflow.com/questions/34315485/linking-to-a-tab-or-panel-of-a-shiny-app
hex_map_note <- glue(
  "Use caution when comparing years and states. In some states, data between \\
  different years cannot be compared due to changes in data availability, \\
  definitions, or methods. In some states, metrics represent only partial data. \\
  For instance, no probation data is included in the count of total technical violations. \\
  See <a href = '#statedashboard'>state reports</a> for details about the data used for each state."
)


# standard state note to display for all states ###############################


standard_state_note_header <- "Key Questions to Consider"

standard_state_note_text <- paste0(
  "<br>", # match break before checked/unchecked boxes in state notes 
  "<ol>", 
  "<li>How are state supervision policies contributing to changes in prison admissions from supervision violations, and what policy and practice changes can be made to address increases in prison admissions and populations (if needed)? </li>", 
  "<li>What investments are being made in community-based programming and services that can provide alternatives to incarceration for technical supervision violations?</li>", 
  "<li>How is the state measuring the effectiveness of policies aimed at reducing supervision violations and their impact on prison populations?</li>", 
  "<li>    How do this state's data and policies compare to states with similar characteristics, and what best practices can be adopted from states that have been successful in reducing the number of people readmitted to prison from community supervision?</li>",
  "</ol>"
)

