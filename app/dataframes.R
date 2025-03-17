
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


# standard state note to display for all states ###############################


standard_state_note_header <- "Key Questions to Consider"

standard_state_note_text <- paste0(
  "<br>", # match break before checked/unchecked boxes 
  "<p>Questions:</p>", 
  "<ul>", 
  "<li>one</li>", 
  "<li>two</li>", 
  "<li>three</li>", 
  "</ul>"
)

