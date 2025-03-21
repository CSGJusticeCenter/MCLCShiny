
box::use(
  ./box/admin, 
  dplyr[...], 
  janitor[clean_names], 
  openxlsx[read.xlsx], 
  readxl[read_excel], 
  stringr[str_replace, str_replace_all]
)



notes_spreadsheet <- file.path(admin$sp_survey, "Data", "raw", "SVII_2024_Survey_Notes.xlsx")
# file.path(admin$sp_data_raw, "notes/states_notes_no_data_text.xlsx") # old location 

# STATE NOTES (DISPLALY FORMATED NOTES) ----------------------------------------

notes <- openxlsx::read.xlsx( # need to use openxlsx; formatting issue 
    notes_spreadsheet,
    sheet = "DISPLAY_formatted_notes"
  ) |> 
  as_tibble() |> 
  # format checkbox and x box for parole and probation metrics
  # if the state submitted the variable, it gets a green check
  # if the state did not submit the variable, it gets a red x
  clean_names() |> 
  mutate(across(c(probation_metrics, parole_metrics), 
    ~str_replace_all(
      .x, 
      c(
        "☒" = "<br><span style='color: #248A3D;'>&#x2714;&nbsp;&nbsp;&nbsp;</span>",
        "☐" = "<br><span style='color: #D70015;'>&#x2716;&nbsp;&nbsp;&nbsp;</span>"
      )
    )
  ))



formatted_notes <- notes |> 
  # prob/par notes 
  mutate(across(
    c(probation_metrics, parole_metrics), 
    ~paste0("<p class = 'statetxt'>", .x, "</p>")
  )) |> 
  # prob/par arsterisks 
  mutate(across(
    c(parole_asterisks, probation_asterisks), 
    ~paste0("<p class = 'statetxt'>", .x, "</p>") |> 
      str_replace_all("\\*+", "<b>\\0</b>") |> 
      str_replace("<b>\\*\\*</b>", "<br><br><b>\\*\\*</b>") |> 
      str_replace_all("<p class = 'statetxt'>NA</p>", "<p class = 'statetxt'></p>")
  )) |> 
  # addl notes 
  rowwise() |> 
  # put notes into a list; each column will become its own paragraph 
  mutate(addl_lst = lst(c(
    additional_notes, cy_or_fy_notes#, race_ethnicity_notes
  ))) |> 
  mutate(
    additional_notes = paste0(
      "<p class = 'statetxt'>",  
      paste((unlist(addl_lst[!is.na(addl_lst)])), collapse = "</p><p class = 'statetxt'>"), 
      "</p>"
    )
  ) |> 
  select(
    state, 
    probation_metrics, parole_metrics, 
    probation_asterisks, parole_asterisks, 
    additional_notes
  ) 

admin$save_rds_twice(formatted_notes)



# MISSING-NESS NOTES -----------------------------------------------------------
# Reformat data about probation and parole being abolished
# Load info on missing sentence info
missingness_sentences <- read_excel(
  notes_spreadsheet,
  sheet = "DISPLAY_missingness_notes",
  skip = 1
  ) |> 
  clean_names() |> 
  select(state,
         supervision_violation_admissions_graph,
         parole_violation_admissions_graph,
         probation_violation_admissions_graph,
         supervision_violation_population_graph,
         parole_violation_population_graph,
         probation_violation_population_graph) |> 
  distinct() |> 
  mutate(state = gsub('Excel', "", state),
         state = gsub('[()]', "", state),
         state = trimws(state),
         supervision_violation_admissions_graph = gsub('[\"]', '', supervision_violation_admissions_graph),
         parole_violation_admissions_graph      = gsub('[\"]', '', parole_violation_admissions_graph),
         probation_violation_admissions_graph   = gsub('[\"]', '', probation_violation_admissions_graph),
         supervision_violation_population_graph = gsub('[\"]', '', supervision_violation_population_graph),
         parole_violation_population_graph      = gsub('[\"]', '', parole_violation_population_graph),
         probation_violation_population_graph   = gsub('[\"]', '', probation_violation_population_graph))



admin$save_rds_twice(missingness_sentences)


