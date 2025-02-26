
box::use(
  ./box/admin, 
  dplyr[...], 
  janitor[clean_names], 
  openxlsx[read.xlsx], 
  readxl[read_excel], 
  stringr[str_replace, str_replace_all]
)


# SHOULD BE ABLE TO REMOVE THIS; THIS IS USED IN THE R/E THAT IS BEING REMOVED 
# Load definitions for disparities
disparities_definitions <- read.xlsx(file.path(admin$sp_data_raw, "notes/states_notes_no_data_text.xlsx"),
                                     sheet = "Disparities Definitions")

admin$save_rds_twice(disparities_definitions)

# STATE NOTES (DISPLALY FORMATED NOTES) ----------------------------------------

notes <- openxlsx::read.xlsx( # need to use openxlsx; formatting issue 
    file.path(admin$sp_data_raw, "notes/states_notes_no_data_text.xlsx"),
    sheet = "Formatted Notes 2022"
  ) |> 
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


# get probation related notes
probation_notes <- notes |>
  group_by(state) |>
  summarize(note_lst = list(probation_metrics)) |>
  ungroup() |>
  rowwise() |>
  mutate(
    notes = paste(note_lst, collapse = "</p><p class = 'statetxt'>")
    , notes = paste0("<p class = 'statetxt'>", notes, "</p>")
  ) |>
  ungroup() |>
  select(state, notes)

# get parole related notes
parole_notes <- notes |>
  group_by(state) |>
  summarize(note_lst = list(parole_metrics)) |>
  ungroup() |>
  rowwise() |>
  mutate(
    notes = paste(note_lst, collapse = "</p><p class = 'statetxt'>")
    , notes = paste0("<p class = 'statetxt'>", notes, "</p>")
  ) |>
  ungroup() |>
  select(state, notes)

# get parole asteriks notes
# make asteriks bold
parole_asterisks_notes <- notes |>
  mutate(parole_asterisks = str_replace_all(parole_asterisks, "\\*+", "<b>\\0</b>")) |>
  mutate(parole_asterisks = str_replace(parole_asterisks, "<b>\\*\\*</b>", "<br><br><b>\\*\\*</b>")) |>
  group_by(state) |>
  summarize(note_lst = list(parole_asterisks)) |>
  ungroup() |>
  rowwise() |>
  mutate(
    notes = paste(note_lst, collapse = "</p><p class = 'statetxt'>")
    , notes = paste0("<p class = 'statetxt'>", notes, "</p>")
  ) |>
  ungroup() |>
  select(state, notes) |>
  mutate(notes =
           str_replace_all(notes, "<p class = 'statetxt'>NA</p>", "<p class = 'statetxt'></p>"))


# get probation asteriks notes
# make asteriks bold
probation_asterisks_notes <- notes |>
  mutate(probation_asterisks = str_replace_all(probation_asterisks, "\\*+", "<b>\\0</b>")) |>
  mutate(probation_asterisks = str_replace(probation_asterisks, "<b>\\*\\*</b>", "<br><br><b>\\*\\*</b>")) |>
  group_by(state) |>
  summarize(note_lst = list(probation_asterisks)) |>
  ungroup() |>
  rowwise() |>
  mutate(
    notes = paste(note_lst, collapse = "</p><p class = 'statetxt'>")
    , notes = paste0("<p class = 'statetxt'>", notes, "</p>")
  ) |>
  ungroup() |>
  select(state, notes) |>
  mutate(notes =
           str_replace_all(notes, "<p class = 'statetxt'>NA</p>", "<p class = 'statetxt'></p>"))

# get additional notes
additional_notes <- notes |>
  mutate(
    cy_or_fy_notes   = ifelse(is.na(cy_or_fy_notes), "", cy_or_fy_notes),
    additional_notes = ifelse(is.na(additional_notes), "", additional_notes),
    additional_notes = ifelse(is.na(additional_notes),
                              paste(additional_notes, cy_or_fy_notes, sep = " "),
                              paste(additional_notes, cy_or_fy_notes, sep = " "))) |>
  group_by(state) |>
  summarize(note_lst = list(additional_notes)) |>
  ungroup() |>
  rowwise() |>
  mutate(
    notes = paste(note_lst, collapse = "</p><p class = 'statetxt'>")
    , notes = paste0("<p class = 'statetxt'>", notes, "</p>")
  ) |>
  ungroup() |>
  select(state, notes)

admin$save_rds_twice(parole_notes)
admin$save_rds_twice(probation_notes)
admin$save_rds_twice(parole_asterisks_notes)
admin$save_rds_twice(probation_asterisks_notes)
admin$save_rds_twice(additional_notes)


# MISSING-NESS NOTES -----------------------------------------------------------
# Reformat data about probation and parole being abolished
# Load info on missing sentence info
missingness_sentences <- read_excel(
  file.path(admin$sp_data_raw, "notes/states_notes_no_data_text.xlsx"),
  sheet = "Missingness 2022", skip = 1) |> 
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

# states that don't have graphs because of missing data

# states that are missing data and will not have a graph showing technical and new offense violations
# adm
nt_na_adm1 <- missingness_sentences |>
  filter(!is.na(supervision_violation_admissions_graph))
nt_na_adm <- nt_na_adm1$state

# states that are missing data and will not have a graph showing technical and new offense violations
# pop
nt_na_pop1 <- missingness_sentences |>
  filter(!is.na(supervision_violation_population_graph))
nt_na_pop <- nt_na_pop1$state

# states that are NOT missing data and will have a graph showing technical and new offense violations
# adm
nt_not_na_adm1 <- missingness_sentences |>
  ungroup() |> select(state) |> distinct() |>
  anti_join(nt_na_adm1, by = "state")
nt_not_na_adm <- nt_not_na_adm1$state

# states that are NOT missing data and will have a graph showing technical and new offense violations
# pop
nt_not_na_pop1 <- missingness_sentences |>
  ungroup() |> select(state) |> distinct() |>
  anti_join(nt_na_pop1, by = "state")
nt_not_na_pop <- nt_not_na_pop1$state

# states that are missing data and will not have a parole graph
# adm
parole_na_adm1 <- missingness_sentences |>
  filter(!is.na(parole_violation_admissions_graph))
parole_na_adm <- parole_na_adm1$state

# states that are missing data and will not have a parole graph
# pop
parole_na_pop1 <- missingness_sentences |>
  filter(!is.na(parole_violation_population_graph))
parole_na_pop <- parole_na_pop1$state

# states that are NOT missing data and will have a graph showing technical and new offense parole violations
# adm
parole_not_na_adm1 <- missingness_sentences |>
  select(state) |>
  distinct() |>
  anti_join(parole_na_adm1, by = "state")
parole_not_na_adm <- parole_not_na_adm1$state

# states that are NOT missing data and will have a graph showing technical and new offense parole violations
# pop
parole_not_na_pop1 <- missingness_sentences |>
  select(state) |>
  distinct() |>
  anti_join(parole_na_pop1, by = "state")
parole_not_na_pop <- parole_not_na_pop1$state

# states that are missing data and will not have a probation graph
# adm
probation_na_adm1 <- missingness_sentences |>
  filter(!is.na(probation_violation_admissions_graph))
probation_na_adm <- probation_na_adm1$state

# states that are missing data and will not have a probation graph
# pop
probation_na_pop1 <- missingness_sentences |>
  filter(!is.na(probation_violation_population_graph))
probation_na_pop <- probation_na_pop1$state

# states that are NOT missing data and will have a graph showing technical and new offense probation violations
# adm
probation_not_na_adm1 <- missingness_sentences |>
  select(state) |>
  distinct() |>
  anti_join(probation_na_adm1, by = "state")
probation_not_na_adm <- probation_not_na_adm1$state

# states that are NOT missing data and will have a graph showing technical and new offense probation violations
# pop
probation_not_na_pop1 <- missingness_sentences |>
  select(state) |>
  distinct() |>
  anti_join(probation_na_pop1, by = "state")
probation_not_na_pop <- probation_not_na_pop1$state


admin$save_rds_twice(missingness_sentences)
admin$save_rds_twice(nt_na_adm)
admin$save_rds_twice(nt_na_pop)
admin$save_rds_twice(nt_not_na_adm)
admin$save_rds_twice(nt_not_na_pop) 
admin$save_rds_twice(parole_na_adm) 
admin$save_rds_twice(parole_na_pop)
admin$save_rds_twice(parole_not_na_adm)
admin$save_rds_twice(parole_not_na_pop) 
admin$save_rds_twice(probation_na_adm)
admin$save_rds_twice(probation_na_pop)
admin$save_rds_twice(probation_not_na_adm)
admin$save_rds_twice(probation_not_na_pop) 

