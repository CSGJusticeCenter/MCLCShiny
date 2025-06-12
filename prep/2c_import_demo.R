

save_RDS_to_sharepoint <- FALSE 


box::use(
  ./box/admin, 
  csgjcr[...], 
  dplyr[...], 
  forcats[fct_recode, fct_reorder],
  glue[glue], 
  htmltools[...], 
  janitor[clean_names], 
  purrr[pmap, reduce], 
  readr[read_rds, read_csv, parse_number], 
  readxl[read_excel], 
  rlang[set_names], 
  stringr[str_detect, str_remove, str_replace_all, str_remove_all, word, str_sub], 
  scales[comma, percent], 
  tidyr[pivot_longer, pivot_wider]
)


info_file <- file.path(admin$sp_survey, "Data/raw", "survey_metrics_and_categories.xlsx")

demo_cat <- bind_rows(
  read_excel(info_file, sheet = "race_ethnicity"), 
  read_excel(info_file, sheet = "sex_gender") 
) |> 
  select(group_order = item, group, group_cat, group_name = long_name) |> 
  add_row(
    group_order = 0, group = "aggregate", group_cat = "aggregate", group_name = "Aggregate"
  ) |> 
  mutate(group_order_sep = group_order, 
         group_order = ifelse(group_cat == "sex_gender", group_order_sep + 9, group_order_sep)) |> 
  select(group, group_cat, group_order)


saveRDS(demo_cat, "prep/demo_cat.rds")


# CENSUS DATA ##################################################################

# raw data files
# - jr_data_library/data/raw/census/pep/sc-est2009-alldata6-all.csv
# - jr_data_library/data/raw/census/pep/sc-est2019-alldata6.csv
# - jr_data_library/data/raw/census/pep/sc-est2023-alldata6.csv
# https://github.com/CSGJusticeCenter/jr_data_library/blob/develop/R/pull_clean/census/pep/clean_pep.R
# https://github.com/CSGJusticeCenter/jr_data_library/blob/f1f42e52476fb303ac521dbff14ed7689fd3a3c1/R/pull_clean/census/pep/clean_pep.R#L1
# https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/state/asrh/
pep_state_20_23 <- read_csv(
  csg_sp_path("JR_data_library/data/raw/census/pep/sc-est2023-alldata6.csv"),
  show_col_types = FALSE
) |> 
  mutate(
    adult = between(AGE, 18, 85), # top-coded at 85
    juv   = between(AGE, 10, 17),
    sex = case_match(
      SEX,
      0 ~ "Total",
      1 ~ "Male",
      2 ~ "Female"
    ),
    race_eth = case_when(
      ORIGIN == 2  ~ "Hispanic",
      RACE ==   1  ~ "White",
      RACE ==   2  ~ "Black",
      RACE ==   3  ~ "AIAN",
      RACE ==   4  ~ "Asian",
      RACE ==   5  ~ "NHPI", 
      RACE ==   6  ~  "Two"
    )
  )

# function to clean and reshape from wide to long
# right now each column is a year of estimates, but we want years to be rows
# this will work to group and summarize for any single variable e.g. sex, age, race
clean_pep <- function(df, group_var, var_name) {
  df |>
    group_by(state_fips = STATE, state_name = NAME, {{group_var}}) |>
    summarize(across(starts_with("POPESTIMATE"), sum), .groups = "drop") |>
    pivot_longer(
      -c(state_fips, state_name, {{group_var}}),
      names_to = "year",
      names_transform = parse_number,
      values_to = "n",
    ) |>
    mutate(
      state_abbr = csg_state_convert(state_fips, "fips", "abbr"),
      variable = var_name
    ) |>
    select(
      year,
      state_name,
      state_abbr,
      state_fips,
      variable,
      {{group_var}},
      n
    )
}


# include all sex, not all ethnicities and summarize by combined race and ethnicity
# ORIGIN == 0: all ethnicities, ORIGIN == 1: Not Hispannic, ORIGIN == 2: Hispanic
# so we want to remove the ORIGIN == 0 records because we don't want to double count
total_pop_by_race_eth <- pep_state_20_23 |>
  filter(sex == "Total", ORIGIN != 0) |>
  clean_pep(race_eth, "pop_total")

# include all sex, not all ethnicities, adults only and summarize by combined race and ethnicity
adult_pop_by_race_eth <- pep_state_20_23 |>
  filter(sex == "Total", ORIGIN != 0, adult) |>
  clean_pep(race_eth, "pop_adult")

# combine total, adult, juv pop by race and ethnicity
# pivot to wide
pop_by_race_eth_state <- total_pop_by_race_eth |>
  bind_rows(adult_pop_by_race_eth) |>
  pivot_wider(names_from = "variable", values_from = n) |>
  rename(group = race_eth) |>
  mutate(group_cat = "race_ethnicity")


# include not all sex, all ethnicities and summarize by sex
total_pop_by_sex <- pep_state_20_23 |>
  filter(sex != "Total", ORIGIN == 0) |>
  clean_pep(sex, "pop_total")

# include not all sex, all ethnicities, adults only and summarize by sex
adult_pop_by_sex <- pep_state_20_23 |>
  filter(sex != "Total", ORIGIN == 0, adult) |>
  clean_pep(sex, "pop_adult")

# combine total pop and adult pop by sex
# pivot to wide
pop_by_sex_state <- total_pop_by_sex |>
  bind_rows(adult_pop_by_sex) |>
  pivot_wider(names_from = "variable", values_from = n) |>
  rename(group = sex) |>
  mutate(group_cat = "sex_gender")

# aggregate state by sex to total population for state
pop_state <- pop_by_sex_state |>
  group_by(year, state_name, state_abbr, state_fips) |>
  summarize(across(starts_with("pop"), sum), .groups = "drop") |>
  mutate(group = "aggregate", group_cat = "aggregate")


comp_census <- pop_state |>
  bind_rows(pop_by_sex_state, pop_by_race_eth_state) |>
  mutate(indicator = "Resident population") |>
  select(
    year,
    state_name,
    state_abbr,
    state_fips,
    group,
    group_cat,
    census_n = pop_total # pop_adult
  ) |> 
  filter(year == 2023, state_name %in% state.name) |> 
  arrange(state_name, year, group_cat, group) |>
  group_by(year, state_name) |>
  mutate(
    comp = census_n/census_n[group == "aggregate"],
  ) |>
  ungroup() |> 
  mutate(comparison_source = "Census") |> 
  filter(group != "aggregate") |> 
  select(state_name, group, group_cat, comparison_source, comp)

saveRDS(comp_census, "prep/comp_census.rds")

# PPUS DATA ##################################################################  

# there is no 'supervision' by jurisdiction
# parole + probation > supervision 
# Table 1: Adults under community supervision, 2012-2022
# Adults on Supervision at year end 2022: 3,668,800
# Adults on Probation at year end 2022: 2,990,900
# Adults on Parole at year end 2022: 698,800
# probation (2,990,900) + parole (698,800) = 3,689,700 > supervision (3,668,800)
# diff = 20,900; ~0.6% of reported supervision value 



# Appendix Table 09: Select characteristics of adults on probation, by jurisdiction, 2022
# - jr_data_library/data/raw/bjs/ppus/ppus22/ppus22t09.csv
ppus_prob <- read_csv(
  csg_sp_path("JR_data_library/data/raw/bjs/ppus/ppus22/ppus22at09.csv"), 
  show_col_types = FALSE, 
  skip = 11
) |> 
  select(
    state = 2, 
    aggregate = 3, 
    Male = 4, 
    Female = 5, 
    `Unknown s/g` = 6, 
    White = 8, 
    Black = 9, 
    Hispanic = 10, 
    Other = 11, 
    `Unknown r/e` = 12
  ) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(comparison_source = "PPUS_Probation")

# Appendix Table 13: Selected characteristics of adults on parole, by jurisdiction, 2022
# - jr_data_library/data/raw/bjs/ppus/ppus22/ppus22t13.csv
ppus_par <- read_csv(
  csg_sp_path("JR_data_library/data/raw/bjs/ppus/ppus22/ppus22at13.csv"), 
  show_col_types = FALSE, 
  skip = 11
) |> 
  select(
    state = 2, 
    aggregate = 3, 
    Male = 5, 
    Female = 6, 
    `Unknown s/g` = 7, 
    White = 9, 
    Black = 10, 
    Hispanic = 11, 
    Other = 12, 
    `Unknown r/e` = 13
  ) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(comparison_source = "PPUS_Parole")



comp_ppus <- bind_rows(ppus_prob, ppus_par) |> 
  mutate(across(
    c(-state, -comparison_source), 
    ~readr::parse_number(.x, na = c("", "NA", "..", "~"))
    # converts above listed into NA, converts <10 --> 10 
  )) |> 
  rowwise() |> 
  mutate(
    footnote_start = stringr::str_locate(state, "/")[[1]], 
    state_name = str_sub(state, 1, ifelse(is.na(footnote_start), -1, footnote_start-1)), 
  ) |> 
  ungroup() |> 
  filter(state_name %in% state.name) |> 
  select(-state, -footnote_start) |> 
  pivot_longer(cols = -c(state_name, comparison_source), names_to = "group", values_to = "n") |> 
  mutate(
    group_cat = case_when(
      group %in% filter(demo_cat, group_cat == "race_ethnicity")$group ~ "race_ethnicity", 
      group %in% filter(demo_cat, group_cat == "sex_gender")$group ~ "sex_gender",
      group == "aggregate" ~ "aggregate"
    )
  ) |> 
  group_by(state_name, comparison_source) |>
  mutate(comp = n/n[group == "aggregate"]) |> 
  ungroup() |> 
  filter(group != "aggregate") |> 
  select(state_name, group, group_cat, comparison_source, comp)

saveRDS(comp_ppus, "prep/comp_ppus.rds")

# SVII DATA ##################################################################

# svii is not needed on repo, but is for creation of demo tables 
svii <- readRDS("prep/svii.rds") 



PERC_ACC <- 1
ndigits <- -log(PERC_ACC, base = 10)


svii_demo <- svii |> 
  filter(
    year %in% c(2022, 2023), 
    # do NOT show combined supervision numbers (par + prob) 
    !word(metric_abbr, 2, -1) %in% c("supervision", "tech", "new")
  ) |> 
  select(year, state_name, state_abbr, state_fips, 
         type, data, metric_abbr, group, group_cat, n) |> 
  # create abbreviation for metrics that does NOT have type designation 
  mutate(data_abbr = word(metric_abbr, 2, -1), .after = "data") |> 
  # pivot wider so each year is it's own column 
  pivot_wider(names_from = year, values_from = n) |> 
  # create the combined n value (sum of 2022 & 2023) and determine flag 
  mutate(case_when(
    !is.na(`2022`) & !is.na(`2023`) ~ tibble(n = `2022` + `2023`, flag = 1), 
    !is.na(`2022`) &  is.na(`2023`) ~ tibble(n = `2023`,          flag = 2), 
     is.na(`2022`) & !is.na(`2023`) ~ tibble(n = `2022`,          flag = 3), 
     is.na(`2022`) &  is.na(`2023`) ~ tibble(n = NA_real_,        flag = 4)
  )) |> 
  group_by(state_name, metric_abbr) |> 
  # calculate the proportion of each metric 
  # calculate 'perc' rounded to the nearest percent 
  mutate(
    prop = case_when(
      n[group == "aggregate"] == 0 ~ NA_real_, 
      TRUE ~ n/n[group == "aggregate"] 
    ), 
    perc = round(prop*100, digits = ndigits), 
  ) |> 
  ungroup() |> 
  # no longer needs aggregate values 
  filter(group != "aggregate") |> 
  # add comparison source (which data should be compared to for RRI's) 
  mutate(
    comparison_source = case_when(
      data_abbr == "total" ~ "Census", 
      data_abbr %in% c("par",  "tech par",  "new par") ~ "PPUS_Parole", 
      data_abbr %in% c("prob", "tech prob", "new prob")~ "PPUS_Probation"
    )
  ) |> 
  left_join(
    bind_rows(comp_census, comp_ppus), 
    by = join_by(state_name, group, group_cat, comparison_source)
  ) |>
  # create perc value that is rounded; pull out unknown percent 
  group_by(state_name, metric_abbr) |> 
  mutate(
    compperc = round(comp*100, digits = ndigits) , 
    compunk = case_when(
      group_cat == "race_ethnicity" ~ compperc[group == "Unknown r/e"], 
      group_cat == "sex_gender"     ~ compperc[group == "Unknown s/g"]
    )
  ) |> 
  ungroup() |> 
  # calculate RRI's with ROUNDED VALUES 
  mutate(
    rri = case_when(
      group %in% c("Other", "Unknown r/e", "Unknown s/g", "Diverse") ~ NA_real_ , 
      compperc == 0 ~ NA_real_, # if value is 0 --> CAN'T CALC RRI B/C CAN'T DIVIDE BY ZERO 
      compunk > 15 ~ NA_real_, # if the unknown perc of comparison group is greater than 15% --> don't calc or highlight 
      !is.na(perc) & !is.na(compperc) ~ perc/compperc, 
      TRUE ~ NA_real_
    )
  ) |> 
  # update text (need to specify total prison admission/population) 
  mutate(
    data_order = as.numeric(data), 
    data = str_replace_all(data, c("Admissions" = "Prison Admissions", 
                                   "Population" = "Prison Population")), 
    data = fct_reorder(factor(data), data_order)
  ) |> 
  select(-data_order)


display_data_text <- svii_demo |> 
  distinct(type, metric_abbr, data_abbr, data) |> 
  mutate(data_order = as.numeric(data)) |> 
  # add rows for comparison populations 
  add_row(
    type = "Admissions", 
    metric_abbr = "a total comp", 
    data_abbr = "total comp", 
    data = "State Population", # display name 
    data_order = 0.5 # want in front of a_total 
  ) |> 
  add_row(
    type = "Admissions", 
    metric_abbr = "a prob comp", 
    data_abbr = "prob comp", 
    data = "State Probation Population", # display name 
    data_order = 1.5 # want in from of a_prob
  ) |>
  add_row(
    type = "Admissions", 
    metric_abbr = "a par comp", 
    data_abbr = "par comp", 
    data = "State Parole Population", # display name 
    data_order = 4.5 # want in front of a_par
  ) |> 
  # only for population section 
  add_row(
    type = "Population", 
    metric_abbr = "p total comp", 
    data_abbr = "total comp", 
    data = "State Population", # display name 
    data_order = 7.5 # want in front of p_total
  ) |> 
  add_row(
    type = "Population", 
    metric_abbr = "p prob comp", 
    data_abbr = "prob comp", 
    data = "State Probation Population", # display name 
    data_order = 8.5 # want in front of p_prob
  ) |>
  add_row(
    type = "Population", 
    metric_abbr = "p par comp", 
    data_abbr = "par comp", 
    data = "State Parole Population", # display name 
    data_order = 11.5 # want in front of p_par
  ) |> 
  arrange(data_order) |> 
  mutate(across(c(metric_abbr, data), ~fct_reorder(factor(.x), data_order))) 
 
# svii_demo_table_prep ----------------------------------------------------------


# ## admissions highlight 
# svii_adm_highlight <- svii_demo |> 
#   select(state_name, type, data_abbr, group, group_cat, perc) |> 
#   pivot_wider(names_from = type, values_from = perc) |> 
#   # highlight instances where admissions perc is greater than population perc 
#   mutate(
#     highlight = case_when(
#       Admissions/Population > 1 ~ TRUE, 
#       TRUE ~ FALSE 
#     )
#   ) |> 
#   mutate(metric_abbr = paste0("a ", data_abbr)) |> 
#   select(state_name, metric_abbr, group, group_cat, highlight, Admissions, Population) 
# 
# ## population highlight 
# svii_pop_highlight <- svii_demo |> 
#   filter(type == "Population") |> 
#   select(state_name, metric_abbr, group, group_cat, perc, compperc, rri) |> 
#   mutate(
#     highlight = case_when(
#       is.na(rri) ~ FALSE, 
#       !is.na(rri) & rri <= 1 ~ FALSE, 
#       !is.na(rri) & rri >  1 ~ TRUE
#     ), 
#     .after = group_cat
#   )


svii_highlight <- svii_demo |> 
  select(state_name, metric_abbr, group, group_cat, perc, compperc, rri) |> 
  mutate(
    highlight = case_when(
      is.na(rri) ~ FALSE, 
      !is.na(rri) & rri <= 1 ~ FALSE, 
      !is.na(rri) & rri >  1 ~ TRUE
    ), 
    .after = group_cat
  )



perc_display <- function(prop, acc = 1){
  ndigits <- -log(acc, base = 10)
  val <- prop*100
  rnd_val <- round(val, digits = ndigits)
  case_when(
    val == 0 ~ "0%", 
    val != 0 & rnd_val == 0 ~ glue("<{acc}%"), 
    val != 0 & rnd_val != 0 ~ glue("{rnd_val}%"), 
    !is.na(val) ~ NA_character_
  )
}


svii_demo_table_prep <- svii_demo |> 
  bind_rows(
    comp_census                                                |> mutate(metric_abbr = "a total comp") |> rename(prop = comp), 
    comp_ppus |> filter(comparison_source == "PPUS_Probation") |> mutate(metric_abbr = "a prob comp")  |> rename(prop = comp), 
    comp_ppus |> filter(comparison_source == "PPUS_Parole")    |> mutate(metric_abbr = "a par comp")   |> rename(prop = comp), 
    comp_census                                                |> mutate(metric_abbr = "p total comp") |> rename(prop = comp), 
    comp_ppus |> filter(comparison_source == "PPUS_Probation") |> mutate(metric_abbr = "p prob comp")  |> rename(prop = comp), 
    comp_ppus |> filter(comparison_source == "PPUS_Parole")    |> mutate(metric_abbr = "p par comp")   |> rename(prop = comp) 
  ) |> 
  # join with demo categories to get set order 
  left_join(demo_cat, by = join_by(group, group_cat)) |> 
  select(-data, -type, -data_abbr) |> 
  # join with data text to get set order 
  left_join(display_data_text, by = join_by(metric_abbr)) |> 
  # join with df's that specify if cell should be highlighted in some way 
  left_join(
    #bind_rows(svii_adm_highlight, svii_pop_highlight) |> 
    svii_highlight |> 
      select(state_name, metric_abbr, group, group_cat, highlight), 
    by = join_by(state_name, metric_abbr, group, group_cat)
  ) |> 
  mutate(
    group       = fct_reorder(factor(group), group_order), 
    metric_abbr = fct_reorder(factor(metric_abbr), data_order), 
    data        = fct_reorder(factor(data), data_order)
  ) |> 
  arrange(state_name, group, metric_abbr) |> 
  ## assign table number for easy filtering 
  mutate(
    table = case_when(
      data_abbr %in% c("total comp", "total") ~ 1, 
      data_abbr %in% c("par comp", "par", "tech par", "new par") ~ 2, 
      data_abbr %in% c("prob comp", "prob", "tech prob", "new prob") ~ 3
    )
  ) |> 
  ## adjust display value 
  mutate(prop = perc_display(prop, acc = 1)) |> 
  select(state_name, table, type, metric_abbr, group_cat, data, group, prop, highlight)  
  


admin$save_rds_twice(svii_demo_table_prep, save_to_sp = save_RDS_to_sharepoint)



# TEXT -----------------------------------------------------------------------


demo_rri_to_bullets <- function(STATE, TYPE, GROUPCAT){
  
  rri_list  <- svii_demo |> 
    filter(state_name == STATE, type == TYPE, group_cat == GROUPCAT) |> 
    #filter(state_name == "Alabama", type == "Admissions", group_cat == "race_ethnicity") 
    filter(!is.na(rri) & round(rri, 1) > 1) |> 
    arrange(desc(rri)) |>
    top_n(2, rri) |>
    mutate(
      rri_text_group_name = case_when(
        group == "White" ~ "White people", 
        group == "Black" ~ "Black people", 
        group == "Hispanic" ~ "Hispanic people", 
        group == "AIAN" ~ "American Indian people", 
        group == "Asian" ~ "Asian people", 
        group == "NHPI" ~ "Pacific Islander people", 
        group == "Male" ~ "Men", 
        group == "Female" ~ "Women"
      ), 
      rri_text_adm_or_pop = case_when(
        type == "Admissions" ~ "admitted to prison",
        type == "Population" ~ "incarcerated"
      ), 
      rri_text_violation = case_when(
        word(metric_abbr, 2, -1) == "total" ~ "",
        word(metric_abbr, 2, -1) != "total" ~ paste0(" for a ", tolower(word(data, 1, -3)))
      ),
      rri_text_population = case_when(
        comparison_source == "Census"         ~ paste0(state_name, " state population"),
        comparison_source == "PPUS_Parole"    ~ paste0(state_name, " parole population"),
        comparison_source == "PPUS_Probation" ~ paste0(state_name, " probation population"),
      ),
      RRI_TEXT = paste0(
        rri_text_group_name,
        " are ",
        round(rri, digits = 1),
        " times more likely to be ",
        rri_text_adm_or_pop,
        rri_text_violation,
        " than their share of the ",
        rri_text_population
      )
    ) |> 
    pull(RRI_TEXT)
  
  
  if (length(rri_list) == 0){
    out <- "This state did not provide sufficent demographic data to determine any relative rates."
  } else {
    out <- paste0(
      "<ul><li>", 
      paste(rri_list, collapse = "</li><li>"), 
      "</li></ul>"
    )
  }
  
}


notes <- openxlsx::read.xlsx( # need to use openxlsx; formatting issue 
  file.path(admin$sp_survey, "Data", "raw", "SVII_2024_Survey_Notes.xlsx"),
  sheet = "DISPLAY_formatted_notes"
) |> 
  as_tibble() |> 
  clean_names() |> 
  select(state_name = state, race_ethnicity = race_ethnicity_notes, sex_gender = sex_gender_notes) |> 
  pivot_longer(cols = -state_name, values_to = "state_notes", names_to = "group_cat") |> 
    mutate(
      state_notes = paste0(
      "<div class = 'notetxt' style = 'text-align: left;'>", 
      "<p>",  
      ifelse(is.na(state_notes), "", state_notes), 
      "</p></div>"
    ) |> str_replace_all("\\n", "</p><p>") 
  ) 




make_link <- function(href, text){
  
  paste0(
    "<a class = 'datasource' href = '", href, "' target = '_blank'>", 
    text, 
    "</a>"
  )
}


# "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/state/asrh/", 
census_link <- make_link(
  "https://www.census.gov/data/datasets/time-series/demo/popest/2020s-state-detail.html", 
  "U.S. Census Bureau American Community Survey, 2023"
)


ppus_link <- make_link(
  "https://bjs.ojp.gov/library/publications/probation-and-parole-united-states-2022", 
  "Probation and Parole in the United States, 2022"
)

demo_highlight_desc <- paste0(
 # "<div class = 'notetxt' style = 'text-align: left;'>",
  "<p><span class = 'highlight'>Bold orange text</span>", 
  " indicates the percentage of a group for a superivsion metric", 
  " is greater than the percentagae for the population group (top row)",
  " and the percentage of unknown for the population group is less than 15%", 
  "</p>"
#  "</div>"
)


demo_posttext_source <- paste0(
    "<div class = 'notetxt' style = 'text-align: left;'>",
    demo_highlight_desc, 
    "<p>Demographic percentages are created from combining the values for 2022 and 2023", 
    " from each state and then calculating the percentage of each demographic group for a given metric.</p>", 
    "<ol style = 'padding-left: 1em;'>", 
    "<li>State population data is from ", census_link, "</li>", 
    #"<br>", 
    "<li>State parole population data is from the BJS report ", ppus_link, ", Appendix Table 13</li>", 
    #"<br>", 
    "<li>State probation population data is sourced from the BJS report ", ppus_link, ", Appendix Table 9</li>",  
    "</ol>", 
    "</div>"
  )


re_static_note <- paste0(
  "<div class = 'notetxt' style = 'text-align: left; font-size: 0.9em !important;'>", 
  "<p><i>", 
  "States vary in how they collect and report information about race and ethnicity.<br>", 
  "Some states use the race or ethnicity recorded by intake officers, ", 
  "while other states allow individuals to identify their own race and ", 
  "ethnicity.  Furthermore, some states combine race and ethnicity into ", 
  "one category, while others separate them into two distinct items. ", 
  "These differences can result in an incomplete count of Hispanic ", 
  "individuals and those of mixed race in state reports. The US Census ", 
  "allows individuals to self-identify their race and ethnicity ", 
  "separately. The variation in practices across states and between ", 
  "state and Federal data reporting systems may result in a deflated ", 
  "RRI for Hispanic individuals. These differences can result in an ", 
  "incomplete count of Hispanic individuals and those of mixed race in ", 
  "state reports. Conversely, it can result in an overcount of people ", 
  "of other races, especially non-Hispanic White people.", 
  "</p></i>", 
  "</div>"
)


sg_static_note <- paste0(
  "<div class = 'notetxt' style = 'text-align: left; font-size: 0.9em !important;'>", 
  "<p><i>", 
  "States vary in how they collect and report information about sex and gender.<br>", 
  "The majority of states only have two categories, usually 'Male' and 'Female'.", 

  "</p></i>", 
  "</div>"
)

svii_demo_text <- tidyr::crossing(
  state_name = state.name, 
  type = c("Admissions", "Population"), 
  group_cat = c("race_ethnicity", "sex_gender")
  ) |> 
  left_join(notes, by = join_by(state_name, group_cat)) |> 
  rowwise() |> 
  mutate(
    demo_rri_text = demo_rri_to_bullets(state_name, type, group_cat)
  ) |> 
  ungroup() |> 
  mutate(
    pretext =   paste0(
      "<div class = 'notetxt' style = 'text-align: left;'>",
      demo_rri_text, 
      "</div>"
    ), 
    posttext = case_when(
      group_cat == "sex_gender"     ~ paste0(state_notes, demo_posttext_source, sg_static_note),
      group_cat == "race_ethnicity" ~ paste0(state_notes, demo_posttext_source, re_static_note)
    )
  ) |> 
  ungroup() |> 
  select(-state_notes)

admin$save_rds_twice(svii_demo_text, save_to_sp = save_RDS_to_sharepoint)


# KY 
# MI  r/e 
# NV - adm s/g 
# NM - pop 
# OH
# RI - adm r/e






