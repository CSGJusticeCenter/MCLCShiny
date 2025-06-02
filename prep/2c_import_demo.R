

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
         group_order = ifelse(group_cat == "sex_gender", group_order_sep + 9, group_order_sep))


saveRDS(demo_cat, "prep/demo_cat.rds")


# CENSUS DATA ##################################################################

# raw data files
# - jr_data_library/data/raw/census/pep/sc-est2009-alldata6-all.csv
# - jr_data_library/data/raw/census/pep/sc-est2019-alldata6.csv
# - jr_data_library/data/raw/census/pep/sc-est2023-alldata6.csv
# https://github.com/CSGJusticeCenter/jr_data_library/blob/develop/R/pull_clean/census/pep/clean_pep.R
# https://github.com/CSGJusticeCenter/jr_data_library/blob/f1f42e52476fb303ac521dbff14ed7689fd3a3c1/R/pull_clean/census/pep/clean_pep.R#L1
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


census_pop <- pop_state |>
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
  filter(year %in% c(2022, 2023), state_name %in% state.name) |> 
  arrange(state_name, year, group_cat, group) |>
  group_by(year, state_name) |>
  mutate(
    census_perc = census_n/census_n[group == "aggregate"],
  ) |>
  ungroup()

saveRDS(census_pop, "prep/census_pop.rds")

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
  mutate(supervision_type = "Probation")

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
    Other = 12
  ) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(supervision_type = "Parole")



ppus <- bind_rows(ppus_prob, ppus_par) |> 
  mutate(across(
    c(-state, -supervision_type), 
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
  pivot_longer(cols = -c(state_name, supervision_type), names_to = "group", values_to = "n") |> 
  mutate(
    group_cat = case_when(
      group %in% filter(demo_cat, group_cat == "race_ethnicity")$group ~ "race_ethnicity", 
      group %in% filter(demo_cat, group_cat == "sex_gender")$group ~ "sex_gender",
      group == "aggregate" ~ "aggregate"
    )
  ) |> 
  pivot_wider(names_from = supervision_type, values_from = n) |> 
  rowwise() |> 
  mutate(
    Both = case_when(
      !is.na(Parole) & !is.na(Probation) ~ Parole + Probation, 
       is.na(Parole) &  is.na(Probation) ~ NA_real_, 
      !is.na(Parole) &  is.na(Probation) ~ Parole, 
       is.na(Parole) & !is.na(Probation) ~ Probation
    )
  ) |> 
  ungroup() |> 
  pivot_longer(cols = c(Probation, Parole, Both), names_to = "supervision_type", values_to = "n") |> 
  select(state_name, supervision_type, group, group_cat, ppus_n = n) |> 
  arrange(state_name, supervision_type, group_cat, group) |>
  group_by(state_name, supervision_type) |>
  mutate(
    ppus_perc = ppus_n/ppus_n[group == "aggregate"]
  ) |> 
  ungroup()

saveRDS(ppus, "prep/ppus.rds")

# SVII DATA ##################################################################

# svii is not needed on repo, but is for creation of demo tables 
svii <- readRDS("prep/svii.rds") 


perc_display <- function(prop, acc = 1, pref = NA, suff = NA){
  
  ndigits <- -log(acc, base = 10)
  
  val <- prop*100
  rnd_val <- round(val, digits = ndigits)
  
  txt_val <- case_when(
    val == 0 ~ "0%", 
    val != 0 & rnd_val == 0 ~ glue("<{acc}%"), 
    val != 0 & rnd_val != 0 ~ glue("{rnd_val}%")
    
  )
  
  txt_pref <- case_when(
    # dont repeat carrot, would occur when using suppressed value results in a perc
    # lower than the accuracy level 
    # i.e. n = 4; n_cnt = 5, n_cnt_agg = 600 | 5/600 = 0.833% --> <1%
     str_sub(txt_val, 1, 1) == "<" & pref == "<" ~ "", 
    !is.na(pref) ~ pref, 
    is.na(pref) ~ ""
  )
  
  txt_suff <- ifelse(is.na(suff), "", suff)
  
  display <- paste0(txt_pref, txt_val, txt_suff)
  
  ifelse(is.na(prop), NA_character_, display)
  
}



SUPPRESSION_VALUE <- 5

svii_demo_supp1 <- svii |> 
  filter(year %in% c(2022, 2023)) |> 
  select(year, state_name, state_abbr, state_fips, data, metric, type, group, group_cat, n, metric_abbr, supervision_type) |> 
  # add data suppression if data point is < 5 --> use value of 5 
  group_by(state_abbr, year, data) |> 
  mutate(
    supp_val = SUPPRESSION_VALUE, # any value LESS THAN the supp value is then replaced with the suppressed value 
    case_when( # only need to suppress demographic counts (not aggregate counts) 
      group == "aggregate"  ~ tibble(svii_n = n       , svii_supp = FALSE), 
      n < supp_val & n != 0 ~ tibble(svii_n = NA_real_, svii_supp = TRUE), 
      n >=supp_val | n == 0 ~ tibble(svii_n = n,        svii_supp = FALSE), 
      is.na(n)              ~ tibble(svii_n = NA_real_, svii_supp = FALSE)
    ), 
    supp_flag = case_when(
      # no suppression, using real data
      svii_supp == FALSE & svii_n == 0 & svii_n[group == "aggregate"] == 0 ~ 0.1, 
      svii_supp == FALSE & svii_n == 0 & svii_n[group == "aggregate"] != 0 ~ 0.2, 
      svii_supp == FALSE & svii_n != 0 & svii_n[group == "aggregate"] != 0 ~ 0.3, 
      # suppressed data  
      svii_supp == TRUE & supp_val >  svii_n[group == "aggregate"] ~ 1.1,
      svii_supp == TRUE & supp_val == svii_n[group == "aggregate"] ~ 1.2,
      svii_supp == TRUE & supp_val <  svii_n[group == "aggregate"] ~ 1.3,
      # missing data 
      is.na(svii_n) & !is.na(svii_n[group == "aggregate"]) ~ 2.1,
      is.na(svii_n) &  is.na(svii_n[group == "aggregate"]) ~ 2.2
    )
  )  |> 
  ungroup() 




if (SUPPRESSION_VALUE != 0){
  # after suppression based on values only, check to see if there is only 1 
  # suppressed value within a group 
  # need at least 2 suppressed values so the user cannot back-calculate the value 
  # i.e. 50 = 25 +21 + 4
  # first we suppressed the 4 value b/c it's less than 5
  # but we could still calculate that cell by pulling the aggregate value and subtracting 
  # the non-suppressed cells 
  # fix this by suppressing the next smalled cell: 50 = 25 + * +*
  svii_demo_next_min <- svii_demo_supp1 |> 
    group_by(year, state_name, data, group_cat) |>
    mutate(
      n_cells_suppressed = sum(ifelse(trunc(supp_flag) == 1, TRUE, FALSE)),
    ) |> 
    # only interested in adding suppression if 
    # (1) the number of cells suppressed are == 1 (but not aggregate,)
    # (2) n value is greater than the suppression value (value would not be suppressed otherwise)
    # (3) n value is NOT na
    # (4) not part of the aggregate category 
    filter(n_cells_suppressed == 1, n >= SUPPRESSION_VALUE, !is.na(n), group != "aggregate") |> 
    summarise(n_next_min = min(n), .groups = "drop") 
  
  
  #suppress a secondary value if necessary, adjust supp_flag to 1.5 
  svii_demo_supp2 <- svii_demo_supp1 |> 
    left_join(svii_demo_next_min, by = join_by(year, state_name, data, group_cat)) |> 
    mutate(
      case_when(
         is.na(n_next_min)                   ~ tibble(svii_n = svii_n,   supp_flag = supp_flag, svii_supp = svii_supp), 
        !is.na(n_next_min) & n_next_min == n ~ tibble(svii_n = NA_real_, supp_flag = 1.4,       svii_supp = TRUE), 
        !is.na(n_next_min) & n_next_min != n ~ tibble(svii_n = svii_n,   supp_flag = supp_flag, svii_supp = svii_supp),
        !is.na(n_next_min) & is.na(svii_n)   ~ tibble(svii_n = svii_n,   supp_flag = supp_flag, svii_supp = svii_supp),
      )
    ) |> 
    # drop un-needed columns 
    select(-n_next_min)
  
  # instances where the next min value is the same in multiple cells 
  # do you suppress 1 or both? -- CURRENTLY SUPPRESSING BOTH 
  # if only suppress 1, which one do you pick? 
  n_1.4_unique <-  nrow(svii_demo_next_min)
  n_1.4 <- nrow(filter(svii_demo_supp2, supp_flag == 1.4))
  
  
  svii_demo_supp2 |> 
    filter(supp_flag == 1.4) |> 
    group_by(year, state_name, data, group_cat) |> 
    mutate(n_1.4 = sum(ifelse(supp_flag == 1.4, TRUE, FALSE))) |> 
    ungroup() |> 
    filter(n_1.4 != 1) |> 
    select(year, state_abbr, data, group, n) |> 
    mutate(grp = rep(c("group1", "group2"), (n_1.4 - n_1.4_unique))) |> 
    pivot_wider(names_from = grp, values_from = group) |> 
    arrange(desc(year), state_abbr, data)
  
}

if (SUPPRESSION_VALUE == 0){
  svii_demo_supp2 <- svii_demo_supp1
}



svii_demo <- svii_demo_supp2 |> 
  group_by(year, state_abbr, data) |> 
  mutate(
    svii_perc = case_when(
      svii_n[group == "aggregate"] == 0 ~ NA_real_, 
      TRUE ~ svii_n/svii_n[group == "aggregate"] 
    )
  ) |> 
  ungroup() 
  

# save in prep folder as it's needed to create demo tab outputs  
saveRDS(svii_demo, "prep/svii_demo.rds")
#admin$save_rds_twice(svii_demo, save_to_sp = save_RDS_to_sharepoint)


svii_demo_table <- svii_demo |> 
  filter(year == 2023) |> 
  rename(cnt = svii_n, perc = svii_perc) |> 
  pivot_longer(cols = c(cnt, perc)) |> 
  left_join(demo_cat, by = join_by(group, group_cat)) |> 
  mutate(group = fct_reorder(factor(group), group_order)) |> 
  mutate(
    display = case_when(
      trunc(supp_flag) == 1 ~ "*", 
      is.na(value)   ~ NA_character_, 
      name == "cnt"  ~ comma(value, 1), 
      name == "perc" ~ perc_display(value, 1), 
    )
  ) 


census_demo_table <- census_pop |> 
  filter(year == 2023) |> 
  rename(cnt = census_n, perc = census_perc) |> 
  pivot_longer(cols = c(cnt, perc)) |> 
  left_join(demo_cat, by = join_by(group, group_cat)) |> 
  mutate(group = fct_reorder(factor(group), group_order)) |> 
  mutate(
    display = case_when(
      is.na(value)   ~ NA_character_, 
      name == "cnt"  ~ comma(value, 1), 
      name == "perc" ~ perc_display(value, 1), 
    )
  ) 


ppus_demo_table <- ppus |> 
  rename(cnt = ppus_n, perc = ppus_perc) |> 
  pivot_longer(cols = c(cnt, perc)) |> 
  left_join(demo_cat, by = join_by(group, group_cat)) |> 
  mutate(group = fct_reorder(factor(group), group_order)) |> 
  mutate(
    display = case_when(
      is.na(value)   ~ NA_character_, 
      name == "cnt"  ~ comma(value, 1), 
      name == "perc" ~ perc_display(value, 1), 
    )
  ) 


demo_tables <- list(
  "svii" = svii_demo_table, 
  "census" = census_demo_table, 
  "ppus" = ppus_demo_table
)


admin$save_rds_twice(demo_tables, save_to_sp = save_RDS_to_sharepoint)

