

save_RDS_to_sharepoint <- TRUE  


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


# pop_state |>
#   bind_rows(pop_by_sex_state, pop_by_race_eth_state) |>
#   mutate(indicator = "Resident population") |>
#   select(
#     year,
#     state_name,
#     state_abbr,
#     state_fips,
#     group,
#     group_cat,
#     pop_adult, 
#     pop_total
#   ) |> 
#   filter(year == 2023, state_name %in% state.name) |>
#   arrange(state_name, year, group_cat, group) |>
#   group_by(year, state_name) |>
#   mutate(
#     total = 100*round(pop_total/pop_total[group == "aggregate"], 2),
#     adult = 100*round(pop_adult/pop_adult[group == "aggregate"], 2), 
#     comp = round(total - adult, 0)
#   ) |>
#   ungroup() |> 
#   count(comp) |> 
#   mutate(
#     perc = (n/sum(n))*100
#   )

# 60% adult = total when rounded to the nearest percentage 
# majority of differences are +/-1% (~24%)



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
  select(state_name, group, group_cat, comparison_source, ncomp = census_n, comp)


# PPUS DATA ##################################################################  

# https://www.ojp.gov/library/publications/probation-and-parole-united-states-2023
# there is no 'supervision' by jurisdiction
# parole + probation > supervision 
# Table 1: Adults under community supervision, 2013-2023
# Adults on Supervision at year end 2023: 3,772,000
# Adults on Probation at year end 2023: 3,103,400
# Adults on Parole at year end 2023: 680,400
# probation (3,103,400) + parole (680,400) = 3,783,800 > supervision (3,772,000)
# diff = 11,800; ~0.3% of reported supervision value 
# there are some states (ex. MN) where someone can be on BOTH prob & parole 


# Appendix Table 10: Selected characteristics of adults on probation, by jurisdiction, 2023
# - jr_data_library/data/raw/bjs/ppus/ppus23/ppus23t10.csv
ppus_prob <- read_csv(
  csg_sp_path("JR_data_library/data/raw/bjs/ppus/ppus23/ppus23at10.csv"), 
  show_col_types = FALSE, 
  skip = 11
) |> 
  select(
    state = 2, 
    aggregate = 3, 
    Male = 4, 
    Female = 5, 
    unknown_sg = 6, 
    notasked_sg = 7, 
    White = 9, 
    Black = 10, 
    Hispanic = 11, 
    Other = 12, 
    unknown_re = 13, 
    notasked_re = 14
  ) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(comparison_source = "PPUS_Probation")


# Appendix Table 13: Selected characteristics of adults on parole, by jurisdiction, 2023
# - jr_data_library/data/raw/bjs/ppus/ppus23/ppus23t13.csv
ppus_par <- read_csv(
  csg_sp_path("JR_data_library/data/raw/bjs/ppus/ppus23/ppus23at13.csv"), 
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
  mutate(comparison_source = "PPUS_Parole")


# combine probation and parole data 
comp_ppus <- bind_rows(ppus_prob, ppus_par) |> 
  mutate(across(
    c(-state, -comparison_source), 
    ~readr::parse_number(.x, na = c("", "NA", "..", "~"))
    # converts above listed into NA, converts <10 --> 10 
  )) |> 
  rowwise() |> 
  # remove footnotes, to get clean state names 
  mutate(
    footnote_start = stringr::str_locate(state, "/")[[1]], 
    state_name = str_sub(state, 1, ifelse(is.na(footnote_start), -1, footnote_start-1)), 
  ) |> 
  ungroup() |> 
  # only include 50 states 
  filter(state_name %in% state.name) |> 
  select(-state, -footnote_start) |> 
  # combine Unknown/Not reported & Not asked into single variable (Unknown) 
  # only for probation 
  rowwise() |> 
  mutate(
    `Unknown s/g` = case_when(
      comparison_source == "PPUS_Parole" ~ `Unknown s/g`, 
      is.na(unknown_sg) & is.na(notasked_sg) ~ NA_real_, 
      TRUE ~ sum(c(unknown_sg, notasked_sg), na.rm = TRUE)
    ), 
    `Unknown r/e` = case_when(
      comparison_source == "PPUS_Parole" ~ `Unknown r/e`, 
      is.na(unknown_re) & is.na(notasked_re) ~ NA_real_, 
      TRUE ~ sum(c(unknown_re, notasked_re), na.rm = TRUE)
    )
  ) |> 
  ungroup() |> 
  # drop 2 variables that were combined into single unknown variable 
  select(-c(unknown_re, notasked_re, unknown_sg, notasked_sg)) |> 
  # pivot longer 
  pivot_longer(cols = -c(state_name, comparison_source), names_to = "group", values_to = "n") |> 
  # assign group_category 
  mutate(
    group_cat = case_when(
      group %in% filter(demo_cat, group_cat == "race_ethnicity")$group ~ "race_ethnicity", 
      group %in% filter(demo_cat, group_cat == "sex_gender")$group ~ "sex_gender",
      group == "aggregate" ~ "aggregate"
    )
  ) |> 
  # calculate proportions for each state and demographic group 
  group_by(state_name, comparison_source) |>
  mutate(comp = n/n[group == "aggregate"]) |> 
  ungroup() |> 
  filter(group != "aggregate") |> 
  select(state_name, group, group_cat, comparison_source, ncomp = n, comp)

# SVII DATA ##################################################################

# svii is not needed on repo, but is for creation of demo tables 
svii <- readRDS("prep/svii.rds") 



PERC_ACC <- 1
ndigits <- -log(PERC_ACC, base = 10)


# filter and set flags 
svii_demo_prep1 <- svii |>
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
    !is.na(`2022`) &  is.na(`2023`) ~ tibble(n = `2022`,          flag = 2), 
     is.na(`2022`) & !is.na(`2023`) ~ tibble(n = `2023`,          flag = 3), 
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
    # state specific calculations
    prop = case_when(
      # NC did not provide gender breakout for a_total in 2022; just use 2023 values to calc percent 
      state_abbr == "NC" & group %in% c("Female", "Male") & metric_abbr == "a total" ~ `2023`/`2023`[group == "aggregate"], 
      TRUE ~ prop
    ), 
    perc = round(prop*100, digits = ndigits), 
  ) |> 
  ungroup() |> 
  # no longer needs aggregate values 
  filter(group != "aggregate") 


# comparison sources added 
svii_demo_prep2 <- svii_demo_prep1 |> 
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
  # update text (need to specify total prison admission/population) 
  mutate(
    data_order = as.numeric(data), 
    data = str_replace_all(data, c("Admissions" = "Prison Admissions", 
                                   "Population" = "Prison Population")), 
    data = fct_reorder(factor(data), data_order)
  ) |> 
  select(-data_order)

# calculate RRI's 
svii_demo <- svii_demo_prep2 |> 
  # calculate comparison to population percent 
  mutate(
    comp_to_pop_perc = case_when(
      group %in% c("Other", "Unknown r/e", "Unknown s/g", "Diverse") ~ NA_real_ ,
      # if value is 0 --> CAN'T CALC RRI B/C CAN'T DIVIDE BY ZERO
      compperc == 0 & comp != 0 & perc > comp ~ prop/comp, 
      compperc == 0 & comp != 0 & perc <= comp ~ NA_real_, 
      # if the unknown perc of comparison group is greater than 15% --> don't calc or highlight
      compunk > 15 ~ NA_real_, 
      # MOST CASES --> use the rounded values to calcualte the comparison 
      !is.na(perc) & !is.na(compperc) ~ perc/compperc,
      TRUE ~ NA_real_
    )
  )  |>
  # calculate RRI with comparison group 
  group_by(state_name, metric_abbr) |> 
  mutate(
    rate = ((n/2)/ncomp)*1e5, 
    rri = case_when(
      group %in% c("Other", "Unknown r/e", "Unknown s/g", "Diverse") ~ NA_real_ , 
      comp  == 0 ~ NA_real_, # if value is 0 --> CAN'T CALC RRI B/C CAN'T DIVIDE BY ZERO 
      # compunk > 15 ~ NA_real_, # if the unknown perc of comparison group is greater than 15% --> don't calc or highlight 
      group_cat == "race_ethnicity" ~ rate/rate[group == "White"], 
      group_cat == "sex_gender" ~ rate/rate[group == "Female"]
    )
  ) |> 
  ungroup() 


# save in prep folder as it's needed to create demo tab outputs  
saveRDS(svii_demo, "prep/svii_demo.rds")


# export csv of RRI's 
svii_demo |> 
  # join with demo categories to get set order 
  left_join(demo_cat, by = join_by(group, group_cat)) |> 
  mutate(
    group = fct_reorder(factor(group), group_order), 
  ) |> 
  arrange(state_name, data, group) |> 
  select(state_name, state_abbr, data, group, rri) |> 
  # filter out White/Female (comparison groups) 
  # filter out Other/Unknown/Diverse categories (not showing RRI's)
  filter(!group %in% c(
    "White", "Female", 
    "Other", "Unknown r/e", "Unknown s/g", "Diverse")
  ) |> 
  mutate(
    # round RRI's to 2 decimals
    rri = round(rri, digits = 2), 
    # adjust group names to match table headers 
    # table headers are adjusted using a function in the app
    group =   stringr::str_replace_all(group, c(
      "AIAN" = "American Indian",
      "NHPI" = "Pacific Islander",
      "Two" = "Multiple"
    ))
  ) |> 
  pivot_wider(names_from = group, values_from = rri) |> 
  readr::write_csv(file = file.path(admin$sp_data, "svii_rri.csv"))






# save version on sp but don't save on repo; don't need to use in app 
admin$save_rds_twice(svii_demo, save_to_repo = FALSE, save_to_sp = save_RDS_to_sharepoint)


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

svii_highlight <- svii_demo |> 
  select(state_name, metric_abbr, group, group_cat, perc, compperc, val = comp_to_pop_perc) |> 
  mutate(
    highlight = case_when(
      is.na(val) ~ FALSE, 
      !is.na(val) & val <= 1 ~ FALSE, 
      !is.na(val) & val >  1 ~ TRUE
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

demo_rri_text_prep <- svii_demo |> 
  select(state_name, state_abbr, metric_abbr, group, group_cat, comparison_source, rri) |> 
  filter(
    # drop comparison groups 
    !group %in% c("White", "Female", "Other", "Unknown r/e", "Unknown s/g"), 
    # drop metrics that are not features in tables 
    !metric_abbr %in% c("a new", "p new", "a tech", "p tech", "a supervision", "p supervision"), 
  ) |> 
  # create factor for groups 
  mutate(group = factor(group, levels = demo_cat$group)) |> 
  arrange(state_name, metric_abbr, group) |> 
  mutate(
    group_text = case_when(
      group == "White" ~ "White people", 
      group == "Black" ~ "Black people", 
      group == "Hispanic" ~ "Hispanic people", 
      group == "AIAN" ~ "American Indians", 
      group == "Asian" ~ "Asians", 
      group == "NHPI" ~ "Pacific Islanders", 
      group == "Two" ~ "Multiracial people", 
      group == "Male" ~ "Males", 
      group == "Female" ~ "females" 
    ), 
    val = case_when(
      rri > 1 ~ comma(rri, accuracy = 0.1, suffix = "&#215 more"), 
      rri < 1 ~ percent(1-rri, accuracy = 1, suffix = "% less")
    ), 
    type_text = case_when(
      word(metric_abbr, 1) == "a" ~ "admitted to prison", 
      word(metric_abbr, 1) == "p" ~ "incarcerated in prison"
    ), 
    bulletpoint = glue("{group_text} are {val} likely to be {type_text}"), 
    # if not an expected higher metric (total, par, prob) --- then add text at end 
    bulletpoint = case_when(
      word(metric_abbr, 2, -1) == "total"     ~ bulletpoint, 
      word(metric_abbr, 2, -1) == "par"       ~ bulletpoint, 
      word(metric_abbr, 2, -1) == "tech par"  ~ paste0(bulletpoint, " for a technical offense parole violation"), 
      word(metric_abbr, 2, -1) == "new par"   ~ paste0(bulletpoint, " for a new offense parole violation"), 
      word(metric_abbr, 2, -1) == "prob"      ~ bulletpoint, 
      word(metric_abbr, 2, -1) == "tech prob" ~ paste0(bulletpoint, " for a technical offense probation violation"), 
      word(metric_abbr, 2, -1) == "new prob"  ~ paste0(bulletpoint, " for a new offense probation violation")
    ), 
    section_header = case_when(
      metric_abbr == "a total" ~ "Prison Admisisons", 
      metric_abbr == "p total" ~ "Prison Population", 
      metric_abbr %in% c("a par", "a new par", "a tech par") ~ "Parole Violations Readmissions", 
      metric_abbr %in% c("p par", "p new par", "p tech par") ~ "Parole Violations Population", 
      metric_abbr %in% c("a prob", "a new prob", "a tech prob") ~ "Probation Violation Admissions", 
      metric_abbr %in% c("p prob", "p new prob", "p tech prob") ~ "Probation Violation Population", 
    ), 
    comp_group = case_when(
      group_cat == "race_ethnicity" ~ "White people's", 
      group_cat == "sex_gender" ~ "females'"
    ), 
    prebullet_text = case_when(
      metric_abbr == "a total" ~ glue("<i>Compared to {comp_group} prison admission rate relative to their <b>community representation:</b></i>"), 
      metric_abbr == "p total" ~ glue("<i>Compared to {comp_group} prison population rate relative to their <b>community representation:</b></i>"), 
      word(metric_abbr, -1) == "par"  ~ glue("<i>Compared to {comp_group} rate <b>among those on parole:</b></i>"),
      word(metric_abbr, -1) == "prob" ~ glue("<i>Compared to {comp_group} rate <b>among those on probation:</b></i>"),
    )

  ) |> 
  select(state_name, state_abbr, metric_abbr, group, group_cat, comparison_source, 
         rri, section_header, prebullet_text, bulletpoint) 


make_pretext_on_rri <- function(CAT, TYPE){
  
  category_text <- case_when(
    CAT == "race_ethnicity" ~ "racial", 
    CAT == "sex_gender" ~ "sex or gender"
  )
  
  comp_group1 <- case_when(
    CAT == "race_ethnicity" ~ "White people's", 
    CAT == "sex_gender" ~ "females'"
  )
  
  comp_group2 <- case_when(
    CAT == "race_ethnicity" ~ "White", 
    CAT == "sex_gender" ~ "female"
  )
  
  comp_group3 <- case_when(
    CAT == "race_ethnicity" ~ "White people", 
    CAT == "sex_gender" ~ "females"
  )

  
  rate_text1 <- case_when(
    TYPE == "Admissions" ~ "prison admissions rates", 
    TYPE == "Population" ~ "incarceration rates"
  )
  
  rate_text2 <- case_when(
    TYPE == "Admissions" ~ "prison admissions rate", 
    TYPE == "Population" ~ "incarceration rate"
  )
  
  type_text1 <- case_when(
    TYPE == "Admissions" ~ "admissions", 
    TYPE == "Population" ~ "incarcerated individuals"
  )
  
  type_text2 <- case_when(
    TYPE == "Admissions" ~ "admitted to prison", 
    TYPE == "Population" ~ "incarcerated"
  )
  
  paste0(
    "<div class = 'notetxt' style = 'text-align: left;'>",
    "<span class = 'notesubtitle'>Understanding Relative Incarceration Rates</span>", 
    "<p>This analysis compares ", rate_text1, " across ", category_text," groups relative ",
    "to their representation in the population of interest, using ", comp_group1, " rate as the baseline. ", 
    "These population-adjusted comparisons reveal true disparites by accounting ", 
    "for different group sizes in the community", 
    "<ul>", 
    "<li>Step 1: Calculate each group's ", rate_text2," by dividing the number of ", 
      type_text1, " by that group's total population", "</li>",
    "<li>Step 2: Create Relative Rate Indices (RRIs) by dividing each group's rate by the ",
      comp_group2, " population's rate", "</li>", 
    "<li>Results: RRIs show how many times more or less likely each group is to be ", type_text2," compared to ", 
      comp_group3, "</li>", 
    "<li>Format: Multiplers like 4&#215 mean four times higher than the rate for the ", comp_group2, " population; ", 
      "percentages like 79% less mean below the rate in the ", comp_group2, " population" ,
    "</ul>", 
    "</p>",
    "</div>"
  )
  
  
}





make_vec_to_bullets <- function(STATE, TYPE, CAT, COMP_GROUP){
  
  df <- demo_rri_text_prep |> 
    filter(
      state_name == STATE, 
      word(metric_abbr, 1) == tolower(str_sub(TYPE, 1, 1)), 
      group_cat == CAT, 
      comparison_source == COMP_GROUP, 
    ) 
  
  vec <- df |> 
    # drop an instances where RRI is not available 
    tidyr::drop_na(rri) |>
    # put the metric number, in case lower metrics are only available 
    # want to select those 
    # if all data is available, we only want to 'lower' number metrics 
    # (factor level first/top) metrics 
    # want to group by group 
    # AL | Pop | Par  | New 
    # AL | Pop | Prob | New  
    # IA | Pop | Prob | New 
    # MN | Pop | Prob | Tech 
    # ND | Adm | Par  | Tech 
    # ND | Adm | Prob | Tech 
    # ND | Pop | Par  | Tech 
    # ND | Pop | Prob | Tech 
    # UT | Pop | Prob | New 
    mutate(metric_n = as.numeric(metric_abbr)) |> 
    group_by(group) |> 
    top_n(-1, metric_n) |> 
    ungroup() |> 
    pull(bulletpoint)
  
  
  if (length(vec) == 0){
    out <- paste0(
      "<i>Insufficient demographic data available</i>"
    )
  } else {
    out <- paste0(
       df$prebullet_text[1],
      "<ul><li>", 
      paste(vec, collapse = "</li><li>"), 
      "</li></ul>"
    )
  }
  
  paste0(
    "<div class='table-title'>", df$section_header[1], "</div>", 
    "<div class = 'notetxt' style = 'text-align: left'>",
    out, 
    "</div>"
  )

  
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
  "https://bjs.ojp.gov/library/publications/probation-and-parole-united-states-2023", 
  "Probation and Parole in the United States, 2023"
)

demo_highlight_desc <- paste0(
 # "<div class = 'notetxt' style = 'text-align: left;'>",
  "<p><span class = 'highlight'>Bold orange text</span>", 
  " indicates that a group’s percentage within a supervision metric is greater ", 
  "than that group’s percentage within the comparison population (top row). ", 
  "Cells will not be highlighted if 15% or more of the comparison population is unknown.", 
  "</p>"
#  "</div>"
)


make_posttext_section <- function(CAT, TYPE){
  

  comp_group_text <- case_when(
    CAT == "race_ethnicity" ~ "White people", 
    CAT == "sex_gender" ~ "females"
  )
  
  type_text <- tolower(TYPE)
  
  
 paste0(
    "<div class = 'notetxt' style = 'text-align: left;'>",
    "<span class = 'notesubtitle'>Data Sources and Calculations</span>", 
    "<p>Demographic percentages are created from combining the values for 2022 and 2023", 
    " from each state and then calculating the percentage of each demographic group for a given metric.</p>", 
    "<ol style = 'padding-left: 1em;'>", 
    "<li>State population data is from ", census_link, "</li>", 
    #"<br>", 
    "<li>State parole population data is from the BJS report ", ppus_link, ", Appendix Table 13</li>", 
    #"<br>", 
    "<li>State probation population data is sourced from the BJS report ", ppus_link, ", Appendix Table 10</li>", 
    "<ul><li>The 'Unknown' category is a combination of the 'Unknown/not reported' and 'Not asked' columns in Appendix Table 10</li><ul>", 
    "</ol>", 
    demo_highlight_desc, 
    "<p>", 
    "When available, the Relative Rate Index (RRI) values are show as bullet points. ", 
    "These values help show how different group compare to a reference group, here ", comp_group_text, ". ", 
    "To calculate it, we first find the rate for each group by dividing the number of events (e.g., prison ", type_text,") ", 
    "by that group’s share of the population (e.g., state population, parole or probation population). ", 
    "Then, we divide the group rate by the comparison group rate, resulting in a simple number that shows whether a group is over or underrepresented.", 
    "</p>", 
    "</div>"
  )
  
  
}


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
    pretext0 = make_pretext_on_rri(group_cat, type), 
    pretext1 = make_vec_to_bullets(state_name, type, group_cat, "Census"), 
    pretext2 = make_vec_to_bullets(state_name, type, group_cat, "PPUS_Parole"), 
    pretext3 = make_vec_to_bullets(state_name, type, group_cat, "PPUS_Probation"), 
    demo_post_text = make_posttext_section(group_cat, type), 
  ) |> 
  ungroup() |> 
  mutate(
    posttext = case_when(
      group_cat == "sex_gender"     ~ paste0(state_notes, demo_post_text, sg_static_note),
      group_cat == "race_ethnicity" ~ paste0(state_notes, demo_post_text, re_static_note)
    )
  ) |> 
  select(-state_notes, -demo_post_text)

admin$save_rds_twice(svii_demo_text, save_to_sp = save_RDS_to_sharepoint)


rm(save_RDS_to_sharepoint)
