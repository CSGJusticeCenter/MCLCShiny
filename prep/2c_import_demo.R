

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
  select(group_order = item, group, group_cat, group_name = long_name)


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
      RACE ==   6  ~ "Two"
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
    pop_total_n = pop_total,
    pop_adult_n = pop_adult
  ) |> 
  filter(year %in% c(2022, 2023), state_name %in% state.name) |> 
  arrange(state_name, year, group_cat, group) |> 
  group_by(year, state_name) |> 
  mutate(
    pop_total_perc = pop_total_n/pop_total_n[group == "aggregate"], 
    pop_adult_perc = pop_adult_n/pop_adult_n[group == "aggregate"], 
  ) |> 
  ungroup() 

  


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



svii_demo <- svii |> 
  filter(year %in% c(2022, 2023)) |> 
  select(year, state_name, state_abbr, state_fips, data, metric, type, group, group_cat, n, metric_abbr) |> 
  # pull out aggregate count (used to calc demo proportions) 
  # once cell values is pulled out of row, remove aggregate rows 
  group_by(state_abbr, year, data) |> 
  mutate(n_agg = n[group == "aggregate"])  |> 
  ungroup() |> 
  filter(group != "aggregate") |> 
  # join with demo_cat df to get item orders (for factoring & display) 
  # and also get long names (display names) 
  left_join(demo_cat, by = c("group", "group_cat")) |> 
  # add data suppression if data point is < 5 --> use value of 5 
  mutate(
    supp_val = 0, 
    case_when(
      n < supp_val & n != 0 ~ tibble(n_cnt = supp_val, n_cnt_supp = TRUE), 
      n >=supp_val | n == 0 ~ tibble(n_cnt = n,        n_cnt_supp = FALSE), 
      is.na(n)              ~ tibble(n_cnt = NA_real_, n_cnt_supp = FALSE)
    ), 
    case_when(
      n_agg < supp_val & n_agg != 0 ~ tibble(n_cnt_agg = supp_val, n_cnt_agg_supp = TRUE), 
      n_agg >=supp_val | n_agg == 0 ~ tibble(n_cnt_agg = n_agg,    n_cnt_agg_supp = FALSE), 
      is.na(n_agg)                  ~ tibble(n_cnt_agg = n_agg,    n_cnt_agg_supp = FALSE)
    ), 
    supp_flag = case_when(
      ## percents are for when supp_val = 5
      # no suppression, using real data; includes zeros -- 40.3%
      n_cnt_supp == FALSE & n_cnt_agg_supp == FALSE & n_cnt == 0 & n_cnt_agg == 0 ~ 0.1, # both val & val_agg == 0 | 0.2% -- IN, MA
      n_cnt_supp == FALSE & n_cnt_agg_supp == FALSE & n_cnt == 0 & n_cnt_agg != 0 ~ 0.2, # val == 0 & val_agg != 0 |  6.3%
      n_cnt_supp == FALSE & n_cnt_agg_supp == FALSE & n_cnt != 0 & n_cnt_agg != 0 ~ 0.3, # val != 0 & val_agg != 0 | 33.8%
      # suppressed data -- 5.7 % 
      n_cnt_supp == TRUE & n_cnt_agg_supp == FALSE & n_cnt <  n_cnt_agg ~ 1.1, # 5.5%
      n_cnt_supp == TRUE & n_cnt_agg_supp == FALSE & n_cnt == n_cnt_agg ~ 1.2, # 0.1%  -- HI, IN, MA, NH, NC
      n_cnt_supp == TRUE & n_cnt_agg_supp == TRUE  & n == n_agg         ~ 1.3, # 0.05% -- MA 
      n_cnt_supp == TRUE & n_cnt_agg_supp == TRUE  & n != n_agg         ~ 1.4, # 0.04% -- HI, MA
      # missing data -- 53.9% 
      is.na(n_cnt) & !is.na(n_cnt_agg) ~ 2.1, # 25.0%
      is.na(n_cnt) &  is.na(n_cnt_agg) ~ 2.2  # 28.9% 
    )
  ) |> 
  # create display value 
  mutate(
    case_when(  
      supp_flag == 0.1 ~ tibble(cnt = 0,        cnt_text = "0",             perc = 0,                 perc_text = NA_character_), 
      supp_flag == 0.2 ~ tibble(cnt = 0,        cnt_text = "0",             perc = 0,                 perc_text = "0%"), 
      supp_flag == 0.3 ~ tibble(cnt = n_cnt,    cnt_text = comma(n_cnt, 1), perc = n_cnt/n_cnt_agg,   perc_text = perc_display(n_cnt/n_cnt_agg, 1)), 
      supp_flag == 1.1 ~ tibble(cnt = NA_real_, cnt_text = "*",             perc = n_cnt/n_cnt_agg,   perc_text = perc_display(n_cnt/n_cnt_agg, 1, '<', '*'),), 
      supp_flag == 1.2 ~ tibble(cnt = NA_real_, cnt_text = "*",             perc = (n+0.5)/n_cnt_agg, perc_text = perc_display((n+0.5)/n_cnt_agg, 1, '<', '*')), 
      supp_flag == 1.3 ~ tibble(cnt = NA_real_, cnt_text = "*",             perc = 1,                 perc_text = "100%"), 
      supp_flag == 1.4 ~ tibble(cnt = NA_real_, cnt_text = "*",             perc =(n+0.5)/(n_agg+0.5),perc_text = perc_display((n+0.5)/(n_agg+0.5), 1, '<', '*')), 
      supp_flag > 2    ~ tibble(cnt = NA_real_, cnt_text = NA_character_)
    ), 
  ) |> 
  left_join(
    census_pop, 
    by = join_by(year, state_name, state_abbr, state_fips, group, group_cat)
  ) 
  
  



svii_demo_table <- svii_demo |> 
  mutate(pop = perc_display(pop_total_perc, 1)) |> 
  select(year, state_name, data, type, group, group_cat, cnt = cnt_text, perc = perc_text, pop, group_order) |> 
  pivot_longer(cols = c(cnt, perc), values_to = "display", names_to = "display_type") |> 
  mutate(group = fct_reorder(as.factor(group), group_order))

admin$save_rds_twice(svii_demo_table, save_to_sp = save_RDS_to_sharepoint)


# demo_table_header <- function(text){
#   str_replace_all(text, c(
#     "Unknown s/g" = "Unknown", 
#     "Unknown r/e" = "Unknown", 
#     "AIAN" = "American Indian", 
#     "NHPI" = "Pacific Islander", 
#     "Two" = "Multiple"
#   ))
#   
# }
# 
# 
# 
# demo_reactable <- function(df) {
#   # need width of reactable to be <= 995
#   # data (275) + demo cnts (9*80)  = 995
#   
#   reactable(
#     df, 
#     style = list(fontFamily = "Graphik, sans-serif", fontSize = "1.4rem"), 
#     theme = reactableTheme(
#       cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"), 
#       headerStyle = list(textAlign = "right")
#     ), 
#     compact = TRUE,
#     searchable = FALSE,
#     pagination = FALSE,
#     defaultColDef = colDef(
#       format = colFormat(separators = TRUE), 
#       header = function(value) demo_table_header(value), 
#       minWidth = 80, 
#       align = "right", 
#       na = "-", # using n dash; could also use longer m dash: "–"
#       headerVAlign = "bottom"
#     ),
#     columns = list(
#       data = colDef(
#         name = "Metric",
#         align = "left",
#         minWidth = 235,
#         style = list(fontWeight = "bold")
#       ), 
#       Hispanic = colDef(minWidth = 110), 
#       AIAN = colDef(minWidth = 100), 
#       NHPI = colDef(minWidth = 90), 
#       Two = colDef(minWidth = 90), 
#       `Unknown r/e` = colDef(minWidth = 100)
#     )
#   )
# }
# 
# 
# 
# 
# svii_demo_table |> 
#   filter(year == 2023, type == "Admissions", 
#          state_name == "Alabama", group_cat == "race_ethnicity", 
#          display_type == "cnt") |> 
#   select(data, group, display) |> 
#   arrange(data, group) |> 
#   pivot_wider(names_from = group, values_from = display) |> 
#   demo_reactable()
# 
# svii_demo_table |> 
#   filter(year == 2023, type == "Admissions", 
#          state_name == "Alabama", group_cat == "race_ethnicity") |>
#   select(group, display = pop) |> 
#   distinct() |> 
#   mutate(data = "Residents of State") |> 
#   arrange(data, group) |> 
#   pivot_wider(names_from = group, values_from = display) |>
#   demo_reactable()
#   
# 
# 
# 
# svii_demo_table |> 
#   filter(year == 2023, type == "Admissions", 
#          state_name == "Alabama", group_cat == "race_ethnicity", 
#          display_type == "perc") |> 
#   select(data, group, display) |> 
#   arrange(data, group) |> 
#   pivot_wider(names_from = group, values_from = display) |> 
#   demo_reactable()



