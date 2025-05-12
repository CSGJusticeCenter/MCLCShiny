

save_RDS_to_sharepoint <- FALSE 


box::use(
  ./box/admin, 
  dplyr[...], 
  forcats[fct_recode, fct_reorder],
  glue[glue], 
  htmltools[...], 
  janitor[clean_names], 
  purrr[pmap, reduce], 
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
base_file <- read_rds(file.path(sp_jrdatalib, "census", "pep", "census_pep_state.rds"))



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
  display
  
}



svii_demo <- svii |> 
  filter(year %in% c(2022, 2023)) |> 
  select(-tooltip, -chg1yr) |> 
  #select(state_abbr, year, data, group, group_cat, n) |>
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
    supp_val = 5, 
    case_when(
      n < supp_val & n != 0 ~ tibble(n_cnt = supp_val, n_cnt_supp = TRUE), 
      n >=supp_val | n == 0 ~ tibble(n_cnt = n,        n_cnt_supp = FALSE), 
      TRUE                  ~ tibble(n_cnt = n,        n_cnt_supp = NA)
    ), 
    case_when(
      n_agg < supp_val & n != 0 ~ tibble(n_cnt_agg = supp_val, n_cnt_agg_supp = TRUE), 
      n_agg >=supp_val | n == 0 ~ tibble(n_cnt_agg = n_agg,    n_cnt_agg_supp = FALSE), 
      TRUE                      ~ tibble(n_cnt_agg = n_agg,    n_cnt_agg_supp = NA)
    ), 
    supp_flag = case_when(
      # no suppression, using real data; includes zeros -- 40.3%
      n_cnt_supp == FALSE & n_cnt_agg_supp == FALSE & n_cnt == 0 & n_cnt_agg == 0 ~ 0.1, #  0.2% -- IN, MA
      n_cnt_supp == FALSE & n_cnt_agg_supp == FALSE & n_cnt == 0 & n_cnt_agg != 0 ~ 0.2, #  6.3%
      n_cnt_supp == FALSE & n_cnt_agg_supp == FALSE & n_cnt != 0 & n_cnt_agg != 0 ~ 0.3, # 33.8%
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
    cnt = case_when(
      supp_flag == 0.1 ~ "0", 
      supp_flag == 0.2 ~ "0", 
      supp_flag == 0.3 ~ comma(n_cnt, 1), 
      supp_flag == 1.1 ~ "*", 
      supp_flag == 1.2 ~ "*", 
      supp_flag == 1.3 ~ "*", 
      supp_flag == 1.4 ~ "*",
      supp_flag > 2    ~ NA_character_
    ), 
    perc = case_when(
      supp_flag == 0.1 ~ "", 
      supp_flag == 0.2 ~ "", 
      supp_flag == 0.3 ~ perc_display(n_cnt/n_cnt_agg, 1),  
      supp_flag == 1.1 ~ perc_display(n_cnt/n_cnt_agg, 1, '<', '*'),
      supp_flag == 1.2 ~ perc_display((n+0.5)/n_cnt_agg, 1, '<', '*'), 
      supp_flag == 1.3 ~ "100%", 
      supp_flag == 1.4 ~ perc_display((n+0.5)/(n_agg+0.5), 1, '<', '*'),
      supp_flag > 2    ~ NA_character_
    )
  ) |> 
  pivot_longer(cols = c(cnt, perc), values_to = "display", names_to = "display_type") |> 
  mutate(
    group_order = ifelse(display_type == "perc", group_order + 0.5, group_order), 
    group_place = glue("{display_type}_{group}"), 
    group_place = fct_reorder(as.factor(group_place), group_order)
  )



df <- svii_demo |> 
  filter(year == 2023, type == "Admissions", state_name == "Alabama", group_cat == "race_ethnicity") |> 
  select(data, group_place, display) |> 
  arrange(data, group_place) |> 
  pivot_wider(names_from = group_place, values_from = display)



demo_table_header <- function(text){
  ## adjust display names here 
  temp <- case_when(
    str_detect(text, "cnt_") == TRUE ~ str_remove_all(text, "cnt_"),
    str_detect(text, "perc_") == TRUE ~ "%",
    TRUE ~ text
  )
  str_replace_all(temp, c(
    "Unknown s/g" = "Unknown", 
    "Unknown r/e" = "Unknown", 
    "AIAN" = "Amer Ind/AK Native", 
    "NHPI" = "Native HI/Pacific Isl", 
    "Two" = "Multiple"
  ))
  
}




demo_reactable <- function(df) {
  
  
  # need width of reactable to be <= 995
  # data (275) + demo cnts & perc (9*2*90)  = 1065
  
  reactable(
    df, 
    style = list(fontFamily = "Graphik, sans-serif", fontSize = "1.4rem"), 
    theme = reactableTheme(
      cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"), 
      headerStyle = list(textAlign = "right")
    ), 
    compact = TRUE,
    searchable = FALSE,
    pagination = FALSE,
    defaultColDef = colDef(
      format = colFormat(separators = TRUE), 
      header = function(value) demo_table_header(value), 
      minWidth = 90, 
      align = "right", 
      na = "-", # using n dash; could also use longer m dash: "–"
      headerVAlign = "bottom"
    ),
    columns = list(
      data = colDef(
        name = "Metric",
        align = "left",
        minWidth = 235,
        style = list(fontWeight = "bold")
      ), 
      Hispanic = colDef(
        align = "left",
        minWidth = 235,
      )
    )
  )
}



