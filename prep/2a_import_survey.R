
save_RDS_to_sharepoint <- TRUE 


box::use(
  ./box/admin, 
  dplyr[...], 
  forcats[fct_recode, fct_reorder],
  htmltools[...], 
  janitor[clean_names], 
  purrr[pmap, reduce], 
  rlang[set_names], 
  stringr[str_detect, str_remove, str_replace_all, str_remove_all, word, str_sub], 
  tidyr[pivot_longer, pivot_wider]
)


display_metric_natl <- c("total", "supervision", "tech", "new", "prob", "par") 
display_metric_par  <- c("new par" , "tech par" , "par")
display_metric_prob <- c("new prob", "tech prob", "prob")



# **svii_agg** | all aggregate data ---------------------------------------------------

svii_prep <- readRDS(file.path(admin$sp_survey, "Data/raw/combined/svii_main.rds")) |> 
  # filter to only include aggregate data 
  filter(group == "aggregate") |> 
  # remove variables that will not be displayed in app at all 
  filter(word(metric_abbr, 2, -1) %in% c(display_metric_natl, display_metric_par, display_metric_prob)) |> 
  # remove columns that aren't relevant 
  select(-c(group, group_cat, time_period, FY_end)) |> 
  # create new metric_long column; metric will be the 'displayed' 
  # create new columns (data and metric) to match previous iteration 
  mutate(metric_long = metric, .before = metric) |> 
  mutate(
    # create simple metric -- part 1
    # remove total from supervision/new/tech 
    metric = ifelse(
      metric_short == "Total Prison", 
      "Total", 
      str_remove(metric_short, "Total ") 
    ), 
    # create simple metric -- part 2
    # remove par/prob tech from new/tech par/prob 
    metric = ifelse(
      word(metric, 1) %in% c("New", "Technical"), 
      str_remove_all(metric, "Parole |Probation "), 
      metric
    ), 
    metric = factor(
      metric, 
      levels = c("Total", "Supervision Violation", 
                 "Technical Violation", "New Offense Violation", 
                 "Probation Violation", "Parole Violation" )
    ),
    data = paste(
      ifelse(metric_short == "Total Prison", "Total", str_remove(metric_short, "Total ")), 
      type
    ),
    data = fct_reorder(factor(data), as.numeric(metric_long), .na_rm = FALSE), 
  ) |> 
  select(starts_with("state"), year, data, metric, type, n, starts_with("metric_")) |> 
  arrange(state_name, metric_abbr, year) |> 
  group_by(state_name, metric_abbr) |> 
  # calculate yearly change 
  mutate(
    chg1yr = ifelse(
      year == min(year), 
      NA_real_, 
      (n - lag(n))/lag(n)
    ), 
  ) |> 
  ungroup() |> 
  mutate(
    supervision_type = case_when(
      str_detect(metric_abbr, "par") ~ "Parole", 
      str_detect(metric_abbr, "prob") ~ "Probation", 
      str_detect(metric_abbr, "non-supervision") ~ "Neither", 
      TRUE ~ "Both"
    ), 
    violation_type = case_when(
      str_detect(metric_abbr, "new") ~ "New Offense", 
      str_detect(metric_abbr, "tech") ~ "Technical", 
      str_detect(metric_abbr, "non-supervision") ~ "Neither", 
      TRUE ~ "Both"
    ), 
    # match specific text from previous app 
    text = case_when(
      supervision_type %in% c("Both", "Neither")     ~ paste(metric, type), 
      supervision_type %in% c("Parole", "Probation") & violation_type != "Both" ~ paste(supervision_type, violation_type, "Violation", type), 
      supervision_type %in% c("Parole", "Probation") & violation_type == "Both" ~ paste(supervision_type, "Violation", type)
    ), 
    text = fct_reorder(factor(text), as.numeric(metric_long), na.rm = FALSE), 
    tooltip = paste0("<b>", state_name, " - ", year, "</b><br>",
                     metric, " ",
                     type, "<br>",
                     formattable::comma(n, digits = 0), "<br>")
  ) 

# if total is equal to probation or parole, then indicate that the total only includes
#   probation or parole
# want to apply this to supervision, new, and tech metrics 
# (these are the metircs where subtitels should be noted)
# subtitles for value boxes and subtitles for area/bar charts 
# subtitle_vb for supervision/tech/new are subtitles for the VALUE BOXES  
# subtitle_areabar is the subtitle for the area and bar chart on the overview tab of the state dashboard 

subtext_no_par <- "(No Parole Data Available)"
subtext_no_prob <- "(No Probation Data Available)"
subtext_nodata <- "no data" 
subtext_gen_partial <- "(Partial Data Available)"

svii_subtitle_vb <- svii_prep |> 
  select(state_name, year, type, metric_abbr, n) |> 
  mutate(metric_abbr = word(metric_abbr, 2, -1)) |> 
  pivot_wider(names_from = metric_abbr, values_from = n) |> 
  janitor::clean_names() |> 
  mutate(
    txt_supervision  = case_when( # yearly designation, this is for the value boxes 
      !is.na(prob) & !is.na(par) ~ NA_character_, 
      is.na(prob) &  is.na(par) ~ subtext_nodata, 
      !is.na(prob) &  is.na(par) ~ subtext_no_par, 
      is.na(prob) & !is.na(par) ~ subtext_no_prob
    ), 
    txt_tech  = case_when( # yearly designation, this is for the value boxes 
      !is.na(tech_prob) & !is.na(tech_par) ~ NA_character_, 
      is.na(tech_prob) &  is.na(tech_par) ~ subtext_nodata, 
      !is.na(tech_prob) &  is.na(tech_par) ~ subtext_no_par, 
      is.na(tech_prob) & !is.na(tech_par) ~ subtext_no_prob
    ), 
    txt_new  = case_when( # yearly designation, this is for the value boxes 
      !is.na(new_prob) & !is.na(new_par) ~ NA_character_,  
      is.na(new_prob) &  is.na(new_par) ~ subtext_nodata, 
      !is.na(new_prob) &  is.na(new_par) ~ subtext_no_par, 
      is.na(new_prob) & !is.na(new_par) ~ subtext_no_prob
    )
  ) |> 
  # overrides/adjustments 
  mutate(
    # remove '(no probation data)* from tech for Iowa, tech = par_tech b/c prob_tech == 0 (which is converted to NA)
    txt_tech = ifelse(state_name == "Iowa" & type == "Population", subtext_gen_partial, txt_tech)
  ) |> 
  # drop count values
  select(state_name, year, type, starts_with("txt_")) |>
  pivot_longer(c(txt_supervision, txt_tech, txt_new), names_to = "metric_abbr", values_to = "subtitle_vb") |>
  mutate(metric_abbr = paste(tolower(str_sub(type, 1, 1)), str_sub(metric_abbr, 5, -1))) 

svii_subtitle_areabar <- svii_subtitle_vb |> 
  group_by(state_name, type, metric_abbr) |> 
  summarise(
    vec = list(subtitle_vb), 
    n_notna    = length(subtitle_vb[!is.na(subtitle_vb)]), 
    n_unique = length(unique(subtitle_vb)), 
    n_unique_notna = length(unique(subtitle_vb[!is.na(subtitle_vb)])), 
    # vec_view = paste(subtitle_vb, collapse = " | "), 
    vec_unique_view = paste(unique(sort(subtitle_vb, na.last = TRUE)), collapse = " | "), 
    .groups = "drop"
  ) |> 
  rowwise() |> 
  mutate(
    grouped_text = case_when(
      ## all cells are NA --> NA 
      n_notna == 0 ~ NA_character_, 
      ## all cells have text; only one type of text --> unique text 
      n_notna == length(vec) & n_unique_notna == 1 ~  vec[1], 
      ## mix of text and NA
      # NA & unique txt == subtext_nodata --> adj label 
      n_unique == 2 & str_sub(vec_unique_view, 1, nchar(subtext_nodata)) != subtext_nodata ~ str_replace_all(sort(vec[!is.na(vec)])[1], c(`No` = "Partial")),
      # NA & unique txt == subtext_nodata --> PARTIAL DATA AVAILABLE
      n_unique == 2 & str_sub(vec_unique_view, 1, nchar(subtext_nodata)) == subtext_nodata ~ subtext_gen_partial, 
      # all other cases 
      TRUE ~ subtext_gen_partial
    )
  ) |> 
  ungroup() |> 
  mutate(
    letter = word(metric_abbr, 1), 
    metric_sh = word(metric_abbr, 2)
  ) |> 
  select(state_name, type, metric_sh, grouped_text) |> 
  pivot_wider(names_from = metric_sh, values_from = grouped_text) |> 
  rowwise() |> 
  mutate(
    subtitle_areabar = case_when(
      all(is.na(c(supervision, tech, new))) ~ NA_character_, 
      supervision == tech & tech == new ~ supervision, 
      any(c(supervision, tech, new) == subtext_nodata)      ~ subtext_gen_partial, 
      any(c(supervision, tech, new) == subtext_gen_partial) ~ subtext_gen_partial, 
      TRUE ~ subtext_gen_partial
    )
  ) |> 
  ungroup() |> 
  select(state_name, type, subtitle_areabar)

svii_subtitles <- full_join(
  svii_subtitle_vb, 
  svii_subtitle_areabar, 
  by = c("state_name", "type")
) |> 
  # remove place holder text for when there is not data 
  mutate(across(
    c(subtitle_vb, subtitle_areabar), 
    ~ifelse(.x == subtext_nodata, NA_character_, .x)
  )) |> 
  select(state_name, year, metric_abbr, starts_with("subtitle"))


# 6000 rows 
# 50 states
#  6 years 
#  2 types (adm/pop) 
# 10 metrics

svii_agg <- full_join(
  svii_prep, 
  svii_subtitles, 
  by = c("state_name", "year", "metric_abbr")
) |> 
  select(
    starts_with("state_"), 
    year, 
    data, 
    n, chg1yr, 
    text, type, supervision_type, violation_type, 
    subtitle_vb, subtitle_areabar,  
    tooltip, 
    contains("metric")
  )


# save version on sp but don't save on repo; don't need to use in app 
admin$save_rds_twice(svii_agg, save_to_repo = FALSE, save_to_sp = save_RDS_to_sharepoint)

# save in prep folder as it's needed to create highcharts 
saveRDS(svii_agg, "prep/svii_agg.rds")



# **svii_yr** | create df's that determine trend from year to year; and from min year to max year ----

svii_yr <- tibble(
  str_yr = c(seq(min(svii_agg$year)  , max(svii_agg$year)-1), min(svii_agg$year)), 
  end_yr = c(seq(min(svii_agg$year)+1, max(svii_agg$year)),   max(svii_agg$year))
) |> 
  mutate(
    trend_data_name = paste0("trend_data_", substr(str_yr, 3, 4), "_", substr(end_yr, 3, 4)),
    trend_name      = paste0("trend_",      substr(str_yr, 3, 4), "_", substr(end_yr, 3, 4)),
    change_name     = paste0(str_yr, " - ", end_yr), 
    min_yr = min(svii_agg$year), 
    max_yr = max(svii_agg$year)
  )
admin$save_rds_twice(svii_yr, save_to_sp = save_RDS_to_sharepoint)

# **svii_explorer_table** | table with counts, year-to-year changes, trend vec and category -----


# each table should have 1000 rows 
# 50 states 
# 20 metrics (26 metrics - unk x4 - non-sup x2)

# wide table of n (integer count) values 
df_n <- svii_agg |> 
  select(-chg1yr) |>
  mutate(time = as.character(year), .after = year) |> 
  select(state_name, data, time, n) |> 
  pivot_wider(names_from = time, values_from = n)

# wide table of yearly changes 
df_chg1yr <- svii_agg |> 
  select(-n) |> 
  filter(year != min(year)) |> 
  mutate(time = paste(year - 1, "-", year)) |> 
  select(state_name, data, time, chg1yr) |> 
  pivot_wider(names_from = time, values_from = chg1yr)

df_chgrng <- svii_agg |> 
  filter(year %in% range(year)) |> 
  group_by(state_name, metric_abbr) |> 
  mutate(
    chgrng = (n - lag(n))/lag(n)
  ) |> 
  ungroup() |> 
  filter(year == max(year)) |>
  select(state_name, data, chgrng) |> 
  set_names(c(
    "state_name", "data", 
    paste0(svii_yr$min_yr[1], " - ", svii_yr$max_yr[1])
  ))


create_df_trend <- function(str_yr, end_yr, trend_data_name, trend_name){
  svii_agg |> 
    filter(between(year, str_yr, end_yr)) |> 
    group_by(state_name, data) |> 
    # double list is so n_vec works with code below 
    summarise(n_vec = list(list(n)), .groups = "drop") |>
    rowwise() |> 
    mutate(
      vec_nona = list(n_vec[[1]][!is.na(n_vec[[1]])]), 
      length_nona = length(vec_nona), 
      first  = ifelse(length_nona == 0, NA, vec_nona[1]), 
      last   = ifelse(length_nona == 0, NA, vec_nona[length_nona]), 
      trend = case_when(
        first == last ~ "same",
        first >  last ~ "negative",  #trend is negative, decreasing
        first <  last ~ "positive"   #trend is positive, increasing
      )
    ) |> 
    ungroup()  |> 
    select(state_name, data, n_vec, trend) |> 
    set_names(
      "state_name", "data", 
      trend_data_name, 
      trend_name
    )
  
}

df_trend_lst <- pmap(svii_yr[,1:4] |> as.list(), create_df_trend)


df_addlcols <- svii_agg |> 
  select(
    # remove numeric columns
    -where(is.numeric),
    # also remove vars that are based on a specific year  
    -subtitle_vb, -tooltip) |> 
  distinct() 

##TABLE 600 x 38 
##ROWS: 600 = 50 * 12
# - states (50)
# - data (12) = metric (6) * type (2)
##COLUMNS: 38 = 14 + 6*4
# - id variable columns (9)
#     state_name, state_abbr, state_fips, 
#     data, text, type, supervision_type, violation_type, subtitle_areabar
#     metric, metric_abbr, metric_short, metric_long 
# - yearly count (6)
#     2018, 2019, 2020, 2021, 2022, 2023
# - year-to-year changes (6)
#     2018-2019, 2019-2020, 2020-2021, 2021-2022, 2022-2023, 2018-2023
# - year-to-year vectors (6)
#     trend_18_19, trend_19_20, trend_20_21, trend_21_22, trend_22_23, trend_18_23
# - year-to-year trend category (6)
#     trend_18_19, trend_19_20, trend_20_21, trend_21_22, trend_22_23, trend_18_23

# svii_explorer_table0 includes all metrics  
# svii_explorer_table is filtered to included display metrics 


svii_explorer_table0 <- reduce(
    c(list(df_n), list(df_chg1yr), list(df_chgrng), df_trend_lst, list(df_addlcols)
      ), 
    full_join, by = c("state_name", "data")
  )
svii_explorer_table <- svii_explorer_table0 |> 
  filter(word(metric_abbr, 2, -1) %in% display_metric_natl)
admin$save_rds_twice(svii_explorer_table, save_to_sp = save_RDS_to_sharepoint)


# **svii_explorer** | data for hex maps --------------------------------------------
# data for hex map; create year range and min/max values for legend scale 

# 1 year changes 
map_df_1yr <- svii_agg |> 
  filter(year != min(year)) |> 
  rename(chg = chg1yr) |> 
  mutate(
    year_chg = paste0(
      year - 1, 
      " - ", 
      year
    )
  )


# full year range change 
map_df_rng <- svii_explorer_table0 |> 
  select(
    # keep numeric column that shows the year-to-year change (min/max year) 
    # select it first for each rename 
    all_of(filter(svii_yr, str_yr == min_yr, end_yr == max_yr)$change_name), 
    !where(is.numeric),     # remove all numeric cols 
    -starts_with("trend_"), # remove trend character columns 
  ) |> 
  rename(chg = 1) |> 
  mutate(
    year = svii_yr$max_yr[1] + 1, 
    year_chg = paste0(
      svii_yr$min_yr[1], 
      " - ", 
      svii_yr$max_yr[1]
    )
  ) 
  

# svii_explorer0 includes all metrics  
# svii_explorer is filtered to included display metrics  
svii_explorer0 <- bind_rows(
  map_df_1yr, 
  map_df_rng
) |> 
  select(state_name, state_abbr, year, year_chg, data, chg_raw = chg, metric, type, metric_abbr) |> 
  mutate(
    chg_rnd = round(chg_raw*100, 0), 
    chg_label = ifelse(
      is.na(chg_raw), 
      "-", 
      # use roundedval func with accuracy of 1 so that if value is <0.5% and != 0
      # lable will be <1% instead of 0% 
      paste0(admin$roundedval(chg_raw*100, 1), "%")
    ), 
    tooltip = paste0("<b>", state_name, "</b><br>",
                     "Change in ",
                     data, "<br>from ",
                     year_chg, "<br>",
                     chg_label, "<br>"),
  ) |> 
  arrange(year, year_chg, data, state_name) |> 
  group_by(year_chg, data) |> 
  mutate(all_chg_na = ifelse(
    all(is.na(chg_raw)), TRUE, FALSE
    )
  ) |> 
  mutate(# use -1 to round up to nearest tenth
    min_map = ifelse(all_chg_na == TRUE, NA, round(min(chg_rnd, na.rm = TRUE), 0)), 
    max_map = ifelse(all_chg_na == TRUE, NA, round(max(chg_rnd, na.rm = TRUE), 0))
  ) |> 
  mutate(# get absolute value for comparison
    min_map_abs = abs(min_map),
    max_map_abs = abs(max_map),
    min_map_type = ifelse(min_map >= 0, "positive", "negative"),
    max_map_type = ifelse(max_map >= 0, "positive", "negative"), 
    # Has diverging scales when there are neg and pos values which centers the color gradient at zero
    # Has a gradient scale when both the min and max are both negative or both positive
    # Determine the new min and max so that zero is centered
    # For example, If the highest positive value is 20 than the negative value is -20
    # TODO: round to the nearest 5 (instead the nearest 1) ??
    NEW_MAX = case_when(
      min_map_type != max_map_type & max_map_type == "negative" ~ -max(min_map_abs, max_map_abs), 
      min_map_type != max_map_type & max_map_type == "positive" ~  max(min_map_abs, max_map_abs), 
      min_map_type == max_map_type                              ~  max_map 
    ), 
    NEW_MIN = case_when(
      min_map_type != max_map_type & min_map_type == "negative" ~ -max(min_map_abs, max_map_abs), 
      min_map_type != max_map_type & min_map_type == "positive" ~  max(min_map_abs, max_map_abs), 
      min_map_type == max_map_type                              ~  min_map 
    ), 
  ) |> 
  ungroup() 

## 3600 rows 
# 50 states 
#  6 year changes nrow(svii_yr)
#  6 metrics 
#  2 types 
svii_explorer <- svii_explorer0 |> 
  filter(word(metric_abbr, 2, -1) %in% display_metric_natl)

admin$save_rds_twice(svii_explorer, save_to_sp = save_RDS_to_sharepoint)


# **svii_table** | data for table under hex map -------------------------------------
# select variables
# sum by type
# remove probation, parole and other
# create text for table

svii_table <- svii_explorer_table |> 
  filter(
    str_detect(data, "Probation") == FALSE, 
    str_detect(data, "Parole") == FALSE
  ) |> 
  # update var name to match prev version 
  # recoding categories is a carry over from last MCLC app 
  select(-text) |> 
  rename(text = data) |> 
  mutate(
    text = fct_recode(text, `Technical Population` = "Technical Violation Population")
  ) |> 
  select(
    state_name, 
    text, 
    type, 
    metric, 
    # individual year counts 
    all_of(as.character(svii_yr$min_yr[1]:svii_yr$max_yr[1])), 
    # rng change and trends 
    all_of(
      svii_yr |> 
        filter(str_yr == min_yr, end_yr == max_yr) |> 
        select(-ends_with("yr")) |> 
        pivot_longer(cols = everything()) |> 
        arrange(name) |> 
        pull(value)
    )
  )

admin$save_rds_twice(svii_table, save_to_sp = save_RDS_to_sharepoint)



# prob_par_tables ----------------------------------------------------------


svii_prob_par_tables <- svii_explorer_table0 |> 
  filter(word(metric_abbr, 2, -1) %in% c(display_metric_par, display_metric_prob)) |> 
  select(
    state_name, 
    text, 
    type, 
    supervision_type, 
    metric, 
    # individual year counts 
    all_of(as.character(svii_yr$min_yr[1]:svii_yr$max_yr[1])), 
    # rng change and trends 
    all_of(
      svii_yr |> 
        filter(str_yr == min_yr, end_yr == max_yr) |> 
        select(-ends_with("yr")) |> 
        pivot_longer(cols = everything()) |> 
        arrange(name) |> 
        pull(value)
    )
  )


# **svii_par** | data for table under hex map ------------------------------------

svii_par <- svii_prob_par_tables |> 
  filter(supervision_type == "Parole") 
admin$save_rds_twice(svii_par, save_to_sp = save_RDS_to_sharepoint)

# **svii_prob** | data for table under hex map ----------------------------

svii_prob <- svii_prob_par_tables |> 
  filter(supervision_type == "Probation") 
admin$save_rds_twice(svii_prob, save_to_sp = save_RDS_to_sharepoint)

# **svii_valbox** | value boxes for state dashboards ---------------------------

# create subheader for valuebox
# get information on whether probation or parole are excluded from the data

# create value box data
# filter to value box values (total, supervision violations, and technical violations)
# merge info on whether probation or parole are excluded from the data

svii_valbox <- svii_agg |> 
  filter(word(metric_abbr, 2, -1) %in% display_metric_natl[1:4]) |> 
  mutate(
    chg_rnd = round(chg1yr*100, 1), 
    chg_label = ifelse(
      is.na(chg1yr), 
      "-", 
      paste0(admin$roundedval(chg1yr*100, 1), "%")
    ), 
    chg_type = ifelse(chg_rnd > 0, "increase", "decrease")
  ) |> 
  rename(subheader = subtitle_vb) |> 
  #drop metric text col to add other text col 
  select(-text) |> 
  mutate(
    text = case_when(
      is.na(chg_rnd) ~ "", 
      chg_rnd < 0 ~ paste0(HTML("&darr;"), paste0(chg_label, " from ", year-1)), 
      chg_rnd > 0 ~ paste0(HTML("&uarr;"), paste0(chg_label, " from ", year-1))
    ), 
    value_shown = case_when(
      is.na(n) ~ "No Data", 
      TRUE     ~ paste0(formattable::comma(n, digits = 0))
    ), 
    subheader = case_when(
      is.na(subheader) ~ "<br>", 
      value_shown == "No Data" ~ "<br>", 
      TRUE  ~ subheader
    )
  ) 
  
admin$save_rds_twice(svii_valbox, save_to_sp = save_RDS_to_sharepoint)


# **svii_download** | downloadable data  -------------------------------------------------

svii_download <- svii_agg |> 
  filter(
    word(metric_abbr, 2, -1) %in% 
    c("total", "supervision", display_metric_par, display_metric_prob)
  ) |> 
  select(
    state = state_name, 
    metric = text, 
    year, 
    total = n
  ) |> 
  mutate(across(c(state, year), as.character)) 

admin$save_rds_twice(svii_download, save_to_sp = save_RDS_to_sharepoint)
