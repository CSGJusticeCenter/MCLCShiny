#######################################
# Project: MCLCShiny
# File: highchart_download.R
# Authors: Mari Roberts, Martha Eichlersmith
# Date last updated: August 31, 2023 (MAR)
# Description:
#    Create and save highcharts WITH LOGO (pngs) so the app loads faster
#######################################

# path to data on research div sharepoint
# make sure sharepoint folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# in your Renviron (usethis::edit_r_environ()), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"


################################################################################




####################################

# STATE REPORTS - State bar chart

####################################

# list of states for function
states <- adm_pop_long$state %>%
  unique() %>%
  sort()

# generate list of state highcharts to call in app (admissions)
all_state_bar_adm <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Admissions") %>%
    group_by(state, year, metric, adm_or_pop, probation_or_parole) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    ungroup() %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Supervision Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart_logo(df1, "Supervision Violation Admissions by Type")
  return(highcharts)
})

# set names of charts
all_state_bar_adm <- setNames(all_state_bar_adm, states)

# generate list of state highcharts to call in app (population)
all_state_bar_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population") %>%
    group_by(state, year, metric, adm_or_pop, probation_or_parole) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    ungroup() %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Supervision Violation Population by Type, {x}"))
  highcharts <- fnc_highchart_state_barchart_logo(df1, "Supervision Violation Population by Type")
  return(highcharts)
})

# set names of charts
all_state_bar_pop <- setNames(all_state_bar_pop, states)






####################################

# STATE REPORTS - Parole bar chart

####################################

# generate list of state highcharts to call in app (admissions)
parole_bar_adm <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Admissions",
           prob_vs_parole == "Parole") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Parole Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_parole_barchart_logo(df1, "Parole Violation Admissions by Type")
  return(highcharts)
})

# set names of charts
parole_bar_adm <- setNames(parole_bar_adm,states)

# generate list of state highcharts to call in app (population)
parole_bar_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population",
           prob_vs_parole == "Parole") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Parole Violations Population by Type, {x}"))
  highcharts <- fnc_highchart_parole_barchart_logo(df1, "Parole Violation Population by Type")
  return(highcharts)
})

# set names of charts
parole_bar_pop <- setNames(parole_bar_pop,states)






####################################

# STATE REPORTS - Probation bar chart

####################################

# generate list of state highcharts to call in app (admissions)
probation_bar_adm <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Admissions",
           prob_vs_parole == "Probation") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Probation Violation Admissions by Type, {x}"))
  highcharts <- fnc_highchart_probation_barchart_logo(df1, "Probation Violation Admissions by Type")
  return(highcharts)
})

# set names of charts
probation_bar_adm <- setNames(probation_bar_adm, states)

# generate list of state highcharts to call in app (population)
probation_bar_pop <- map(.x = states,  .f = function(x) {
  df1 <- adm_pop_long %>%
    filter(state == x &
             adm_or_pop == "Population",
           prob_vs_parole == "Probation") %>%
    group_by(state, year, metric, adm_or_pop) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    filter(metric == "New Offense Violation" | metric == "Technical Violation") %>%
    mutate(total = ifelse(total == 0, NA, total))
  admin$mylog(glue("hc: Probation Violation Population by Type, {x}"))
  highcharts <- fnc_highchart_probation_barchart_logo(df1, "Probation Violation Population by Type")
  return(highcharts)
})

# set names of charts
probation_bar_pop <- setNames(probation_bar_pop, states)



################################################################################

# Save graphs as pngs (with logo and data labels)

################################################################################

admin$mylog("!!START SAVING HIGHCHARTS AS PNGS")


# functions

add_st_name <- function(hc_obj, state = NA){

  org_title <- hc_obj$x$hc_opts$title$text
  thisstate <- ifelse(!is.na(state), state, hc_obj$x$hc_opts$series[[1]]$data[[1]]$state)

  new_title <- paste0(thisstate, " ", org_title)

  hc_obj %>%
    hc_title(text = new_title)

}


save_state_png <- function(hc_obj, folderpath, id, title){

  admin$mylog(glue("Save plot: {title} for {id}"))
  saveWidget(hc_obj, file = "temp.html", selfcontained = TRUE)
  webshot2::webshot(
    url = "temp.html"
    , file = file.path(folderpath, glue("{id}_{title}.png"))
    , zoom = 4
    , vwidth = 500
    , vheight = 500
    , delay = 1
  )

}


save_map_png <- function(hc_obj, folderpath, id, title){

  admin$mylog(glue("Save plot: {title} for {id}"))
  saveWidget(hc_obj, file = "temp.html", selfcontained = TRUE)
  webshot2::webshot(
    url = "temp.html"
    , file = file.path(folderpath, glue("Change_{id}_{title}.png"))
    , delay = 1
  )
}


# folders
theseFOLDERS <- c("app" = "app/data/plots",
                  "sharepoint" = file.path(admin$sp_data, "plots"))
savefolder <- theseFOLDERS[1]
copyfolder <- theseFOLDERS[2]


# remove pngs from sharepoint and app
for (folder in theseFOLDERS){

  walk(list.files(folder, pattern = "*.png"), ~file.remove(file.path(folder, .x)))

}


##########
# State Overview Area Chart
##########

admin$mylog("PRISON ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_area_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Prison_Admissions")
)


admin$mylog("PRISON POPULATION")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_area_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Prison_Population")
)



##########
# State Supervision Violation Bar Chart
##########

admin$mylog("SUPERVISION VIOLATION ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_bar_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Supervision_Violation_Admissions_by_Type")
)

admin$mylog("SUPERVISION VIOLATION POPULATION")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_bar_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Supervision_Violation_Population_by_Type")
)


##########
# Probation Bar Chart
##########


admin$mylog("PROBATION VIOLATION ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(probation_bar_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Probation_Violation_Admissions_by_Type")
)
# 
# 2025-02-24 15:48:28.423388 - Save plot: Admissions_2018 - 2019 for Technical Violation
# Error in `map()`:
#   ℹ In index: 5.
# Caused by error in `s$close()`:
#   ! attempt to apply non-function
# Run `rlang::last_trace()` to see where the error occurred.


admin$mylog("PROBATION VIOLATION POPULATIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(probation_bar_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Probation_Violation_Population_by_Type")
)

# 2025-02-24 15:54:14.734775 - Save plot: Probation_Violation_Population_by_Type for Connecticut
# Error in `map()`:
#   ℹ In index: 7.
# Caused by error in `s$close()`:
#   ! attempt to apply non-function
# Run `rlang::last_trace()` to see where the error occurred.



##########
# Parole Bar Chart
##########

admin$mylog("PAROLE VIOLATION ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(parole_bar_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Parole_Violation_Admissions_by_Type")
)

admin$mylog("PAROLE VIOLATION POPULATIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(parole_bar_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Parole_Violation_Population_by_Type")
)

#########################

# copy over pngs from save folder folder to sharepoint
# walk(
#   list.files(savefolder, pattern = "*.png")
#   , ~file.copy(
#     from = file.path(savefolder, .x)
#     , to = file.path(copyfolder, .x)
#     , overwrite = TRUE
#   )
# )

admin$mylog("!!END SAVING HIGHCHARTS AS PNGS")

