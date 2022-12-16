
# need to run highchart.R before hand to create charts and pathways 


box::use(
    prep/box/admin
  , glue[glue]
  , purrr[...]
  , dplyr[...]
  , highcharter[...]
  , htmlwidgets[saveWidget]
  , webshot2[webshot]
)

admin$mylog("!!START SAVING HIGHCHARTS AS PNGS")


## functions 

add_st_name <- function(hc_obj, state = NA){
  
  org_title <- hc_obj$x$hc_opts$title$text
  thisstate <- ifelse(!is.na(state), state, hc_obj$x$hc_opts$series[[1]]$data[[1]]$state)
  
  new_title <- paste0(thisstate, " ", org_title)
  
  #admin$mylog(glue("Add state to title: {org_title} plot for {thisstate}"))
  
  hc_obj %>% 
    hc_title(text = new_title)
  
}


save_state_png <- function(hc_obj, folderpath, id, title){
  
  admin$mylog(glue("Save plot: {title} for {id}"))
  
  temp <- tempfile(fileext = ".html")
  saveWidget(hc_obj, file = temp, selfcontained = TRUE)
  webshot(
      url = temp
    , file = file.path(folderpath, glue("{id}_{title}.png"))
    , zoom = 4
    , vwidth = 500
    , vheight = 500
    , delay = 1
  )
  
  
}


save_map_png <- function(hc_obj, folderpath, id, title){
  
  admin$mylog(glue("Save plot: {title} for {id}"))
  
  temp <- tempfile(fileext = ".html")
  saveWidget(hc_obj, file = temp, selfcontained = TRUE)
  webshot(
    url = temp
    , file = file.path(folderpath, glue("Change_{id}_{title}.png"))
    , delay = 1
  )
  
  
}

## folders 

theseFOLDERS <- c("sharepoint" = file.path(admin$sp_data, "plots"), "app" = "app/data/plots")
savefolder <- theseFOLDERS[1]
copyfolder <- theseFOLDERS[2]


## remove pngs from sharepoint and app  
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


admin$mylog("PROBATION VIOLATION POPULATIONS")
walk(
  states_list, 
  ~save_state_png(
      add_st_name(probation_bar_pop[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Probation_Violation_Population_by_Type")
)



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



##########
# MAPS Admissions - loops are separate for now because of timeout issues
##########

admin$mylog("ADMISSIONS MAP 2018-2019")
# 2018-2019
walk(
   metrics_list, 
  ~save_map_png(
      adm_maps_2018_2019[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2018 - 2019")
)

admin$mylog("ADMISSIONS MAP 2018-2021")
# 2018-2021
walk(
  metrics_list, 
  ~save_map_png(
      adm_maps_2018_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2018 - 2021")
)


admin$mylog("ADMISSIONS MAP 2019-2020")
# 2019-2020
walk(
  metrics_list, 
  ~save_map_png(
      adm_maps_2019_2020[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2019 - 2020")
)

admin$mylog("ADMISSIONS MAP 2020-2021")
# 2020-2021
walk(
  metrics_list, 
  ~save_map_png(
      adm_maps_2020_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Admissions_2020 - 2021")
)


##########
# MAPS POPULATION
##########

admin$mylog("POPULATIONS MAP 2018-2019")
# 2018-2019
walk(
  metrics_list, 
  ~save_map_png(
      pop_maps_2018_2019[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2018 - 2019")
)

admin$mylog("POPULATIONS MAP 2018-2021")
# 2018-2021
walk(
  metrics_list, 
  ~save_map_png(
      pop_maps_2018_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2018 - 2021")
)

admin$mylog("POPULATIONS MAP 2019-2020")
# 2019-2020
walk(
  metrics_list, 
  ~save_map_png(
      pop_maps_2019_2020[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2019 - 2020")
)

admin$mylog("POPULATIONS MAP 2020-2021")
# 2020-2021
walk(
  metrics_list, 
  ~save_map_png(
      pop_maps_2020_2021[[.x]]
    , folderpath = savefolder
    , id = .x
    , title = "Population_2020 - 2021")
)


#########################
## copy over pngs from sharepoint to app 
walk(
  list.files(savefolder, pattern = "*.png")
  , ~file.copy(
    from = file.path(savefolder, .x)
    , to = file.path(copyfolder, .x)
    , overwrite = TRUE
  )
)

admin$mylog("!!END SAVING HIGHCHARTS AS PNGS")