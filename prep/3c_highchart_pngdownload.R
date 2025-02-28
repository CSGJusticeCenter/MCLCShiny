# Description: Create and save highcharts WITH LOGO (pngs) so the app loads faster
# path to data on research div sharepoint
# make sure sharepoint folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# in your Renviron (usethis::edit_r_environ()), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"

# remotes::install_github("rstudio/webshot2")
# issues with getting proper height/width -- try downloading development version 

box::use(
   ./box/admin, 
  , glue[glue]
  , htmlwidgets[saveWidget]
  , webshot2[...]
)

# assumes 3a_highchart_fnc has been run 

# functions for saving plots ---------------------------------------------------


##hex map testing 
# this_filename <- "Change_Total_Admissions_2018 - 2019"
# this_hc <- hex_map_lst[[this_filename]] 
# stateplot <- FALSE 
# lst <- list(names(hex_map_lst))

##area map testing 
this_filename <- "Alabama_Prison_Admissions"
this_hc <- state_area_lst[[this_filename]]
stateplot <- TRUE
lst <- list(names(state_area_lst))

save_hc_png <- function(this_hc, this_filename, stateplot = FALSE, lst = NA){
  
  if (!is.na(lst)){
    n <- which(lst[[1]] == this_filename)
    N <- length(lst[[1]]) 
    prefix <- glue("{str_pad(n, width = nchar(N))}/{N} ")
  } else {
    prefix <- ""
  }
  
  message(glue("{format(Sys.time(), '%X')} SAVE HC to PNG {prefix}-- {this_filename}"))
  
  if (stateplot == FALSE){
    #NATIONAL PLOT, use default values; ?webshot2::webshot
    # outputs are saved 992 x 744
    this_vwidth = 992 #default 
    this_vheight = 744
    this_zoom = 1
  } else {
    # STATE PLOT, adj values 
    # outputs are saved as 1000 x 1000
    # 1000 = 500 (h/w) * 2 (zoom) 
    this_vwidth = 500 
    this_vheight = 500
    this_zoom = 2 
    # old version had zoom of 4; change to 2 so state pngs are ~ savem width as hex pngs 
  }
  
  filename_ext <- paste0(this_filename, ".png")
  save_plots_to <- file.path(      "app/data/plots", filename_ext) 
  # copy_plots_to <- file.path(admin$sp_data, "plots", filename_ext)
  
  # save widget takes the longest 
  # ~30 seconds for an area chart
  saveWidget(this_hc, file = "temp.html", selfcontained = TRUE) 
  webshot2::webshot(
    url = "temp.html", 
    file = save_plots_to, 
    vwidth = this_vwidth,
    vheight = this_vheight,
    delay = 1, 
    zoom = this_zoom
  )
  #file.copy(from = save_plots_to, to = copy_plots_to)
}



# NATL - HEX MAPS (WITH LOGO) ##################################################

hex_map_lst <- pmap(
  # created in 3a_highchart_fnc.R file 
  hex_map_opts |> 
    select(type, year_chg,  metric) |> 
    as.list()
  , 
  function(type, year_chg, metric)
    fnc_hc_hex_map(type, year_chg, metric) |> 
      fnc_adj_map_legend() |> 
      fnc_hc_csg_logo()
) |> 
  set_names(hex_map_opts$filename)


walk(
  names(hex_map_lst), 
  ~save_hc_png(
    hex_map_lst[[.x]], 
    .x, 
    stateplot = FALSE, 
    lst = list(names(hex_map_lst))
  )
)

# STATE AREA CHARTS ############################################################


adj_area_adm_grp <- c(
  "Alabama",
  "Arizona",
  "Arkansas",
  "California",
  "Delaware",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Indiana",
  "Iowa",
  "Kansas",
  "Louisiana",
  "Maine",
  "Minnesota",
  "Mississippi",
  "Montana",
  "Nebraska",
  "South Dakota",
  "Utah",
  "Vermont",
  "Virginia",
  "Wyoming"
)


area_opts_print <- area_opts |> 
  mutate(case_when(
    # ADM group adj
    type == "Admissions" & state_name %in% adj_area_adm_grp ~ tibble(adj_y_sup =  0, adj_y_tech =  4, adj_y_new = -2),
    # ADM manual adj 
    type == "Admissions" & state_name == "Alaska"         ~ tibble(adj_y_sup = -2, adj_y_tech =  0, adj_y_new = 10), 
    type == "Admissions" & state_name == "Colorado"       ~ tibble(adj_y_sup = -2, adj_y_tech =  0, adj_y_new = 12), 
    type == "Admissions" & state_name == "Connecticut"    ~ tibble(adj_y_sup =  0, adj_y_tech = 10, adj_y_new =  5), 
    type == "Admissions" & state_name == "Florida"        ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new = 15), 
    type == "Admissions" & state_name == "Illinois"       ~ tibble(adj_y_sup =  0, adj_y_tech = 10, adj_y_new =  0), 
    type == "Admissions" & state_name == "Kentucky"       ~ tibble(adj_y_sup =  0, adj_y_tech = 10, adj_y_new =  0), 
    type == "Admissions" & state_name == "Maryland"       ~ tibble(adj_y_sup =  0, adj_y_tech = 10, adj_y_new =  0), 
    type == "Admissions" & state_name == "Massachusetts"  ~ tibble(adj_y_sup =  0, adj_y_tech =  5, adj_y_new =  5), 
    type == "Admissions" & state_name == "Michigan"       ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new = 10), 
    type == "Admissions" & state_name == "New Jersey"     ~ tibble(adj_y_sup = -5, adj_y_tech =  5, adj_y_new = 10), 
    type == "Admissions" & state_name == "New York"       ~ tibble(adj_y_sup = -3, adj_y_tech =  0, adj_y_new =  0), 
    type == "Admissions" & state_name == "Oklahoma"       ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new = 15), 
    type == "Admissions" & state_name == "Pennsylvania"   ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new = 15), 
    type == "Admissions" & state_name == "Rhode Island"   ~ tibble(adj_y_sup = -5, adj_y_tech = 10, adj_y_new =  0), 
    type == "Admissions" & state_name == "Tennessee"      ~ tibble(adj_y_sup =  0, adj_y_tech = 10, adj_y_new =  0), 
    type == "Admissions" & state_name == "Washington"     ~ tibble(adj_y_sup =  0, adj_y_tech = 15, adj_y_new =  0), 
    type == "Admissions" & state_name == "West Virginia"  ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new =  5), 
    # POP manual adj 
    type == "Population" & state_name == "California"     ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new = 10), 
    type == "Population" & state_name == "Colorado"       ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new =  5), 
    type == "Population" & state_name == "Florida"        ~ tibble(adj_y_sup =  0, adj_y_tech =  5, adj_y_new =  0), 
    type == "Population" & state_name == "Georgia"        ~ tibble(adj_y_sup =  0, adj_y_tech =  5, adj_y_new =  0), 
    type == "Population" & state_name == "Hawaii"         ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new = 10), 
    type == "Population" & state_name == "Illinois"       ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new = 10), 
    type == "Population" & state_name == "Indiana"        ~ tibble(adj_y_sup =  0, adj_y_tech = 10, adj_y_new =  0), #ISSUES?
    type == "Population" & state_name == "Kansas"         ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new = 15), #ISSSUES?
    type == "Population" & state_name == "Massachusetts"  ~ tibble(adj_y_sup =-20, adj_y_tech =-10, adj_y_new =  0), 
    type == "Population" & state_name == "Mississippi"    ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new = 10), 
    type == "Population" & state_name == "Nebraska"       ~ tibble(adj_y_sup =  0, adj_y_tech =  5, adj_y_new =  0), 
    type == "Population" & state_name == "New York"       ~ tibble(adj_y_sup =  0, adj_y_tech =  8, adj_y_new =  5), 
    type == "Population" & state_name == "North Carolina" ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new = 10), 
    type == "Population" & state_name == "Oregon"         ~ tibble(adj_y_sup =  0, adj_y_tech =  0, adj_y_new = 12), # ISSUES?
    type == "Population" & state_name == "Pennsylvania"   ~ tibble(adj_y_sup =  0, adj_y_tech = 10, adj_y_new =  0), 
    type == "Population" & state_name == "Tennessee"      ~ tibble(adj_y_sup = -5, adj_y_tech =  5, adj_y_new = 10), 
    type == "Population" & state_name == "Texas"          ~ tibble(adj_y_sup =  0, adj_y_tech =  5, adj_y_new =  0), 
    type == "Population" & state_name == "West Virginia"  ~ tibble(adj_y_sup =  0, adj_y_tech = 10, adj_y_new = 10), 
    # no adjustment 
    TRUE  ~ tibble(adj_y_sup = 0, adj_y_tech = 0, adj_y_new = 0)
  )) |> 
  mutate(
    any_adj = ifelse(adj_y_sup != 0 | adj_y_tech != 0 | adj_y_new != 0, TRUE, FALSE )
  ) 


state_area_lst <- pmap(
  # created in 3a_highchart_fnc.R file 
  area_opts_print |> 
    select(type, state_name, adj_y_sup, adj_y_tech, adj_y_new) |> 
    as.list()
  , 
  function(type, state_name, adj_y_sup, adj_y_tech, adj_y_new)
    fnc_hc_area(type, state_name, adj_y_sup, adj_y_tech, adj_y_new) |>
    fnc_add_state_title() |> 
    fnc_hc_csg_logo(margR = 30) |> 
    fnc_add_datalabels()
) |> 
  set_names(area_opts_print$filename)


walk(
  names(state_area_lst),
  ~save_hc_png(
    state_area_lst[[.x]], 
    .x, 
    stateplot = TRUE, 
    lst = list(names(state_area_lst))
  )
)


