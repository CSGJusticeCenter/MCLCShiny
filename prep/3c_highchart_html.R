

# NATL HEX MAPS HTML DOWNLOAD ##################################################

natl_hex_html_lst <- pmap(
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

# ~25 min 
# ~avg 20 seconds per chart, 72 charts 
paste("Hopefully done by:", format(Sys.time()+25*60, "%X"))
walk(
  names(natl_hex_html_lst), 
  ~save_hc_to_html(
    natl_hex_html_lst[[.x]], 
    .x, 
    lst = list(names(natl_hex_html_lst))
  )
)



# STATE AREA HTML DOWNLOAD #####################################################

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


state_area_html_lst <- pmap(
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


# ~40 min 
# avg ~24 sec per chart, 100 charts
paste("Hopefully done by:", format(Sys.time()+45*60, "%X"))
walk(
  names(state_area_html_lst),
  ~save_hc_to_html(
    state_area_html_lst[[.x]], 
    .x, 
    lst = list(names(state_area_html_lst))
  )
)



# STATE BAR HTML DOWNLOAD ######################################################

state_bar_html_lst <- pmap(
  # created in 3a_highchart_fnc.R file 
  bar_opts |> 
    select(type, supervision_type, state_name) |> 
    as.list()
  , 
  function(type, supervision_type, state_name)
    fnc_hc_bar(type, supervision_type, state_name) |>
    fnc_add_state_title() |> 
    fnc_hc_csg_logo(margR = 30) |> 
    # last version used default (11px); but have 2 more years so bars are smaller
    fnc_add_datalabels(label_fontSize = "9px")
) |> 
  set_names(bar_opts$filename)


# ~30 min
# avg ~6 sec per chart (early charts take 30-60 sec), 300 charts
paste("Hopefully done by:", format(Sys.time()+90*60, "%X"))
walk(
  names(state_bar_html_lst),
  ~save_hc_to_html(
    state_bar_html_lst[[.x]], 
    .x, 
    lst = list(names(state_bar_html_lst))
  )
)
