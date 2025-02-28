

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

# 15:28 - 
# ~30 min to save area pngs 
# avg ~18 sec per chart
walk(
  names(state_area_lst),
  ~save_hc_png(
    state_area_lst[[.x]], 
    .x, 
    stateplot = TRUE, 
    lst = list(names(state_area_lst))
  )
)
