

# STATE BAR CHARTS #############################################################


state_bar_lst <- pmap(
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


# ~30 min to save area pngs 
# avg ~18 sec per chart
walk(
  names(state_bar_lst),
  ~save_hc_png(
    state_bar_lst[[.x]], 
    .x, 
    stateplot = TRUE, 
    lst = list(names(state_bar_lst))
  )
)
