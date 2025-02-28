
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

# ~25-30 min to save hex map pngs 
# avg ~23 sec per chart 
walk(
  names(hex_map_lst), 
  ~save_hc_png(
    hex_map_lst[[.x]], 
    .x, 
    stateplot = FALSE, 
    lst = list(names(hex_map_lst))
  )
)

