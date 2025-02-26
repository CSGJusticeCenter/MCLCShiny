
box::use(
  ./box/admin, 
  dplyr[...], 
  geojsonsf[sf_geojson], 
  jsonlite[fromJSON], 
  sf[read_sf, st_transform] 
)


hex_gj <- read_sf(file.path(admin$sp_data_raw, "us_states_hexgrid.geojson")) |> 
  select(state_abb = iso3166_2) |> 
  filter(state_abb != "DC") |> 
  mutate(state_name = state.name[match(state_abb, state.abb)]) |> 
  # Reformat hex data
  st_transform(3857) |>
  sf_geojson() |> 
  fromJSON(simplifyVector = FALSE)


admin$save_rds_twice(hex_gj)
