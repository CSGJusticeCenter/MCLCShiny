
name_lst <- c(
  hex_map_opts$filename, 
  area_opts$filename, 
  bar_opts$filename
) |> sort()
#name_lst <- map_vec(list.files(path = "temp"), ~str_remove(.x, ".html"))

# save  ALLLLLL pngs 
# avg ~1-2 sec per chart 
# walk(
#   name_lst, 
#   ~save_hchtml_to_png(
#     .x, 
#     lst = list(name_lst)
#   )
# )

# html last edit 
html_last_mod <- map_dfr(
  name_lst, 
  ~file.info(paste0("temp/", .x, ".html")) |> 
    mutate(name = .x, .before = 1)
) |> 
  rownames_to_column(var = "html_path") |> 
  as_tibble() |> 
  select(name, html_path, html_mod = mtime)


# png last edit 
png_last_mod <- map_dfr(
  name_lst, 
  ~file.info(paste0("app/data/plots/", .x, ".png")) |> 
    mutate(name = .x, .before = 1)
) |> 
  rownames_to_column(var = "png_path") |> 
  as_tibble() |> 
  select(name, png_path, png_mod = mtime)



last_mod_df <- full_join(
  html_last_mod, 
  png_last_mod, 
  by = "name"
) |> arrange(name)


pngs_to_create <- last_mod_df |> 
  filter(html_mod > png_mod | is.na(png_mod))


message(glue("{nrow(pngs_to_create)} png to create/update; \\
             {nrow(pngs_to_create)} out of {length(name_lst)} total charts; \\
             {length(name_lst) - nrow(pngs_to_create)} pngs already up-to-date"))

# save pngs 
# avg ~1-2 sec per chart 
walk(
  pngs_to_create$name, 
  ~save_hchtml_to_png(
    .x, 
    lst = list(pngs_to_create$name)
  )
)


