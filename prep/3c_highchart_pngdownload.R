# Description: Create and save highcharts WITH LOGO (pngs) so the app loads faster
# path to data on research div sharepoint
# make sure sharepoint folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# in your Renviron (usethis::edit_r_environ()), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"

box::use(
   ./box/admin, 
  , glue[glue]
  , htmlwidgets[saveWidget]
  , webshot2[webshot]
)

# assumes 3a_highchart_fnc has been run 

# functions for saving plots ---------------------------------------------------

save_hc_png <- function(this_hc, this_filename){
  
  message(glue("SAVE HC to PNG  -- {this_filename}"))
  
  filename_ext <- paste0(this_filename, ".png")
  save_plots_to <- file.path(      "app/data/plots", filename_ext) 
  # copy_plots_to <- file.path(admin$sp_data, "plots", filename_ext)
  
  saveWidget(this_hc, file = "temp.html", selfcontained = TRUE)
  webshot2::webshot(
    url = "temp.html", 
    file = file.path(save_plots_to), 
    delay = 1
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
      fnc_hc_csg_logo()
) |> 
  set_names(hex_map_opts$filename)


walk(
  names(hex_map_lst), 
  ~save_hc_png(hex_map_lst[[.x]], .x)
)



