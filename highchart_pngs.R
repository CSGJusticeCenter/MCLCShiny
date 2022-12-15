
# need to run highchart.R before hand to create charts and pathways 

theseFOLDERS <- c("sharepoint" = file.path(admin$sp_data, "plots"), "app" = "app/data/plots")

##########
# State Overview Area Chart
##########

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(all_state_area_adm[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Prison_Admissions.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(all_state_area_pop[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Prison_Population.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

##########
# State Supervision Violation Bar Chart
##########

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(all_state_bar_adm[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Supervision_Violation_Admissions_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(all_state_bar_pop[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Supervision_Violation_Population_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

##########
# Probation Bar Chart
##########

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(probation_bar_adm[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Probation_Violation_Admissions_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(probation_bar_pop[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Probation_Violation_Population_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

##########
# Parole Bar Chart
##########

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(parole_bar_adm[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Parole_Violation_Admissions_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

for (folder in theseFOLDERS){
  for (i in states_list){
    htmlwidgets::saveWidget(parole_bar_pop[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/", i, "_Parole_Violation_Population_by_Type.png", sep = ""), zoom = 4, vwidth = 500, vheight = 500, delay = 1)
  }
}

##########
# MAPS Admissions - loops are separate for now because of timeout issues
##########

for (folder in theseFOLDERS){
  # 2018-2019
  for (i in metrics_list){
    htmlwidgets::saveWidget(adm_maps_2018_2019[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Admissions_2018 - 2019.png", sep = ""), delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2018-2021
  for (i in metrics_list){
    htmlwidgets::saveWidget(adm_maps_2018_2021[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Admissions_2018 - 2021.png", sep = ""), delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2019-2020
  for (i in metrics_list){
    htmlwidgets::saveWidget(adm_maps_2019_2020[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Admissions_2019 - 2020.png", sep = ""), delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2020-2021
  for (i in metrics_list){
    htmlwidgets::saveWidget(adm_maps_2020_2021[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Admissions_2020 - 2021.png", sep = ""), delay = 1)
  }
}

##########
# MAPS POPULATION
##########

for (folder in theseFOLDERS){
  # 2018-2019
  for (i in metrics_list){
    htmlwidgets::saveWidget(pop_maps_2018_2019[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Population_2018 - 2019.png", sep = ""),delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2018-2021
  for (i in metrics_list){
    htmlwidgets::saveWidget(pop_maps_2018_2021[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Population_2018 - 2021.png", sep = ""), delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2019-2020
  for (i in metrics_list){
    htmlwidgets::saveWidget(pop_maps_2019_2020[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Population_2019 - 2020.png", sep = ""), delay = 1)
  }
}

for (folder in theseFOLDERS){
  # 2020-2021
  for (i in metrics_list){
    htmlwidgets::saveWidget(pop_maps_2020_2021[[i]], "temp.html")
    webshot2::webshot(url = "temp.html", file = paste(folder, "/Change_", i, "_Population_2020 - 2021.png", sep = ""), delay = 1)
  }
  
}
