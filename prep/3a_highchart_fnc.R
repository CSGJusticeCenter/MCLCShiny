

library(tidyverse)
library(highcharter) 
library(sf)
library(jsonlite)
library(geojsonsf)
library(scales)
library(rjson)

# can't use box for these func; need to use library for func otherwise highchart won't render 
# don't have time to explore reasoning at the moment 

# saved version of hex doesn't work; need to re-import 
hex_url <- "https://github.com/CSGJusticeCenter/va_data/raw/main/model_code/violation_admissions/us_hex_map.json"
hex <- fromJSON(file = hex_url)

# map_theme <- hc_theme(
#   chart = list(style = list(fontFamily = "Arial", color = "#666666")),
#   title = list(
#     style = list(
#       fontFamily = "Arial",
#       fontWeight = "bold",
#       color = "black",
#       fontSize   = "30px"
#     )
#   )
# )

hex_map_opts <- crossing(
  type = c("Admissions", "Population"), 
  year_chg = factor(svii_yr$change_name, levels = svii_yr$change_name), 
  metric = factor(metrics, levels = metrics),
) |> 
  arrange(type, year_chg, metric) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(n = 1:n(), .before = 1)


# highchart map for map explorer page
fnc_hc_hex_map <- function(this_type, this_year_chg, this_metric){
  
  which_opt <- hex_map_opts |> 
    filter(
      type == this_type, 
      year_chg == this_year_chg, 
      metric == this_metric
   ) |> pull(n)
  
  message(glue("{str_pad(which_opt, 2)}/{nrow(hex_map_opts)} -- \\
               {this_type}, {this_year_chg}, {this_metric}"))
  
  this_df <- svii_explorer |> 
    filter(
      type == this_type, 
      year_chg == this_year_chg, 
      metric == this_metric
    ) |> 
    # to match variable in hex_gj
    mutate(
      state_abb = as.character(state_abbr), 
      state = as.character(state_name), 
      full_metric = as.character(data), 
      value = chg_rnd # hc seems to have issue with _ in var name
    ) 
  
  this_title <- case_when(
    this_metric != "Total" & this_type == "Admissions" ~ paste0("Change in Admissions to State Prison for ", this_metric, "s"), 
    this_metric != "Total" & this_type == "Population" ~ paste0("Change in State Prison Population  for "  , this_metric, "s"),
    this_metric == "Total" & this_type == "Admissions" ~ "Change in Total Admissions to State Prison", 
    this_metric == "Total" & this_type == "Population" ~ "Change in Total Population in State Prison"
  )
  
  # determine color 
  # if only negative/only positive only use negative colors/positive colors 
  # if have both negative/postive values use full range 
  if (this_df$min_map_type[1] != this_df$max_map_type[1]){
    n_col_stops <- 7
    col_vec <- c(
      darkblue, regblue,lightblue, 
      white, 
      lightorange, orange, darkorange
    )
  } else if (this_df$min_map_type[1] == "negative" & this_df$max_map_type[1] == "negative"){
    n_col_stops <- 4
    col_vec <- c(
      darkblue, regblue,lightblue, 
      white
    )
  } else if (this_df$min_map_type[1] == "positive" & this_df$max_map_type[1] == "positive"){
    n_col_stops <- 4
    col_vec <- c(
      white, 
      lightorange, orange, darkorange
    )
  } else {
    stop("girl something wrong with the determining color gradient")
  }
  
  highchart() |> 
  # add map series 
    hc_add_series_map(
      map = hex,
      df = this_df,
      joinBy = "state_abb",
      value = "value",
      dataLabels = list(
        enabled = TRUE,
        useHTML = TRUE,
        formatter = JS("function() {return '<div style=\"text-align:center;\">' +
          '<span style=\"font-weight:bold;\">' + this.point.state_abb + '</span><br>' +
          '<span>' + this.point.chg_label + '</span>' +
          '</div>';}"),
        style = list(
          fontSize = "14px",
          fontWeight = "regular"
        )
      ),
      nullColor = "#e8e8e8",
      accessibility = list(
        point = list(
          valueDescriptionFormat = 
            #"state: {point.state}, percent change: {point.value:.1f}"
            "{point.state}, {point.metric}, {point.type}, Change from {point.year_chg}, {point.value}%"
        )
      )
    ) |> 
    # coloring gradient 
    hc_colorAxis(
      min = this_df$NEW_MIN[1],
      max = this_df$NEW_MAX[1],
      stops = color_stops(n_col_stops, col_vec),
      labels = list(
        format = "{value}%", 
        style = list(fontSize = "14px")
        )
    ) |> 
    # legend 
    hc_legend(
      align = "right",
      verticalAlign = "bottom",
      layout = "vertical",
      symbolHeight = 200,
      symbolWidth = 25,
      x = -25,
      y = 0
    ) |> 
    # labels and title/subtitle 
    hc_xAxis(title = "") |> 
    hc_yAxis(title = "") |> 
    hc_title(text = this_title) |> 
    hc_subtitle(text = this_df$year_chg[1]) |> 
    # tooltips 
    hc_tooltip(
      formatter = JS("function() {
        return '<div style=\"background-color: #FFFFFF; opacity: 1; padding: 8px; border: none;\">' +
        this.point.tooltip +
        '</div>';}"),
      useHTML = TRUE
    ) |> 
    # plot options/accessibility 
    hc_plotOptions(
      series = list(
        animation = FALSE,
        cursor = "pointer",
        borderWidth = 3
      ),
      accessibility = list(
        enabled = TRUE,
        keyboardNavigation = list(enabled = TRUE),
        linkedDescription = paste0(
          "This is a hex map showing the percent change for each state for ", 
          this_metric, " ", 
          this_type, " from ", 
          this_df$year_chg[1], "."
        ), 
        landmarkVerbosity = "one"
      )
    )
  
}
