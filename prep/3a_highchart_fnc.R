

library(tidyverse)
library(highcharter) 
library(sf)
library(jsonlite)
library(geojsonsf)
library(scales)
library(rjson)

# can't use box for these func; need to use library for func otherwise highchart won't render 
# don't have time to explore reasoning at the moment

# load data frames -------------------------------------------------------------

for (x in c("svii_agg", "svii_explorer", "svii_yr")){
  df <- readRDS(paste0("app/data/", x, ".rds")) |> tibble::as_tibble()
  assign(x, df)
  rm(df)
  rm(x)
}

# assign colors for visualizations ---------------------------------------------
source("app/colors.R")

# lists metrics for functions --------------------------------------------------

metrics <- c("New Offense Violation",
             "Parole Violation",
             "Probation Violation",
             "Supervision Violation",
             "Technical Violation",
             "Total")

# themes -----------------------------------------------------------------------

# Highcharts theme for hex map
hc_theme_map_jc <-
  hc_theme_merge(
    hc_theme_smpl(),
    hc_theme(
      chart = list(
        marginTop = 75,
        style = list(fontFamily = "Graphik",
                     align = "center")
      ),
      title = list(align = "center",
                   style = list(fontFamily = "Graphik",
                                fontWeight = "bold",
                                color = "black",
                                fontSize   = "30px")),
      subtitle = list(align = "center",
                      style = list(fontFamily = "Graphik",
                                   fontWeight = "bold",
                                   color = "black",
                                   fontSize   = "30px")),
      caption = list(align = "center", y = 15),
      xAxis = list(
        labels = list(
          style = list(fontSize = "15px"),
          staggerLines = 2
        ),
        gridLineColor = "transparent"
      ),
      plotOptions = list(
        series = list(states = list(inactive = list(opacity = 1))),
        line = list(marker = list(enabled = TRUE)),
        spline = list(marker = list(enabled = TRUE)),
        area = list(marker = list(enabled = TRUE)),
        areaspline = list(marker = list(enabled = TRUE))
      )
    )
  )

hc_theme_jc <- hc_theme(
  colors = c("#D25E2D", "#EDB799", "#C7E8F5", "#236ca7", "#D6C246", "#dcdcdc"),
  chart = list(
    style = list(
      fontFamily = "Graphik",
      color      = "#666666"
      )
  ),
  title = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = "black",
      fontSize   = "16px"
      )
  ),
  subtitle = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = "black",
      fontSize   = "14px"
      )
  ),
  legend = list(
    align         = "center",
    verticalAlign = "top"
  ),
  xAxis = list(
    gridLineColor      = "transparent",
    lineColor          = "transparent",
    minorGridLineColor = "transparent",
    tickColor          = "transparent"
  ),
  yAxis = list(
    labels = list(enabled = TRUE),
    gridLineColor      = "transparent",
    lineColor          = "transparent",
    majorGridLineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor          = "transparent"
  ),
  plotOptions = list(
    line       = list(marker = list(enabled = FALSE)),
    spline     = list(marker = list(enabled = FALSE)),
    area       = list(marker = list(enabled = FALSE)),
    areaspline = list(marker = list(enabled = FALSE)),
    arearange  = list(marker = list(enabled = FALSE)),
    bubble     = list(maxSize = "10%")
  )
)

# set thousands sepeartor to a comma -------------------------------------------

# this sets the thousands separator to a comma 
# so 1000 will be displayed as "1,000"
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


# adj to PNGs (downloaded images) ----------------------------------------------

render_image <- JS("
  function(){
    this.renderer.image('https://csg-state-violent-crime.netlify.app/img/csgjc-logo.png', 30, this.chartHeight - 37, 140.1, 30)
    .add();
  }")

fnc_hc_csg_logo <- function(hc, margR = NA){
  hc |> 
    hc_chart(
      events = list(render = render_image), 
      marginBottom = 80, 
      marginRight = margR
    )
}

fnc_add_datalabels <- function(hc){
  hc |> 
    hc_plotOptions(
      series = list(
        dataLabels = list(
          enabled = TRUE,
          allowOverlap = TRUE
        )
      )
    )
}

fnc_add_state_title <- function(hc, this_state = NA){
  
  org_title <- hc$x$hc_opts$title$text
  thisstate <- ifelse(
    !is.na(this_state), 
    this_state, 
    hc$x$hc_opts$series[[1]]$data[[1]]$state
  )
  
  new_title <- paste0(thisstate, " ", org_title)
  
  hc |> 
    hc_title(text = new_title)
  
}


fnc_adj_map_legend <- function(hc){
  # to match image outputs of last time 
  hc |> 
    hc_legend(
      align = "right",
      layout = "vertical",
      verticalAlign = "top",
      y = 300, 
      x = NA, 
      symbolHeight = NA,
      symbolWidth = NA
    )
  
}

# hex map -----------------------------------------------------------------------

# saved version of hex doesn't work; need to re-import 
hex_url <- "https://github.com/CSGJusticeCenter/va_data/raw/main/model_code/violation_admissions/us_hex_map.json"
hex <- fromJSON(file = hex_url)

hex_map_opts <- crossing(
  type = c("Admissions", "Population"), 
  year_chg = factor(svii_yr$change_name, levels = svii_yr$change_name), 
  metric = factor(metrics, levels = metrics),
) |> 
  mutate(
    filename = paste("Change", metric, type, year_chg, sep = "_")
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
  
  message(glue("HEX MAP {str_pad(which_opt, 2)}/{nrow(hex_map_opts)} -- \\
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
    # theme and legend 
    hc_add_theme(hc_theme_map_jc) |> 
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
    hc_subtitle(text = this_year_chg) |> 
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


# add series (for area and bar charts) -----------------------------------------

.fnc_hc_add_series <- function(hc, hctype, df0, metric0, color0, y0 = 0){
  
  hc |> 
    hc_add_series(
      data = subset(df0, metric == metric0),
      name = metric0,
      type = hctype,
      hcaes(x = year, y = n),
      color = color0,
      accessibility = list(
        enabled = TRUE,
        keyboardNavigation = list(enabled = TRUE),
        point = list(
            valueDescriptionFormat =
            "{point.state}, {point.year}, {point.metric}, {point.type}, {point.total:,.0f}"
        )
      ), 
      dataLabels = list(y = y0)
    )
  
}


# area chart -------------------------------------------------------------------

area_opts <- crossing(
  type = c("Admissions", "Population"), 
  state_name = state.name
) |> 
  mutate(
    filename = paste(state_name, "Prison", type, sep = "_")
  ) |> 
  arrange(type, state_name) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(n = 1:n(), .before = 1) 


fnc_hc_area <- function(this_type, this_state, 
                  # adjustments for data labels for pngs 
                  adj_y_sup = 0, adj_y_tech = 0, adj_y_new = 0){
  
  which_opt <- area_opts |> 
    filter(
      type == this_type, 
      state_name == this_state, 
    ) |> pull(n)
  
  message(glue("AREA CHART {str_pad(which_opt, 2)}/{nrow(area_opts)} -- \\
               {this_type}, {this_state}"))
  
  this_df <- svii_agg |> 
    filter(
      state_name == this_state, 
      type == this_type, 
      word(metric_abbr, 2, -1) %in% c("total", "supervision", "new", "tech")
    ) |> 
    mutate(
      n = ifelse(n == 0, NA, n), 
      tooltip = paste0("<b>", state_name, " - ", year, "</b><br>",
                       metric, " ",
                       type, "<br>",
                       formattable::comma(n, digits = 0), "<br>")
    ) |> 
    mutate(state = state_name) # hc has issues with _ in var names 
  
  
  subtitle_name <- this_df |> 
    filter(metric == "Supervision Violation") |> 
    distinct(probation_or_parole) |> 
    pull(probation_or_parole)
  
  access_text <- paste0(
     "This is an area chart for the state of ", 
     this_state, 
     " displaying the total prison ", 
     tolower(this_type), 
     " due to supervision violations, ", 
     "subset by technical violations and new offense violations.")

    
  highchart() |> 
    hc_chart(type = "area") |> 
    .fnc_hc_add_series("area", this_df, "Total", total_co) |> 
    .fnc_hc_add_series("area", this_df, "Supervision Violation", viol_co,  adj_y_sup) |> 
    .fnc_hc_add_series("area", this_df, "Technical Violation"  , tech_co,  adj_y_tech) |> 
    .fnc_hc_add_series("area", this_df, "New Offense Violation", new_o_co, adj_y_new) |> 
    hc_xAxis(tickPositions = c(svii_yr$min_yr[1]:svii_yr$max_yr[1])) |> 
    hc_yAxis(labels=list(format="{value:,.0f}")) |> 
    hc_title(text = paste("Prison", this_type)) |> 
    hc_subtitle(text = subtitle_name) |> 
    hc_add_theme(hc_theme_jc) |> 
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |> 
    hc_plotOptions(
      series = list(
        animation = FALSE,
        cursor = "pointer",
        borderWidth = 3
      ),
      accessibility = list(
        enabled = TRUE,
        keyboardNavigation = list(enabled = TRUE),
        linkedDescription = access_text,
        landmarkVerbosity = "one"
      ),
      area = list(accessibility = list(description = access_text))
    ) 
}

# bar chart --------------------------------------------------------------------

bar_opts <- crossing(
  type = c("Admissions", "Population"), 
  supervision_type = c("Both", "Parole", "Probation"), 
  state_name = state.name
) |> 
  mutate(
    filename = paste(
      state_name, 
      ifelse(supervision_type == "Both", "Supervision", supervision_type), 
      "Violation", 
      type, 
      "by_Type", 
      sep = "_"
    )
  ) |> 
  arrange(type, supervision_type, state_name) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(n = 1:n(), .before = 1)

fnc_hc_bar <- function(this_type, this_supervision, this_state){
  
  which_opt <- bar_opts |> 
    filter(
      type == this_type, 
      supervision_type == this_supervision, 
      state_name == this_state, 
    ) |> pull(n)
  
  message(glue("BAR CHART {str_pad(which_opt, 2)}/{nrow(bar_opts)} -- \\
               {this_type}, {this_supervision}, {this_state}"))
  
  this_df <- svii_agg |> 
    filter(
      state_name == this_state, 
      supervision_type == this_supervision, 
      type == this_type, 
      word(metric_abbr, 2) %in% c("new", "tech")
    ) |> 
    mutate(
      n = ifelse(n == 0, NA, n), 
      tooltip = paste0("<b>", state_name, " - ", year, "</b><br>",
                       metric, " ",
                       type, "<br>",
                       formattable::comma(n, digits = 0), "<br>")
    ) |> 
    mutate(state = state_name) # hc has issues with _ in var names 
  
  
  title_name <- case_when(
    this_supervision == "Both" ~ paste0("Supervision Violation ", this_type, " by Type"), 
    this_supervision != "Both" ~ paste0(this_supervision, " Violation ", this_type, " by Type")
  )
  
  subtitle_name <- case_when(
    this_supervision == "Both" ~ filter(this_df, metric == "Technical Violation")$probation_or_parole |> unique(), 
    this_supervision != "Both" ~ NA_character_
  )
  
  access_text <- paste0(
    "This is a bar chart for the state of ", 
    this_state, 
    " displaying the total prison ", 
    tolower(this_type), 
    " due to ", 
    ifelse(this_supervision == "Both", "supervision", tolower(this_supervision)), 
    " violations, ", 
    "subset by technical violations and new offense violations.")
  
  
  highchart() |> 
    hc_chart(type = "column") |> 
    .fnc_hc_add_series("column", this_df, "Technical Violation", tech_co) |> 
    .fnc_hc_add_series("column", this_df, "New Offense Violation", new_o_co) |> 
    hc_xAxis(tickPositions = c(svii_yr$min_yr[1]:svii_yr$max_yr[1])) |> 
    hc_yAxis(labels=list(format="{value:,.0f}")) |> 
    hc_title(text = title_name) |> 
    hc_subtitle(text = subtitle_name) |> 
    hc_add_theme(hc_theme_jc) |> 
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |> 
    hc_plotOptions(
      series = list(
        animation = FALSE,
        cursor = "pointer",
        borderWidth = 3, 
        midPointLength = 4
      ),
      accessibility = list(
        enabled = TRUE,
        keyboardNavigation = list(enabled = TRUE),
        linkedDescription = access_text,
        landmarkVerbosity = "one"
      ),
      column = list(accessibility = list(description = access_text))
    )
}


