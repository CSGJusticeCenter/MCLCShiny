## INFO #################################
# Project: MCLCShiny
# File: functions.R
# Authors: Mari Roberts, Martha Eichlersmith
# Date last updated: 2025-03-05 (MYE)
# Description:Defines custom functions for use in shiny app 


# https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/
# Value box
fnc_value_box <- function(
    title,
    adm_or_pop,
    subtitle,
    value,
    finding,
    icon = NULL,
    color = "aqua",
    width = 4,
    href = NULL
) {
  shinydashboard:::validateColor(color)

  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")

  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      HTML(paste0("<h4><b>", title, "</b></h4>")),
      HTML(paste0("<h4><b>", adm_or_pop, "</b></h4>")),
      HTML(paste0("<h5><b>", subtitle, "</b></h5>")),
      HTML(paste0("<h1><b>", value, "</b></h1>")),
      HTML(paste0("<h5><b>", finding, "</b></h5>"))
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )

  if (!is.null(href))
    boxContent <- a(href = href, boxContent)

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}

# global spinner style for charts 
svii_spinner <- function(uiobj){
  withSpinner(
    uiobj, 
    color = lightblue, 
  )
}


# add a nicely styled label above selection box
labeled_input <- function(id, label, input){
  div(id = id,
      span(label, style = "font-size: small;"),
      input)
}



state_reactable <- function(df, these_col_fill = colpal_fill, these_col_stroke = colpal_stroke) {
  
  # function results in warning for each row 
  #> `bindFillRole()` only works on htmltools::tag() objects (e.g., div(), p(), etc.), not objects of type 'shiny.tag.list'
  # comes from dataui::dui_sparkline function 
  
  col_names <- names(df)
  col_chg   <- col_names[str_detect(col_names, "-")]
  col_trnd  <- col_names[str_detect(col_names, "trend_data")]
  
  
  display <- rename(df, change = all_of(col_chg), trend  = all_of(col_trnd)) 
  
  # need width of reactable to be <= 995
  # text (275) + yr cnt (6*95) + change (110) + trend (110) = 1065
  
  reactable(
    display, 
    style = list(fontFamily = "Graphik, sans-serif", fontSize = "1.4rem"), 
    theme = reactableTheme(
      cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"), 
      headerStyle = list(textAlign = "right")
    ), 
    compact = TRUE,
    searchable = FALSE,
    pagination = FALSE,
    defaultColDef = colDef(
      format = colFormat(separators = TRUE), 
      minWidth = 90, 
      align = "right", 
      na = "-" # using n dash; could also use longer m dash: "–"
      #headerVAlign = "bottom"
    ),
    columns = list(
      text = colDef(
        name = "Metric",
        align = "left",
        minWidth = 235,
        style = list(fontWeight = "bold")
      ), 
      change = colDef(
        na = "–", 
        minWidth = 110,
        name = paste0(str_remove_all(col_chg, " ")," Change"),
        style = list(fontWeight = "bold"),
        format = colFormat(percent = TRUE, digits = 0)
      ), 
      trend = colDef(
        minWidth = 110,
        name = "Trend Line",
        cell = function(value, index) {
          points_list <- which(!is.na(value[[1]]))-1
          dui_sparkline(
            data = value[[1]],
            height = 80,
            margin = list(top = 30, right = 20, bottom = 30, left = 20),
            components = list(
              dui_sparkpointseries(
                points = points_list,
                stroke = these_col_fill[index],
                fill = these_col_stroke[index],
                size = 2.5
              ),
              dui_sparklineseries(
                curve = "linear",
                showArea = FALSE,
                fill = these_col_fill[index],
                stroke = these_col_stroke[index]
              )
            )
          )
        }
      )
    )
  )
}





demo_table_header <- function(text){
  stringr::str_replace_all(text, c(
    "Unknown s/g" = "Unknown", 
    "Unknown r/e" = "Unknown", 
    "AIAN" = "American Indian", 
    "NHPI" = "Pacific Islander", 
    "Two" = "Multiracial", 
    "Asian" = "Asian American"
  ))
  
}


# df <- svii_demo_table_prep |> 
#   filter(state_name == "Alabama", type == "Population", group_cat == "race_ethnicity", table == 3)


demo_reactable <- function(df, metric_header = "Metric") {
  # need width of reactable to be <= 995
  # data (275) + demo cnts (9*85)  = 1000
  
  display_df <- df |> 
    arrange(group, metric_abbr) |> 
    mutate(
      data = case_when(
        word(metric_abbr, 2, 3) == "total comp" ~ paste0(data, "<sup>2</sup>"), 
        word(metric_abbr, 2, 3) == "par comp"   ~ paste0(data, "<sup>3</sup>"),
        word(metric_abbr, 2, 3) == "prob comp"  ~ paste0(data, "<sup>4</sup>"), 
        TRUE ~ data
      ), 
      prop = case_when(
        highlight == TRUE ~ paste0("<span class = 'highlight'>", prop, "</span>"), 
        TRUE ~ prop
      )
    ) |> 
    select(data, group, prop) |> 
    pivot_wider(names_from = group, values_from = prop)
  
  greyout_row <- ifelse(str_detect(sort(df$metric_abbr)[1], "comp") == TRUE, 1, 0)
  
  reactable(
    display_df, 
    style = list(fontFamily = "Graphik, sans-serif", fontSize = "1.4rem"), 
    theme = reactableTheme(
      cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"), 
      headerStyle = list(fontWeight = "bold")
    ), 
    compact = TRUE,
    searchable = FALSE,
    pagination = FALSE,
    sortable = FALSE, 
    rowStyle = function(index){
      if (index == 1){
        list(
          background = "rgba(0, 0, 0, 0.05)",
          `border-bottom` = "thin solid",
          `border-color` = "rgba(0, 0, 0, 0.10)")
      }
    },
    defaultColDef = colDef(
      format = colFormat(separators = TRUE), 
      header = function(value) demo_table_header(value), 
      minWidth = 85, 
      align = "right", 
      na = "-", # using n dash; could also use longer m dash: "–"
      headerVAlign = "bottom", 
      html = TRUE
    ),
    columns = list(
      data = colDef(
        name = metric_header,
        align = "left",
        minWidth = 235,
        width = 235, 
        style = list(fontWeight = "500"), 
        html = TRUE
      ) 
      # Hispanic = colDef(minWidth = 110), 
      # AIAN = colDef(minWidth = 100), 
      # NHPI = colDef(minWidth = 90), 
      # Two = colDef(minWidth = 90), 
      # `Unknown r/e` = colDef(minWidth = 100)
    )
  )
}

