#######################################
# Project: MCLCShiny
# File: functions.R
# Authors: Mari Roberts
# Date last updated: May 15, 2023 (MAR)
# Description:
#    Defines custom functions
#######################################

# define a custom formatter to replace blanks with a dash
fnc_add_dash <- function(value) {
  if (is.na(value) || value == "") {
    return("-")
  } else {
    return(value)
  }
}

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


# add a nicely styled label above selection box
labeled_input <- function(id, label, input){
  div(id = id,
      span(label, style = "font-size: small;"),
      input)
}
