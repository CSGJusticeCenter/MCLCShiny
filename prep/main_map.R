
library(tidyverse)
library(highcharter) 
library(sf)

# https://mareichler.github.io/usahex/articles/available_maps.html
library(usahex)

box::use(
  ./box/admin
)


this_path <- csgjcr::csg_sp_path("50 State Revocations Project/50 State Survey (2024)/Data/analysis/main_map")

# load data --------------------------------------------------------------------

data <- read_csv(file.path(
  this_path, "Data for main map 2025_fordesigner(CATEGORIES FOR V3).csv"), 
  skip = 1
  ) |> 
  filter(state_name %in% state.name) |> 
  select(state_name, category = Category, color = ...12) |> 
  fill(color, .direction = "down") |> 
  mutate(
    category = factor(category, levels = c("Staying Low", "Creeping Upward", "Back to Baseline", "Sharp Increase"))
  ) 

color_df <- data |> distinct(category, color) |> 
  arrange(category)


hexcoords <- usahex::get_coordinates(map = "states50", coords = "hexmap") 

## OPTION 1 

plotdf <- left_join(hexcoords, data, by = c("name" = "state_name"))

p1 <- plotdf |> 
  ggplot() + 
  geom_sf(aes(fill = category), color = "#8c8c8c") + 
  scale_fill_manual(
    values = c("#26456E", "#C7E8F5", "#FAEC71", "#D6C246"), 
    na.value = "#DEDEDE", 
    name = NULL, 
    guide = guide_legend(override.aes = list(color = "white"))
  ) + 
  geom_sf_text(
    aes(label = abbr_usps, color = category), 
    family = "Graphik", 
    fontface = "bold", 
    show.legend = FALSE
  ) + 
  scale_color_manual(
    values = c("white", "#333333", "#333333", "#333333"), 
    na.value = "#333333", 
    name = NULL, 
  ) + 
  theme_void() + 
  theme(
    text = element_text(family = "Graphik"), 
    legend.position = "top", 
    # add this so don't have transparent background in png
    plot.background = element_rect(fill = "white", color = "white")
  )

p1

ggsave(filename = file.path(this_path, 
    "Data for main map 2025_fordesigner(CATEGORIES FOR V3)opt1.png"), 
    plot = p1, 
    width = 8, 
    height = 5.5)

file.show(file.path(this_path, "Data for main map 2025_fordesigner(CATEGORIES FOR V3)opt1.png"))


## OPTION 2

p2 <- ggplot(plotdf) + 
  geom_sf(data = filter(plotdf, !is.na(category)), aes(fill = category), color = "#8c8c8c") + 
    geom_sf(data = filter(plotdf, is.na(category)), fill = "#DEDEDE", color = "#8c8c8c") + 
  scale_fill_manual(
    values = c("#26456E", "#C7E8F5", "#FAEC71", "#D6C246"), 
    na.value = "#DEDEDE", 
    name = NULL, 
    guide = guide_legend(override.aes = list(color = "white"))
  ) + 
  geom_sf_text(
    aes(label = abbr_usps, color = category), 
    family = "Graphik", 
    fontface = "bold", 
    show.legend = FALSE
  ) + 
  scale_color_manual(
    values = c("white", "#333333", "#333333", "#333333"), 
    na.value = "#333333", 
    name = NULL, 
  ) + 
  theme_void() + 
  theme(
    text = element_text(family = "Graphik"), 
    legend.position = "top", 
    # add this so don't have transparent background in png
    plot.background = element_rect(fill = "white", color = "white")
  )


p2


ggsave(filename = file.path(this_path, 
       "Data for main map 2025_fordesigner(CATEGORIES FOR V3)opt2.png"), 
       plot = p2, 
       width = 8, 
       height = 5.5)

file.show(file.path(this_path, "Data for main map 2025_fordesigner(CATEGORIES FOR V3)opt2.png"))


## OPTION 3

p3 <- ggplot(plotdf) + 
  geom_sf(data = filter(plotdf, !is.na(category)), aes(fill = category), color = "#8c8c8c") + 
  geom_sf(data = filter(plotdf, is.na(category)),      fill = "#DEDEDE", color = "#8c8c8c") + 
  #dummy legend for NA values, also adds lighter border around certain hexagons (appears to be contrast based)
  geom_sf(                                             fill = NA,        aes(color = "NA")) + 
  scale_fill_manual(
    values = c("#26456E", "#C7E8F5", "#FAEC71", "#D6C246"), 
    name = NULL, 
    guide = guide_legend(override.aes = list(color = "white"))
  ) + 
  geom_sf_text(
    data = filter(plotdf, category != "Staying Low" | is.na(category)), 
    aes(label = abbr_usps), 
    color = "#333333", 
    family = "Graphik", 
    fontface = "bold", 
  ) +
  geom_sf_text(
    data = filter(plotdf, category == "Staying Low"), 
    aes(label = abbr_usps), 
    color = "white", 
    family = "Graphik", 
    fontface = "bold", 
  ) +
  scale_color_manual( #dummy legend for NA color 
    name = "  ", 
    values = "#DEDEDE", # color four outline 
    labels = 'N/A', 
  ) +
  guides(
    fill  = guide_legend(order = 1, override.aes = list(color = "white"))
    , color = guide_legend(override.aes = list(fill = "#DEDEDE"))
  ) +
  theme_void() + 
  theme(
    text = element_text(family = "Graphik"), 
    legend.position = "top", 
    # add this so don't have transparent background in png
    plot.background = element_rect(fill = "white", color = "white")
  )


p3


ggsave(filename = file.path(this_path, 
       "Data for main map 2025_fordesigner(CATEGORIES FOR V3)opt3.png"), 
       plot = p3, 
       width = 8, 
       height = 5.5)

file.show(file.path(this_path, "Data for main map 2025_fordesigner(CATEGORIES FOR V3)opt3.png"))


