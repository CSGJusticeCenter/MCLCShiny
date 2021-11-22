
# test file for map

# load necessary packages
requiredPackages = c('dplyr',
                     'janitor',
                     'readxl',
                     'DT',
                     'tidyverse',
                     'gdata',
                     'ggthemes',
                     'shiny',
                     'shinydashboard',
                     'shinythemes',
                     'ggplot2',
                     'leaflet',
                     'maps',
                     'maptools',
                     'mapproj',
                     'rgeos',
                     'geojsonio',
                     'rgdal',
                     'tigris',
                     'tidycensus',
                     'spData',
                     'sf',
                     'tmap',
                     'grid')

# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

setwd("C:/Users/mroberts/OneDrive - The Council of State Governments/Desktop/csgjc/repos/MCLCShiny")

mclc.df <- mclc_change %>% 
  filter(adm_or_pop == "Admissions" &
         year == "2020" &
         metric == "Total")

# From https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
us <- readOGR(dsn = "data/cb_2014_us_state_5m/cb_2014_us_state_5m.shp",
              layer = "cb_2014_us_state_5m", verbose = FALSE)

# convert it to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# extract, then rotate, shrink & move alaska (and reset projection)
# need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
alaska <- us_aea[us_aea$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)

# remove old states and put new ones back in; note the different order
# we're also removing puerto rico in this example but you can move it
# between texas and florida via similar methods to the ones we just used
us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "72"),]
us_aea <- rbind(us_aea, alaska, hawaii)
# transform data again
us_aea2 <- spTransform(us_aea, proj4string(us))

# merge data
mclc.df <- merge(us_aea2, mclc.df, by.x = "NAME", by.y = "states", all=F)

######################
# Positive to negative values
######################

## Make vector of colors for values smaller than 0
rc1 <- colorRampPalette(colors = c("#2A5B71", "#DAEAF2"), space = "Lab")(5)

## Make vector of colors for values larger than 0 (20 colors)
rc2 <- colorRampPalette(colors = c("#D2E9AD", "#72A029"), space = "Lab")(2)

## Combine the two color palettes
rampcols <- c(rc1, rc2)

mypal <- colorNumeric(palette = rampcols, domain = mclc.df$change)

## If you want to preview the color range, run the following code
previewColors(colorNumeric(palette = rampcols, domain = NULL), values = -100:150)

centers <- data.frame(gCentroid(mclc.df, byid = TRUE))
centers$change <- mclc.df$change

mclc.df$popup_text <- 
  paste0('<strong>', mclc.df$NAME, '</strong>',
         '<br/>', '<strong>','Change: ', '</strong>', mclc.df$change,"%", sep = "", ' ') %>% 
  lapply(htmltools::HTML)

leaflet() %>% 
  addTiles() %>%
  addPolygons(data = mclc.df,
            weight=1,opacity = 1.0,color = 'white',
            fillOpacity = 0.9, smoothFactor = 0.5,
            fillColor = ~mypal(mclc.df$change),
            popup = mclc.df$popup_text,
            # highlight options
            highlightOptions = highlightOptions(
              weight = 2,
              color = '#4698BC'
            )) %>%
  addLegend(position = "bottomright", 
            pal = mypal, 
            values = mclc.df$change,
            title = "% Change",
            labFormat = labelFormat(suffix="%"),
            opacity = 1) 
