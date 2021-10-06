#######################################
# Project: MCLCShiny
# File: regional_map.R
# Authors: Mari Roberts
# Date: September 27, 2021
# Description: 
#    Creates 50 state interactive map
#######################################

# references:
# https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html
# https://stackoverflow.com/questions/46392640/shiny-leaflet-map-filtering-data-by-columns-not-rows?rq=1

# data for now
dat <- adm20
dat <- clean_names(dat)

# # leaflet map with states shapefile
# m <- leaflet() %>%
#   addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
#   setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
#   addPolygons(data = states,
#               weight = 1)
# m

# evaluates whether every element of dat$states is contained in states$NAME
# is.element(dat$states, states$NAME) %>%
#   all()

# merge data with shapefile
states <- merge(states.shp, dat, by.x = 'NAME', by.y = "states", all.x = F)

# drop Hawaii and Alaska for now
states <- states[!(states$NAME == 'Hawaii' | states$NAME == 'Alaska'), ]

# make data numeric
states$total_violation_admissions <- as.numeric(states$total_violation_admissions)

# map colors to continuous values, use colorNumeric(), specifying the color palette that values should be mapped to and the values
paletteNum <- colorNumeric('Blues', domain = states$total_violation_admissions)

# # Alternatively, we can map colors to bins of values instead of doing so continuously
# values range from ~7 cents to ~19 cents
# costBins <- c(7:19, Inf)
# paletteBinned <- colorBin('YlGnBu', domain = states$total_violation_admissions, bins = costBins)

# for labels: use sprintf(), lapply() and HTML() to generate a formatted, HTML-tagged label for each state
stateLabels <- sprintf('<b>%s</b><br/>%g Violation Admissions',
                       states$NAME, states$total_violation_admissions) %>%
               lapply(function(x) HTML(x))
states <- cbind(states, matrix(stateLabels, ncol = 1, dimnames = list(c(), c('stateLabels'))))

regional_map <- leaflet() %>%
  
  # map template
  addProviderTiles(providers$CartoDB.PositronNoLabels,
                   options = providerTileOptions(opacity = 0)) %>%
  
  # set view to US
  setView(lng = -96.25, lat = 39.50, zoom = 3.5) %>%
  
  addPolygons(data = states,
              
              # colors
              color = 'white',
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum(states$total_violation_admissions),
              
              # state labels
              label = ~stateLabels,
              labelOptions = labelOptions(
                style = list(color = 'gray30'),
                textsize = '10px'),
              
              # highlight options
              highlightOptions = highlightOptions(
                weight = 2,
                color = 'dodgerblue'
              )
  ) %>%
  
  addLegend(pal = paletteNum, 
            values = states$total_violation_admissions, 
            title = '<small>2020 Admissions due to <br> Supervision Violations<br></small>', 
            position = 'bottomleft')

regional_map
