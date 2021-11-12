temp <- mclc %>% 
  filter(adm_or_pop == "Admissions",
         year == "2020",
         metric == "Total")

# merge data with shapefile
mclc.df <- merge(states.shp, temp, by.x = 'NAME', by.y = "states")

# drop states
mclc.df <- mclc.df[!(mclc.df$NAME == 'Commonwealth of the Northern Mariana Islands' | 
                       mclc.df$NAME == 'American Samoa' |
                       mclc.df$NAME == 'Guam' |
                       mclc.df$NAME == 'District of Columbia' |
                       mclc.df$NAME == 'GUam' | 
                       mclc.df$NAME == 'Puerto Rico' |
                       mclc.df$NAME == 'United States Virgin Islands'), ]

# set colors manually:
# paletteNum <- colorFactor(
#   palette = c('#DEF0F6', '#D3CBC2', '#C7A78D', '#BC8259', '#B05D24'),
#   # palette = c('#B05D24', '#915D43', '#735D63', '#545D82', '#355DA1'),
#   domain = mclc.df$states
# )
# palette_rev <- rev(brewer.pal(5, "Oranges"))

paletteNum <- colorNumeric(palette = c('#004c6d', '#005f82', '#009bc2', '#00b0d7', '#00dcff'), 
                           domain = mclc.df$change)

regional_map <- leaflet() %>%
  
  # map template
  addProviderTiles(providers$CartoDB.PositronNoLabels,
                   options = providerTileOptions(opacity = 0)) %>%
  
  # set view to US
  setView(lng = -96.25, lat = 39.50, zoom = 3.5) %>%
  
  addPolygons(data = mclc.df,
              
              # colors
              color = 'white',
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum(mclc.df$change),
              
              # highlight options
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#355DA1"
              )) %>% 
  addLegend(pal = paletteNum , 
            values = mclc.df$change, 
            title = '<small>2018 Avg. Electricity Cost<br>(cents/kWh | source: US EIA)</small>', 
            position = 'bottomleft')
   

regional_map

