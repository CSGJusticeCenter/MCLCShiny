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
pal <- colorFactor(
  palette = c('#DEF0F6', '#D3CBC2', '#C7A78D', '#BC8259', '#B05D24'),
  domain = mclc.df$states
)

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
              fillColor = ~pal(mclc.df$total),
              
              # highlight options
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#355DA1"
              )
  ) %>%
  
  addLegend(pal = paletteNum, 
            values = mclc.df$total, 
            title = '<small><br></small>', 
            position = 'topright')

regional_map
