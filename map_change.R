temp <- mclc %>% 
  filter(adm_or_pop == "Admissions",
         year == "2020",
         metric == "Total")

vec_breaks <- c(      -50,       -20,       -10,         0,        10,        20,        50)
vec_rgb    <- c("#B05D24", "#B87647", "#BF8E6A", "#C7A78D", "#CFBFB0", "#D6D8D3", "#DEF0F6") 

#Add a colour column, and put in the appropriate RGB value
for (i in 1:length(temp$change)) {
  temp$colour[i] <- vec_rgb[min(which(vec_breaks > temp$change[i])) - 1]
}


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

# # set colors manually:
# pal <- colorFactor(
#   palette = c('#DEF0F6', '#D3CBC2', '#C7A78D', '#BC8259', '#B05D24'),
#   domain = mclc.df$states
# )

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
              fillColor = ~pal(mclc.df$change),
              
              # highlight options
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#355DA1"
              )
  ) %>%
  
  addLegend(pal = paletteNum, 
            values = mclc.df$change, 
            title = '<small><br></small>', 
            position = 'topright')

regional_map
