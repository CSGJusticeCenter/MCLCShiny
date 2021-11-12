temp <- mclc %>% 
  filter(adm_or_pop == "Admissions",
         year == "2020",
         metric == "Total")

# #Add a colour column, and put in the appropriate RGB value
# vec_breaks <- c(      -70,       -60,      -50,        -40,      -20,        10,        20,        40,         50,        60,        70)
# vec_rgb    <- c("#2A5B71", "#387A96", "#4698Bc", "#6BADC9", "#B5D6E4", "#DAEAF2", "#E9F4D6", "#A5D35C", "#8FC833", "#72A029", "#56781F")
# for (i in 1:length(temp$change)) {
#   temp$colour[i] <- vec_rgb[min(which(vec_breaks > temp$change[i])) - 1]
# }

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
paletteNum <- colorFactor(
  palette = c("#2A5B71", "#387A96", "#4698Bc", "#6BADC9", "#B5D6E4", "#DAEAF2", 
              "#E9F4D6", "#A5D35C", "#8FC833", "#72A029", "#56781F"),
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
              fillColor = ~paletteNum(mclc.df$change),
              
              # highlight options
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#355DA1"
              )
  ) %>%
  
  addLegend("bottomright", 
            colors =c("#2A5B71", "#387A96", "#4698Bc", "#6BADC9", "#B5D6E4", "#DAEAF2", 
                      "#E9F4D6", "#A5D35C", "#8FC833", "#72A029", "#56781F", "#FFFFF", "#D3D3D3"),
            labels= c("-70","-60","-50","-40","-20","10","20","40","50","60","70", "", "No Data"),
            title= "% Change from Previous Year",
            opacity = 1)

regional_map
