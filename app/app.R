#######################################
# Project: MCLCShiny
# File: app.R
# Authors: Mari Roberts
# Date last updated: June 7, 2022
# Description:
#    Run ui and server
#######################################


##when on shinyapps.io (linux) -- not sure if its required, please test
if (Sys.info()[['sysname']] == "Linux"){
  dir.create('~/.fonts')
  file.copy("www/fonts/Graphik.ttf", "~/.fonts")
  system('fc-cache -f ~/.fonts')
}

# run ui and server code
source("aui.R")
source("server.R")

# launch shiny app
shinyApp(ui = ui, server = server)
