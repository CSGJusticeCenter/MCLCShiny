#######################################
# Project: MCLCShiny
# File: app.R
# Authors: Mari Roberts
# Date last updated: April 26, 2023 (MAR)
# Description:
#    Run ui and server
#######################################

# add fonts to shiny linux server
if (Sys.info()[['sysname']] == 'Linux') {
  dir.create('~/.fonts')
  fonts = c(
    "www/fonts/Graphik.ttf",
    "www/fonts/GraphikBold.ttf"
  )
  file.copy(fonts, "~/.fonts")
  system('fc-cache -f ~/.fonts')
}

# run ui and server code
source("ui.R")
source("server.R")

# launch shiny app
shinyApp(ui = ui, server = server)
