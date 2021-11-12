# load visualization help
source("C:/Users/mroberts/OneDrive - The Council of State Governments/Desktop/csgjc/repos/csgjc_style_guidelines/csgjc_style_guidelines.R")

source("import.R")
source("ui.R")
source("server.R")

# run app
shinyApp(ui = ui, server = server)


# notes
# https://bootcamp.uxdesign.cc/how-i-built-a-data-visualization-color-palette-for-a-fortune-500-company-cf01d8a66451