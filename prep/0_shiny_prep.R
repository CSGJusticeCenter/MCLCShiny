

#############################################################
## MCLC Data Prep

Sys.time() #takes ~ 2-4 min

my_log <- file("logs/shiny_prep_log_MCLCdata.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("MCLC Data Prep")

source("fnc_library.R", echo = TRUE)
source("import.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

Sys.time()


#############################################################
## MCLC Plot Prep, Creation, and PNGs

Sys.time() #takes ~2 min

my_log <- file("logs/shiny_prep_log_MCLCplot.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("MCLC Plot Prep, Creation, and PNGs")

source("highchart.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

Sys.time()



#############################################################
## MCLC Plot Prep, Creation, and PNGs

Sys.time() #takes ~15-20 min

my_log <- file("logs/shiny_prep_log_MCLCplotpngs.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("MCLC Plot Prep, Creation, and PNGs")

source("highchart_download.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

Sys.time()
