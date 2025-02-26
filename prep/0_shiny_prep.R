

#############################################################
## Data Prep 

start <- Sys.time() #takes ~<1 min 
paste("Should be done by:", format(start, "%X"))

my_log <- file("logs/shiny_prep_log_dataprep.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("== DATA PREP ==========================================================")

print("-- 2a_import_survey ---------------------------------------------------")
source("prep/2a_import_survey.R", echo = TRUE)
print("-- 2b_import_text -----------------------------------------------------")
source("prep/2b_import_text.R", echo = TRUE)
print("-- 2c_import_hex0 -----------------------------------------------------")
source("prep/2c_import_hex.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

end <- Sys.time()
end - start 


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
