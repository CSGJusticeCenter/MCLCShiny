

## Data Prep ###################################################################

start <- Sys.time() #takes ~<1 min 
paste("Should be done by:", format(start+1*60, "%X"))

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


## Aggregate Plot Prep and Creation ############################################
# data frames and plots used in the app 
start <- Sys.time() #takes ~2 min
paste("Should be done by:", format(start+2*60, "%X"))

my_log <- file("logs/shiny_prep_log_highchart.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("== CREATE HIGHCHART OBJECTS ===========================================")
print(paste("Last run", format(Sys.time(), "%a %b %e, %Y at %H:%M:%S %Z")))

print("-- 3a_highchart_fnc ---------------------------------------------------")
source("prep/3a_highchart_fnc.R", echo = TRUE)
print("-- 3b_highchart_create ------------------------------------------------")
source("prep/3b_highchart_create.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

end <- Sys.time()
end - start 



## Aggregate Plot PNGS for download: hex maps ##################################
# create pngs for users to download 
start <- Sys.time() #takes ~30 min 
paste("Should be done by:", format(start+30*60, "%X"))

my_log <- file("logs/shiny_prep_log_pngs_hex.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("== CREATE PNGs FOR DOWNLOAD: HEX MAPS =================================")
print(paste("Last run", format(Sys.time(), "%a %b %e, %Y at %H:%M:%S %Z")))

# need to run 3a if haven't done so already 
print("-- 3a_highchart_fnc ---------------------------------------------------")
source("prep/3a_highchart_fnc.R", echo = TRUE)
print("-- 3c_highchart_png_hex -----------------------------------------------")
source("prep/3c_highchart_png_hex.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

end <- Sys.time()
end - start 


## Aggregate Plot PNGS for download: area ######################################

start <- Sys.time() #takes ~30 min 
paste("Should be done by:", format(start+30*60, "%X"))

my_log <- file("logs/shiny_prep_log_pngs_area.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("== CREATE PNGs FOR DOWNLOAD: AREA =====================================")
print(paste("Last run", format(Sys.time(), "%a %b %e, %Y at %H:%M:%S %Z")))

# need to run 3a if haven't done so already 
print("-- 3a_highchart_fnc ---------------------------------------------------")
source("prep/3a_highchart_fnc.R", echo = TRUE)
print("-- 3d_highchart_png_area ----------------------------------------------")
source("prep/3c_highchart_png_area.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

end <- Sys.time()
end - start 



## Aggregate Plot PNGS for download: bar ######################################

start <- Sys.time() #takes ~90 min 
paste("Should be done by:", format(start+30*90, "%X"))

my_log <- file("logs/shiny_prep_log_pngs_bar.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("== CREATE PNGs FOR DOWNLOAD: BAR ======================================")
print(paste("Last run", format(Sys.time(), "%a %b %e, %Y at %H:%M:%S %Z")))

# need to run 3a if haven't done so already 
print("-- 3a_highchart_fnc ---------------------------------------------------")
source("prep/3a_highchart_fnc.R", echo = TRUE)
print("-- 3c_highchart_png_bar -----------------------------------------------")
source("prep/3e_highchart_png_bar.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

end <- Sys.time()
end - start 
