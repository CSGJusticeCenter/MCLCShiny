

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

warnings()

closeAllConnections() # Close connection to log file

end <- Sys.time()
end - start 


## Aggregate Plot Prep and Creation ############################################
# data frames and plots used in the app 
start <- Sys.time() #takes ~2 min
paste("Should be done by:", format(start+2*60, "%X"))

my_log <- file("logs/shiny_prep_log_hc_create.txt") # File name of output log

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


## Aggregate Plots HTMLs (used to created pngs for download) ###################
# create pngs for users to download 
start <- Sys.time() #takes ~100 min (1 hr + 40 min)
paste("Should be done by:", format(start+100*60, "%X"))

my_log <- file("logs/shiny_prep_log_hc_html.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("== CREATE PNGs FOR DOWNLOAD: HEX MAPS =================================")
print(paste("Last run", format(Sys.time(), "%a %b %e, %Y at %H:%M:%S %Z")))

# need to run 3a if haven't done so already 
print("-- 3a_highchart_fnc ---------------------------------------------------")
source("prep/3a_highchart_fnc.R", echo = TRUE)
print("-- 3c_highchart_png_hex -----------------------------------------------")
source("prep/3c_highchart_html.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

end <- Sys.time()
end - start 


## Aggregate Plot PNGS for download ############################################
# create pngs for users to download 
start <- Sys.time() #takes ~15 min (if updating all plots) 
paste("Should be done by:", format(start+15*60, "%X"))

my_log <- file("logs/shiny_prep_log_hc_pngs.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("== CREATE PNGs FOR DOWNLOAD: HEX MAPS =================================")
print(paste("Last run", format(Sys.time(), "%a %b %e, %Y at %H:%M:%S %Z")))

# need to run 3a if haven't done so already 
print("-- 3a_highchart_fnc ---------------------------------------------------")
source("prep/3a_highchart_fnc.R", echo = TRUE)
print("-- 3c_highchart_png_hex -----------------------------------------------")
source("prep/3d_highchart_png.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

end <- Sys.time()
end - start 