

## Data Prep ###################################################################

start <- Sys.time() #takes ~<1 min 
start
paste("Should be done by:", format(start+1*60, "%X"))
my_log <- file("logs/shiny_prep_log_dataprep.txt") # File name of output log


sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("== DATA PREP ==========================================================")
print(paste("Last run", format(start, "%a %b %e, %Y at %H:%M:%S %Z")))
print("-- 2a_import_survey ---------------------------------------------------")
source("prep/2a_import_survey.R", echo = TRUE)
print("-- 2b_import_text -----------------------------------------------------")
source("prep/2b_import_text.R", echo = TRUE)
warnings()
end <- Sys.time()
print(paste("End run", format(end, "%a %b %e, %Y at %H:%M:%S %Z")))
end - start 

closeAllConnections() # Close connection to log file


end
end - start 


# Documentation ################################################################

csgjcr::csg_render_ds(
  input = "docs/gen_documentation.qmd", 
  save_ds_version = FALSE, 
  output_file = csgjcr::csg_sp_path(
    "50 State Revocations Project/MCLC Shiny App", 
    "SVII_Shiny_Documentation.html"
  ) 
)


## Aggregate Plot Prep and Creation ############################################
# data frames and plots used in the app 
start <- Sys.time() #takes ~2 min
start
paste("Should be done by:", format(start+2*60, "%X"))
my_log <- file("logs/shiny_prep_log_hc_create.txt") # File name of output log


sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("== CREATE HIGHCHART OBJECTS ===========================================")
print(paste("Last run", format(start, "%a %b %e, %Y at %H:%M:%S %Z")))
print("-- 3a_highchart_fnc ---------------------------------------------------")
source("prep/3a_highchart_fnc.R", echo = TRUE)
print("-- 3b_highchart_create ------------------------------------------------")
source("prep/3b_highchart_create.R", echo = TRUE)
warnings()
end <- Sys.time()
print(paste("End run", format(end, "%a %b %e, %Y at %H:%M:%S %Z")))
end - start 

closeAllConnections() # Close connection to log file


end
end - start 


## Aggregate Plots HTMLs (used to created pngs for download) ###################
# create pngs for users to download; time depends on bandwidth of computer
# hex   72 items, ~10-30 min 
# area 100 items, ~20-60 min [10-30 min per 50 items, adm/pop]
# bar  300 items, ~15-90 min [ 5-30 min per 50 items, supervision/par/prob]
start <- Sys.time() #takes ~45 min-3 hours 
start
paste("Should be done by:", format(start+120*60, "%X"))
my_log <- file("logs/shiny_prep_log_hc_html.txt") # File name of output log


sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")
print("== CREATE HTMLS =======================================================")
print(paste("Last run", format(Sys.time(), "%a %b %e, %Y at %H:%M:%S %Z")))
# need to run 3a if haven't done so already 
print("-- 3a_highchart_fnc ---------------------------------------------------")
source("prep/3a_highchart_fnc.R", echo = TRUE)
print("-- 3c_highchart_html -----------------------------------------------")
source("prep/3c_highchart_html.R", echo = TRUE)
warnings()
end <- Sys.time()
print(paste("End run", format(end, "%a %b %e, %Y at %H:%M:%S %Z")))
end - start 

closeAllConnections() # Close connection to log file


end 
end - start 


## Aggregate Plot PNGS for download ############################################
# create pngs for users to download 
start <- Sys.time() #takes ~30 min  
start
paste("Should be done by:", format(start+30*60, "%X"))
my_log <- file("logs/shiny_prep_log_hc_pngs.txt") # File name of output log


sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("== SAVE PNGs FOR DOWNLOAD =============================================")
print(paste("Last run", format(start, "%a %b %e, %Y at %H:%M:%S %Z")))
# need to run 3a if haven't done so already 
print("-- 3a_highchart_fnc ---------------------------------------------------")
source("prep/3a_highchart_fnc.R", echo = TRUE)
print("-- 3d_highchart_png ---------------------------------------------------")
source("prep/3d_highchart_png.R", echo = TRUE)
warnings()
end <- Sys.time()
print(paste("End run", format(end, "%a %b %e, %Y at %H:%M:%S %Z")))
end - start 

closeAllConnections() # Close connection to log file


end
end - start 