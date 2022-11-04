


Sys.time()

my_log <- file("prep/shiny_prep_log.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

# working directory should be the root of the repository ~MCLCShiny/
box::use(box/rri_infographs_tables)
rri_infographs_tables$prep_for_shiny() 

warnings()

closeAllConnections() # Close connection to log file

Sys.time()
