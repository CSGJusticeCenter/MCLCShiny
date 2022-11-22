


Sys.time() #takes ~ 15 min 

my_log <- file("shiny_prep_log.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

####### takes ~ 10min
# working directory should be the root of the repository ~MCLCShiny/
# need to do this one first otherwise fonts will get messed up on pngs 
Sys.time()
box::use(prep/box/rri_infographs_tables)
rri_infographs_tables$prep_for_shiny("N")

########## takes ~ 1min
Sys.time()
source("import.R", echo = TRUE)


####### takes ~ 2min 
Sys.time()
source("highchart.R", echo = TRUE)


####### takes ~ 1min 
Sys.time()
source("reactable.R", echo = TRUE)


warnings()

closeAllConnections() # Close connection to log file

Sys.time()


