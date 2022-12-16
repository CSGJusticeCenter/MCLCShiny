
#############################################################
## HAVE YOU UPDATED THE ROOT FILE TO MATCH YOUR PATHWAY??? ##
#############################################################

# run start to finish takes ~1-1.5 hours 

#############################################################
## Revocation Counts and Race and Ethnicity 

Sys.time() #takes ~ 25 min 

my_log <- file("shiny_prep_log_REVRE.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("Revocation Counts and Race and Ethnicity")

####### takes ~ 20min
# working directory should be the root of the repository ~MCLCShiny/
# need to do this one first otherwise fonts will get messed up on pngs 
Sys.time()
box::use(prep/box/rri_infographs_tables)
rri_infographs_tables$prep_for_shiny()

# overview html and check 
csgjcr::csg_render_ds(
    "prep/rri_overview.qmd"
  , file.path(
       csgjcr::csg_sp_path("50 State Revocations Project/MCLC Shiny App/products")
      , "REVCNT_RRI_Overview.html"
    )
)

warnings()

closeAllConnections() # Close connection to log file

Sys.time()



#############################################################
## MCLC Data Prep  

Sys.time() #takes ~ 2-4 min 

my_log <- file("shiny_prep_log_MCLCdata.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("MCLC Data Prep")

source("import.R", echo = TRUE)

source("reactable.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

Sys.time()


#############################################################
## MCLC Plot Prep, Creation, and PNGs  

Sys.time() #takes ~2 min 

my_log <- file("shiny_prep_log_MCLCplot.txt") # File name of output log

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

my_log <- file("shiny_prep_log_MCLCplotpngs.txt") # File name of output log

sink(my_log, append = FALSE, type = "output") # Writing console output to log file
sink(my_log, append = FALSE, type = "message")

print("MCLC Plot Prep, Creation, and PNGs")

source("highchart_pngs.R", echo = TRUE)

warnings()

closeAllConnections() # Close connection to log file

Sys.time()