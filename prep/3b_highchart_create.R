
box::use(
  ./box/admin
  , dplyr[...], 
  , glue[glue], 
  , purrr[map, walk]
  , stringr[str_remove]
)


# load data frames -------------------------------------------------------------

for (x in c("svii_agg", "svii_explorer", "svii_yr")){
  df <- readRDS(paste0("app/data/", x, ".rds")) |> tibble::as_tibble()
  assign(x, df)
  rm(df)
  rm(x)
}



# assign colors for visualizations ---------------------------------------------
source("app/colors.R")
# source("fnc_library.R")

# lists metrics for functions --------------------------------------------------

# list of metrics for function
metrics <- c("New Offense Violation",
             "Parole Violation",
             "Probation Violation",
             "Supervision Violation",
             "Technical Violation",
             "Total")


# NATL - HEX MAPS ##############################################################
natl_hex_lst  <- 
  map(#lev1 type 
    c("Admissions", "Population") |> set_names(), 
    ~{type <- .x
    map(#lev2 year_chg
      svii_yr$change_name |> set_names(), 
      ~{year_chg <- .x
      map(#lev3 metric 
        metrics |> set_names(), 
        ~{metric <- .x
        #FUNCTION TO CREATE HIGHCHART 
        fnc_hc_hex_map(type, year_chg, metric)
        # paste(type, year_chg, metric) # <-- if want to test nested list structure 
      }) #end of lev3
    }) #end of lev2
  }) #end of lev1

adm_pop_maps <- natl_hex_lst

# STATE - OVERVIEW AREA ########################################################

state_area_lst <-     
  map(#lev1 type
    c("Admissions", "Population") |> set_names(), 
    ~{type <- .x
    map(#lev2 state 
      state.name |> set_names(), 
      ~{state <- .x
      #FUNCTION TO CREATE HIGHCHART 
      fnc_hc_area(type, state)
      #paste(type, state) # <-- if want to test nested list structure 
      }) #end of lev2
    }) #end of lev2=1


all_state_area_adm <- state_area_lst$Admissions
all_state_area_pop <- state_area_lst$Population



# STATE - BAR CHARTS #########################################################

state_bar_lst <-     
  map(#lev1 type 
    c("Admissions", "Population") |> set_names(), 
    ~{type <- .x
    map(#lev2 year_chg
      c("Both", "Parole", "Probation") |> set_names(), 
      ~{supervision_type <- .x
      map(#lev3 metric 
        state.name |> set_names(), 
        ~{state <- .x
        #FUNCTION TO CREATE HIGHCHART 
        fnc_hc_bar(type, supervision_type, state)
        # paste(type, year_chg, metric) # <-- if want to test nested list structure 
        }) #end of lev3
      }) #end of lev2
    }) #end of lev1



all_state_bar_adm <- state_bar_lst$Admissions$Both
all_state_bar_pop <- state_bar_lst$Population$Both

parole_bar_adm <- state_bar_lst$Admissions$Parole
parole_bar_pop <- state_bar_lst$Population$Parole

probation_bar_adm <- state_bar_lst$Admissions$Probation
probation_bar_pop <- state_bar_lst$Population$Probation


# Save HC obj as RDS ###########################################################

# new names 
admin$save_rds_twice(natl_hex_lst)
admin$save_rds_twice(state_area_lst)
admin$save_rds_twice(state_bar_lst)

# old names 
admin$save_rds_twice(adm_pop_maps)
admin$save_rds_twice(all_state_area_adm)
admin$save_rds_twice(all_state_area_pop)
admin$save_rds_twice(all_state_bar_adm)
admin$save_rds_twice(all_state_bar_pop)
admin$save_rds_twice(parole_bar_adm)
admin$save_rds_twice(parole_bar_pop)
admin$save_rds_twice(probation_bar_adm)
admin$save_rds_twice(probation_bar_adm)

