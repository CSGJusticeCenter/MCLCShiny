
# assumes 3a_highchart_fnc has been run 

# NATL HEX MAPS DISPLAY ########################################################
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

admin$save_rds_twice(natl_hex_lst)

# old names (delete if no longer using in shiny app) 
adm_pop_maps <- natl_hex_lst
admin$save_rds_twice(adm_pop_maps)



# STATE AREA DISPLAY ###########################################################

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

admin$save_rds_twice(state_area_lst)

# old names (delete if no longer using in shiny app) 
all_state_area_adm <- state_area_lst$Admissions
all_state_area_pop <- state_area_lst$Population
admin$save_rds_twice(all_state_area_adm)
admin$save_rds_twice(all_state_area_pop)


# STATE BAR DISPLAY ############################################################

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

admin$save_rds_twice(state_bar_lst)


# old names (delete if no longer using in shiny app) 
all_state_bar_adm <- state_bar_lst$Admissions$Both
all_state_bar_pop <- state_bar_lst$Population$Both
parole_bar_adm <- state_bar_lst$Admissions$Parole
parole_bar_pop <- state_bar_lst$Population$Parole
probation_bar_adm <- state_bar_lst$Admissions$Probation
probation_bar_pop <- state_bar_lst$Population$Probation
admin$save_rds_twice(all_state_bar_adm)
admin$save_rds_twice(all_state_bar_pop)
admin$save_rds_twice(parole_bar_adm)
admin$save_rds_twice(parole_bar_pop)
admin$save_rds_twice(probation_bar_adm)
admin$save_rds_twice(probation_bar_adm)







