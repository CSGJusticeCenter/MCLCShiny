
box::use(
    ./admin
  , ./infograph 
  , ./calc
  , ./assignflags
  , dplyr[...]
  , tidyr[pivot_wider, drop_na]
  , purrr[...]
  , glue[glue]
  , scales[comma]
)

suppressed_vars <- c("RRI", "RATE", "REVCNT")



#' Create a table for a state of a single metric 
#' years are column names 
#' single metric are values 
#'
#' @param DATA 
#' @param whichYEARS 
#' @param whichRACE 
#' @param whichABB 
#' @param whichMETRIC 
#'
state_table_single_metric <- function(DATA, whichYEARS, whichRACE, whichSTATE, whichMETRIC, mult = 1){
  
  if (whichMETRIC %in% suppressed_vars){
    S_whichMETRIC <- as.character(glue("S_{whichMETRIC}"))
    addlvars <- c(S_whichMETRIC, "SUPPRESS")
  } else {
    addlvars <- c()
  }
  
  ex_grid <- expand.grid(
      OFFGENERAL = admin$lev_OFFGENERAL2[1:5]
    , RACE = whichRACE
    , RPTYEAR = whichYEARS
  )

  longdf <- bind_rows(
    DATA$R %>% 
      filter(
          RPTYEAR %in% whichYEARS
        , RACE    %in% whichRACE
        , STATE    ==  whichSTATE
      ) %>% 
      mutate(OFFGENERAL = admin$lev_OFFGENERAL2[1]) %>% 
      select(OFFGENERAL, RACE, RPTYEAR, all_of(whichMETRIC), all_of(addlvars)) 
    , 
    DATA$OR %>% 
      filter(
          RPTYEAR %in% whichYEARS
        , RACE    %in% whichRACE
        , STATE    ==  whichSTATE
        , OFFGENERAL %in% admin$lev_OFFGENERAL[1:4]
      ) %>% 
      select(OFFGENERAL, RACE, RPTYEAR, all_of(whichMETRIC), all_of(addlvars)) 
  ) %>% 
    full_join(., ex_grid, by = c("OFFGENERAL", "RACE", "RPTYEAR")) %>% 
    mutate(OFFGENERAL = factor(OFFGENERAL, levels = admin$lev_OFFGENERAL2)) %>% 
    mutate_at(vars(all_of(c(whichMETRIC, addlvars[1]))), ~ifelse(. == Inf, NA, .)) %>% 
    arrange(OFFGENERAL, RACE) 
  
  thisAccuracy <- case_when(
    whichMETRIC == "RRI" ~ 0.01
    , TRUE               ~ 1
  )
  
  
  asis <- longdf %>% 
    mutate_at(vars(all_of(whichMETRIC)), ~comma(.*mult, accuracy = thisAccuracy)) %>% 
    select(OFFGENERAL, RACE, RPTYEAR, all_of(whichMETRIC)) %>% 
    pivot_wider(names_from = RPTYEAR, values_from = all_of(whichMETRIC)) %>% 
    select(OFFGENERAL, RACE, all_of(as.character(whichYEARS))) %>% 
    arrange(OFFGENERAL, RACE)
  
  
  OUT <- list(
      "longdf" = longdf
    , "table_asis" = asis
    , "mult"       = mult
  )
  
  
  if (whichMETRIC %in% suppressed_vars){
    
    suppress <- longdf %>% 
      mutate_at(vars(all_of(S_whichMETRIC)), ~comma(.*mult, accuracy = thisAccuracy)) %>% 
      mutate_at(vars(all_of(S_whichMETRIC)), ~ifelse(SUPPRESS == 1 & !is.na(.), paste0("<", ., "*"), .)) %>% 
      select(OFFGENERAL, RACE, RPTYEAR, all_of(S_whichMETRIC)) %>% 
      pivot_wider(names_from = RPTYEAR, values_from = all_of(S_whichMETRIC)) %>% 
      select(OFFGENERAL, RACE, all_of(as.character(whichYEARS))) %>% 
      arrange(OFFGENERAL, RACE)
    
    OUT <- c(OUT, list("table_suppress" = suppress))
    
  } 
  
  return(OUT)

}


#' pull data for infographics 
#'
#' @param DATA 
#' @param whichRACE 
#' @param whichSTATE 
#'
#' @examples
data_for_info_graphic <- function(DATA, whichRACE, whichSTATE, whichPOP){

  DF <- DATA$R %>% 
    filter(RACE %in% whichRACE) %>% 
    filter(RPTYEAR == RECENT_YR) %>% 
    filter(STATE == whichSTATE) %>% 
    select(STATE, RPTYEAR, RACE, RRI, S_RRI, SUPPRESS) %>% 
    filter(!is.na(RRI), RRI != Inf)
  
  
  if (nrow(DF) == 0){
    outDF <- "NODATA"
    dataavail <- 0
    note <- case_when(
        whichSTATE == "Alabama"                    ~ "Alabama did not report any person with Black or Hispanic race/ethnicity."
      , whichSTATE == "Alaska"                     ~ "Alaska did not report any parole data in the NCRP data set."
      , whichSTATE == "Connecticut"                ~ "Connecticut did not report any parole data in the NCRP data set."
      , whichSTATE == "Hawaii" & whichPOP == "BJS" ~ "Hawaii does not have parole poulation data (BJS) broken out by race."
      , whichSTATE == "Louisiana"                  ~ "Majority of NCRP parole data for Louisiana is missing race/ethnicity. All the obersvations that are not missing are for a single race/ethnicity category: Hispanic."
      , whichSTATE == "Maine"  & whichPOP == "BJS" ~ "Maine has single digits counts of clients on parole who are listed as Black. The parole population (BJS) data lists the population estimate for Black clinets as 0, so rates based on this population cannot be calculated"
      , whichSTATE == "Michigan"                   ~ "Majority of NCRP parole data for Michigan is missing race/ethnicity. All the obersvations that are not missing are for a single race/ethnicity category: Hispanic."
      , whichSTATE == "Nevada" & whichPOP == "BJS" ~ "Nevada does not have parole poulation data (BJS) broken out by race"
      , whichSTATE == "Oklahoma"                   ~ "Majority of NCRP parole data for Oklahoma is missing race/ethnicity. All the obersvations that are not missing are for a single race/ethnicity category: Hispanic."
      , whichSTATE == "Oregon"                     ~ "Oregon did not report any parole data in the NCRP data set"
      , whichSTATE == "South Dakota"               ~ "South Dakota did not report race/ethnicity category for paroles in the NCRP data."
      , whichSTATE == "Vermont"                    ~ "Vermont did not report race/ethnicity category for paroles in the NCRP data."
      , 
    ) 
    
    flag <- "2"
    
  } 
  
  if (nrow(DF) > 0){
    outDF <- DF
    dataavail <- 1
    note <- NA
    
    suppress <- ifelse(1 %in% DF$SUPPRESS, 1, 0)
    totrow      <- nrow(DF)
    
    flag <- case_when(
        suppress == 1 & totrow == 1 ~ "1MS"
      , suppress == 1 & totrow == 2 ~ "1S"
      , suppress == 0 & totrow == 1 ~ "1M"
      , suppress == 0 & totrow == 2 ~ "0"
    )
    
    
  }
  
  
  
  OUT <- list(
      "DF" = outDF
    , "DATAAVAIL" = dataavail
    , "NOTE" = note
    , "FLAG" = flag
  )
  
  return(OUT)

}

#' Create and Save tables for the Racial and Ethnic Disparities Tab 
#'
#' @return
#' @export
#'
#' @examples
create_tables <- function(NCRPLET){
  
  
  REV_BJS <- calc$combine_and_calcrates("BJS", NCRPLET)
  REV_SC  <- calc$combine_and_calcrates("SC" , NCRPLET)
  # REV_BJS <- readRDS(file.path(admin$sp_data, glue("NCRP_{NCRPLET}_REV_BJS.RDS")))
  # REV_SC  <- readRDS(file.path(admin$sp_data, glue("NCRP_{NCRPLET}_REV_SC.RDS")))  
  
  #what 'recent_yr' is the most likely 
  yr_SC  <- REV_SC $OR %>% count(RECENT_YR) %>% filter(n == max(n)) %>% pull(RECENT_YR)
  yr_BJS <- REV_BJS$OR %>% count(RECENT_YR) %>% filter(n == max(n)) %>% pull(RECENT_YR)
  
  
  state_vec <- sort(levels(REV_SC$t$STATE)) %>% .[. != "District of Columbia"] 
  
  admin$mylog("Start creating tables, takes ~40-50 seconds")
  
  outtables <- list(
      "BJS" = map(
      state_vec %>% set_names(),
      ~list(
          "INFOGRAPH" = data_for_info_graphic(    REV_BJS,              admin$lev_RACE[2:3], .x, "BJS")
        , "RRI"       = state_table_single_metric(REV_BJS, 2015:yr_BJS, admin$lev_RACE[2:3], .x, "RRI")
        , "RATE"      = state_table_single_metric(REV_BJS, 2015:yr_BJS, admin$lev_RACE[1:3], .x, "RATE", mult = 1e+03)
        , "REVCNT"    = state_table_single_metric(REV_BJS, 2015:yr_BJS, admin$lev_RACE[1:3], .x, "REVCNT")
        , "POPEST"    = state_table_single_metric(REV_BJS, 2015:yr_BJS, admin$lev_RACE[1:3], .x, "POPEST")
      ) #end list 
    ) #end map BJS
    ,   "SC" = map(
      state_vec %>% set_names(),
      ~list(
          "INFOGRAPH" = data_for_info_graphic(    REV_SC,               admin$lev_RACE[2:3], .x, "SC")
        , "RRI"       = state_table_single_metric(REV_SC,  2015:yr_SC,  admin$lev_RACE[2:3], .x, "RRI")
        , "RATE"      = state_table_single_metric(REV_SC,  2015:yr_SC,  admin$lev_RACE[1:3], .x, "RATE", mult = 1e+05)
        , "REVCNT"    = state_table_single_metric(REV_SC,  2015:yr_SC,  admin$lev_RACE[1:3], .x, "REVCNT")
        , "POPEST"    = state_table_single_metric(REV_SC,  2015:yr_SC,  admin$lev_RACE[1:3], .x, "POPEST")
      ) #end list 
    ) #end map SC
    , "STATEVEC" = state_vec
    , "NCRPLET"  = NCRPLET
  )
  
  admin$mylog("End creating tables")
  
  admin$SPsaveRDS(outtables, glue("NCRP_{NCRPLET}_RRI_tables.RDS"))
  
  assignflags$export(tables = outtables, popdenom = "BJS")
  assignflags$export(tables = outtables, popdenom = "SC")
  
  return(outtables)
  
}



#' Prep for Shiny App - create tables  and infogrpahics 
#' tables: ~30-60 sec
#' infogrpahics: ~10-11 min
#' total time: ~12 min
#'
#' @return
#' @export
#'
#' @examples
prep_for_shiny <- function(NCRPLET){
  
  admin$mylog("!!START PREP FOR SHINY")
  ncrpmessage <- case_when(
      NCRPLET == "A" ~ "NCRP data is from ADMISSIONS"
    , NCRPLET == "N" ~ "NCRP data is from YEAREND POPULATION"
  )
  admin$mylog(ncrpmessage)
  
  
  tables <- create_tables(NCRPLET)
  state_vec <- tables$STATEVEC
  
  
  admin$mylog("Start creating infographics")
  
  #remove old infographs 
  #remove from sharepoint 
  png_lst <- list.files(file.path(admin$sp_data, "infographs"), pattern = "*.png")
  purrr::walk(png_lst, ~file.remove(file.path(file.path(admin$sp_data, "infographs", .x))))
  #remove from clone  
  png_lst <- list.files("app/data/infogs", pattern = "*.png")
  purrr::walk(png_lst, ~file.remove(file.path(file.path("app/data/infogs", .x))))
  
  params_for_loop <- tidyr::expand_grid(POP = c("BJS", "SC"), STATE = state_vec)
  n_of_loops <- nrow(params_for_loop)
  
  for (i in 1:n_of_loops){
    admin$mylog(glue("{i} out of {n_of_loops}"))
    
    whichPOP   <- params_for_loop$POP[i]
    whichSTATE <- params_for_loop$STATE[i]
    
    
    df <- tables[[whichPOP]][[whichSTATE]]$INFOGRAPH$DF
    dataavail <- tables[[whichPOP]][[whichSTATE]]$INFOGRAPH$DATAAVAIL
    
    if (dataavail == 1){
      pwalk(
        list(
            rri_raw = df$S_RRI #do suppressed value (only 2 instances Idaho/West Virigina, both Hispanic)
          , suppress = df$SUPPRESS
          , race = df$RACE
          , label   = paste0(whichPOP, "_", df$STATE, "_", df$RACE)
          , savefile= TRUE
          , infogs  = ifelse(df$RRI <= 10, 10, 20)
        )
        , infograph$create_infograph 
      )
    } else if (dataavail == 0) {
      admin$mylog(glue("{whichPOP} {whichSTATE} does NOT have data for infographics"))
    } else {
      stop("error with DF")
    }
    
  }
  
  admin$mylog("End   creating infographics")
  
  
  admin$mylog("Copy files exported to sharepoint to local app repo")
  
  
  file.copy(
    from = file.path(admin$sp_data, glue("NCRP_{NCRPLET}_RRI_tables.RDS"))
    , to = "app/data/NCRP_RRI_tables.RDS"
    , overwrite = TRUE
  )
  
  png_lst <- list.files(file.path(admin$sp_data, "infographs"), pattern = "*.png")
  
  walk(
    png_lst
    , ~file.copy(
      from = file.path(admin$sp_data, "infographs", .x)
      , to = file.path("app/data/infogs", .x)
      , overwrite = TRUE
    )
  )
  
  
  admin$mylog("!!END   PREP FOR SHINY")
  
}


