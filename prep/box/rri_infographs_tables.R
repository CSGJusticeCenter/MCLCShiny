
box::use(
    ./admin
  , ./infograph 
  , dplyr[...]
  , tidyr[pivot_wider, drop_na]
  , purrr[...]
  , glue[glue]
)



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
state_table_single_metric <- function(DATA, whichYEARS, whichRACE, whichSTATE, whichMETRIC){

  bind_rows(
    DATA$R %>% 
      filter(
          RPTYEAR %in% whichYEARS
        , RACE    %in% whichRACE
        , STATE    ==  whichSTATE
      ) %>% 
      mutate(OFFGENERAL = admin$lev_OFFGENERAL2[1]) %>% 
      select(OFFGENERAL, RACE, RPTYEAR, all_of(whichMETRIC)) %>% 
      pivot_wider(names_from = RPTYEAR, values_from = all_of(whichMETRIC))
    , 
    DATA$OR %>% 
      filter(
          RPTYEAR %in% whichYEARS
        , RACE    %in% whichRACE
        , STATE    ==  whichSTATE
        , OFFGENERAL %in% admin$lev_OFFGENERAL[1:4]
      ) %>% 
      select(OFFGENERAL, RACE, RPTYEAR, all_of(whichMETRIC)) %>% 
      pivot_wider(names_from = RPTYEAR, values_from = all_of(whichMETRIC))
  )

}


#' pull data for infographics 
#'
#' @param DATA 
#' @param whichRACE 
#' @param whichSTATE 
#'
#' @examples
data_for_info_graphic <- function(DATA, whichRACE, whichSTATE){

  DF <- DATA$R %>% 
    filter(RACE %in% whichRACE) %>% 
    filter(RPTYEAR == RECENT_YR) %>% 
    filter(STATE == whichSTATE) %>% 
    select(STATE, RPTYEAR, RACE, RRI) %>% 
    filter(!is.na(RRI), RRI != Inf)
  
  
  if (nrow(DF) == 0){
    OUT <- "NODATA"
  } 
  
  if (nrow(DF) > 0){
    OUT <- DF
  }
  
  return(OUT)

}

#' Create and Save tables for the Racial and Ethnic Disparities Tab 
#'
#' @return
#' @export
#'
#' @examples
create_tables <- function(){
  
  REV_BJS <- readRDS(file.path(admin$sp_data, "NCRP_REV_APS.RDS")) 
  REV_SC  <- readRDS(file.path(admin$sp_data, "NCRP_REV_SC.RDS"))  
  
  state_vec <- sort(levels(REV_SC$t$STATE)) %>% .[. != "District of Columbia"]
  
  admin$mylog("Start creating tables")
  
  tables <- list(
      "BJS" = map(
      state_vec %>% set_names(),
      ~list(
          "INFOGRAPH" = data_for_info_graphic(    REV_BJS,            admin$lev_RACE[2:3], .x)
        , "RRI"       = state_table_single_metric(REV_BJS, 2015:2018, admin$lev_RACE[2:3], .x, "RRI")
        , "RATE_100K" = state_table_single_metric(REV_BJS, 2015:2018, admin$lev_RACE[1:3], .x, "RATE_100K")
        , "REVCNT"    = state_table_single_metric(REV_BJS, 2015:2018, admin$lev_RACE[1:3], .x, "REVCNT")
        , "POPEST"    = state_table_single_metric(REV_BJS, 2015:2018, admin$lev_RACE[1:3], .x, "POPEST")
      ) #end list 
    ) #end map BJS
    ,   "SC" = map(
      state_vec %>% set_names(),
      ~list(
          "INFOGRAPH" = data_for_info_graphic(    REV_SC,             admin$lev_RACE[2:3], .x)
        , "RRI"       = state_table_single_metric(REV_SC,  2015:2019, admin$lev_RACE[2:3], .x, "RRI")
        , "RATE_100K" = state_table_single_metric(REV_SC,  2015:2019, admin$lev_RACE[1:3], .x, "RATE_100K")
        , "REVCNT"    = state_table_single_metric(REV_SC,  2015:2019, admin$lev_RACE[1:3], .x, "REVCNT")
        , "POPEST"    = state_table_single_metric(REV_SC,  2015:2018, admin$lev_RACE[1:3], .x, "POPEST")
      ) #end list 
    ) #end map SC
    , "STATEVEC" = state_vec
  )
  
  admin$mylog("End creating tables")
  
  admin$SPsaveRDS(tables, "NCRP_RRI_tables.RDS")
  
  return(tables)
  
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
prep_for_shiny <- function(){
  
  admin$mylog("!!START PREP FOR SHINY")
  
  tables <- create_tables()
  state_vec <- tables$STATEVEC
  
  params_for_loop <- tidyr::expand_grid(POP = c("BJS", "SC"), STATE = state_vec)
  n_of_loops <- nrow(params_for_loop)
  
  admin$mylog("Start creating infographics")
  
  for (i in 1:n_of_loops){
    
    admin$mylog(glue("{i} out of {n_of_loops}"))
    
    whichPOP   <- params_for_loop$POP[i]
    whichSTATE <- params_for_loop$STATE[i]
    
    
    df <- tables[[whichPOP]][[whichSTATE]]$INFOGRAPH
    
    if (is.data.frame(df)){
      pwalk(
        list(
            rri_raw = df$RRI
          , race = df$RACE
          , label   = paste0(whichPOP, "_", df$STATE, "_", df$RACE)
          , savefile= TRUE
          , infogs  = ifelse(df$RRI <= 10, 10, 20)
        )
        , infograph$create_infograph 
      )
    } else if (is.character(df) & df == "NODATA") {
      admin$mylog(glue("{whichPOP} {whichSTATE} does NOT have data for infographics"))
    } else {
      stop("error with DF")
    }
    
  }
  
  admin$mylog("End   creating infographics")
  
  
  admin$mylog("Copy files exported to sharepoint to local app repo")
  
  
  file.copy(
    from = file.path(admin$sp_data, "NCRP_RRI_tables.RDS")
    , to = "../../app/data/NCRP_RRI_tables.RDS"
    , overwrite = TRUE
  )
  
  png_lst <- list.files(file.path(admin$sp_data, "infographs"), pattern = "*.png")
  
  walk(
    png_lst
    , ~file.copy(
      from = file.path(admin$sp_data, "infographs", .x)
      , to = file.path("../../app/data/infogs", .x)
      , overwrite = TRUE
    )
  )
  
  
  admin$mylog("!!END   PREP FOR SHINY")
  
}


