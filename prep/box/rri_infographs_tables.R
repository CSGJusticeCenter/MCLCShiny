
box::use(
    ./admin
  , ./infograph 
  , ./calc
  , ./assignflags
  , dplyr[...]
  , tidyr[pivot_wider, drop_na]
  , purrr[...]
  , glue[glue]
  , glue[glue]
  , scales[comma]
)

state_vec <- datasets::state.name

suppressed_vars <- c("RRI", "RATE", "REVCNT")

revcnt_notes <- readr::read_csv(file.path(admin$sp_data_raw, "notes/revcnt_notes.csv"), show_col_types = FALSE)




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
    whichMETRIC == "RRI" ~ 0.1
    , TRUE               ~ 1
  )
  
  
  asis <- longdf %>% 
    mutate_at(vars(all_of(whichMETRIC)), ~admin$roundedval(.*mult, accuracy = thisAccuracy)) %>% 
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
      mutate_at(vars(all_of(S_whichMETRIC)), ~admin$roundedval(.*mult, accuracy = thisAccuracy)) %>% 
      mutate_at(vars(all_of(S_whichMETRIC)), ~ifelse(SUPPRESS == 1 & !is.na(.), admin$addsuppressasterick(.), .)) %>% 
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
data_for_info_graphic <- function(DATA, whichRACE, whichSTATE, whichPOP, NCRPLET){

  DF <- DATA$R %>% 
    filter(RACE %in% whichRACE) %>% 
    filter(RPTYEAR == RECENT_YR) %>% 
    filter(STATE == whichSTATE) %>% 
    select(STATE, RPTYEAR, RACE, RRI, S_RRI, SUPPRESS) %>% 
    filter(!is.na(RRI), RRI != Inf)
  
  
  if (nrow(DF) == 0){
    outDF <- "NODATA"
    dataavail <- 0
    
    note <- revcnt_notes %>% 
      filter(STATE == whichSTATE, ncrp == NCRPLET, popdenom == whichPOP) %>% 
      pull(note)
    
    flag <- "2"
    
  } 
  
  if (nrow(DF) > 0){
    outDF <- DF
    dataavail <- 1
    note <- NA
    
    suppress <- ifelse(1 %in% DF$SUPPRESS, 1, 0)
    totrow      <- nrow(DF)
    
    flag <- case_when(
        suppress == 1 & totrow == 1 ~ "1MS" #missing cells and suppressed cells
      , suppress == 1 & totrow == 2 ~ "1S"  #suppressed cells
      , suppress == 0 & totrow == 1 ~ "1M"  #missing cells
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


how_its_calc_txt <- function(DATA, whichRACE, whichSTATE, whichPOP, mult, NCRPLET){

  
  DF <- DATA$R %>% 
    #filter for specific state and races, pick most recent year 
    filter(STATE == whichSTATE, RACE %in% whichRACE, RPTYEAR == RECENT_YR) %>% 
    #format RATE/RRI 
    mutate(
        shownRRI  = admin$roundedval(S_RRI,       accuracy = 0.1)
      , shownRATE = admin$roundedval(S_RATE*mult, accuracy = 1)
    ) %>% 
    # add asterik if suppressed 
    mutate_at(vars(shownRRI, shownRATE), ~ifelse(SUPPRESS == 1 & !is.na(.), admin$addsuppressasterick(.), .)) %>% 
    # remove any NA or Inf RRI rows 
    filter(!is.na(S_RRI), S_RRI != Inf)
  
  
  if (nrow(DF) < 2){
    out <- ""
    dataavail <- 0
    
  } 
  
  
  if (nrow(DF) >= 2){
    
    pre_txt <- case_when(
        NCRPLET == "A" ~ ""
      , NCRPLET == "N" ~ "On any given day, "
    )
    
    suf_txt <- case_when(
        NCRPLET == "A" & whichPOP == "BJS" ~ "serving parole sentences are re-admitted to prison each year"
      , NCRPLET == "A" & whichPOP == "CEN" ~ "from the community are re-admitted to prison after being revoked from parole each year"
      , NCRPLET == "N" & whichPOP == "BJS" ~ "who were revoked from parole sentences remain incarcerated"
      , NCRPLET == "N" & whichPOP == "CEN" ~ "from the community are in prison after being revoked from parole"
    )
    
    multshow <- scales::comma(mult, accuracy = 1)
    
    rate_lst <- map2(
        DF$shownRATE
      , DF$RACE
      , ~ifelse(
          .x == "No Data"
        , ""
        , glue("<li>{pre_txt}{.x} in {multshow} {.y} individuals {suf_txt}</li>")
      )
    ) %>% paste(., collapse = "")
    
    
    
    whiterate <- filter(DF, RACE == "White")$shownRATE
    
    
    rri_lst <- pmap(
      list(
          rate = filter(DF, RACE != admin$lev_RACE[1])$shownRATE
        , race = filter(DF, RACE != admin$lev_RACE[1])$RACE
        , rri  = filter(DF, RACE != admin$lev_RACE[1])$shownRRI
      )
      , function( rate, race, rri){
          glue("<li>The relative rate index for {race} individuals is {rate}/{whiterate} = {rri}</li>")
      }
    )  %>% 
      paste(., collapse = "")
    
    
    anysuppress <- ifelse(sum(DF$SUPPRESS) == 0, "", "<br>&#10033; Asterisk indicates situations where the parole revocation counts were less than 5.") 
    
    
    if (nrow(DF) == 2){
      
      note <- revcnt_notes %>% 
        filter(STATE == whichSTATE, ncrp == NCRPLET, popdenom == whichPOP) %>% 
        pull(note) %>% 
        paste("<br>", .)
      
      
    } else {
      note <- "" 
    }
    

    
    
    
    out <- paste0(
        "<div class = 'notetxt' style = 'text-align: left;'>"
      ,   "<p><b>How it's calculated:</b><br></p>"
      ,   "First, calculate the rate of parole revocations within each Racial/Ethnic group:<br>"
      ,   "<ul class = 'calctxt'>"
      ,     rate_lst
      ,   "</ul>"
      ,   "The Relative Rate Index (RRI) is calculated by dividing the rate for Black individuals or Hispanic individuals by the rate for White individuals. In this case:<br>"
      ,   "<ul class = 'calctxt'>"
      ,     rri_lst
      ,   "</ul>"
      ,   note
      ,   anysuppress
      , "</div>"
    )
    
  }

  
  return(out) 
}

#' Create and Save tables for the Racial and Ethnic Disparities Tab for specific NCPR data source 
#'
#' @return
#' @export
#'
#' @examples
create_single_table <- function(NCRPLET){
  
  REV_BJS <- calc$combine_and_calcrates("BJS" , NCRPLET)
  REV_CEN <- calc$combine_and_calcrates("PUMS", NCRPLET)
  # REV_BJS <- readRDS(file.path(admin$sp_data, glue("NCRP_{NCRPLET}_REV_BJS.RDS")))
  # REV_CEN <- readRDS(file.path(admin$sp_data, glue("NCRP_{NCRPLET}_REV_PUMS.RDS")))  
  
  #what 'recent_yr' is the most likely 
  yr_CEN <- 2020#REV_CEN$OR %>% count(RECENT_YR) %>% filter(n == max(n)) %>% pull(RECENT_YR)
  yr_BJS <- 2018#REV_BJS$OR %>% count(RECENT_YR) %>% filter(n == max(n)) %>% pull(RECENT_YR)
  
  
  bjs_mult <- 1e+03
  cen_mult <- 1e+05
  
  admin$mylog(glue("{NCRPLET} tables, takes ~40-50 seconds"))
  
  outtables <- list(
      "BJS" = map(
      state_vec %>% set_names(),
      ~list(
          "INFOGRAPH" = data_for_info_graphic(    REV_BJS,              admin$lev_RACE[2:3], .x, "BJS", NCRPLET)
        , "CALCTXT"   = how_its_calc_txt(         REV_BJS,              admin$lev_RACE[1:3], .x, "BJS" , mult = bjs_mult, NCRPLET = NCRPLET)
        , "RRI"       = state_table_single_metric(REV_BJS, 2015:yr_BJS, admin$lev_RACE[2:3], .x, "RRI")
        , "RATE"      = state_table_single_metric(REV_BJS, 2015:yr_BJS, admin$lev_RACE[1:3], .x, "RATE", mult = bjs_mult)
        , "REVCNT"    = state_table_single_metric(REV_BJS, 2015:yr_BJS, admin$lev_RACE[1:3], .x, "REVCNT")
        , "POPEST"    = state_table_single_metric(REV_BJS, 2015:yr_BJS, admin$lev_RACE[1:3], .x, "POPEST")
      ) #end list 
    ) #end map BJS
    ,   "CEN" = map(
      state_vec %>% set_names(),
      ~list(
          "INFOGRAPH" = data_for_info_graphic(    REV_CEN,               admin$lev_RACE[2:3], .x, "CEN", NCRPLET)
        , "CALCTXT"   = how_its_calc_txt(         REV_CEN,               admin$lev_RACE[1:3], .x, "CEN" , mult = cen_mult, NCRPLET = NCRPLET)
        , "RRI"       = state_table_single_metric(REV_CEN, 2015:yr_CEN,  admin$lev_RACE[2:3], .x, "RRI")
        , "RATE"      = state_table_single_metric(REV_CEN, 2015:yr_CEN,  admin$lev_RACE[1:3], .x, "RATE", mult = cen_mult)
        , "REVCNT"    = state_table_single_metric(REV_CEN, 2015:yr_CEN,  admin$lev_RACE[1:3], .x, "REVCNT")
        , "POPEST"    = state_table_single_metric(REV_CEN, 2015:yr_CEN,  admin$lev_RACE[1:3], .x, "POPEST")
      ) #end list 
    ) #end map SC
    , "STATEVEC" = state_vec 
    , "NCRPLET"  = NCRPLET
  )
  
  admin$mylog(glue("{NCRPLET} tables end"))
  
  
  assignflags$export(tables = outtables, popdenom = "BJS")
  assignflags$export(tables = outtables, popdenom = "CEN")
  
  return(outtables)
  
}



#' Create tables 
#'
#' @return
#' @export
#'
#' @examples
create_tables <- function(){
  
  
  outtables <- list(
      "Admissions" = create_single_table("A") #this should match option on shiny 
    , "Population" = create_single_table("N") #this should match option on shiny 
    , "STATEVEC"   = state_vec 
  )
  
  
  admin$SPsaveRDS(outtables, glue("NCRP_RRI_tables.RDS"))
  
  return(outtables)
  
}

#' Prep for Shiny App - create tables  and infogrpahics 
#' total time: ~5 min
#'
#' @return
#' @export
#'
#' @examples
prep_for_shiny_DATA <- function(){
  
  admin$mylog("!!START PREP FOR SHINY - DATA")
  
  tables <- create_tables()
  state_vec <- tables$STATEVEC

  file.copy(
    from = file.path(admin$sp_data, glue("NCRP_RRI_tables.RDS"))
    , to = "app/data/NCRP_RRI_tables.RDS"
    , overwrite = TRUE
  )
  
  admin$mylog("!!END   PREP FOR SHINY - DATA")
  
}



#' pREP FOR SHINY - CREATE INFOGRAPHIC PNGS 
#' total time: ~20 min 
#'
#' @return
#' @export
#'
#' @examples
prep_for_shiny_PNG <- function(){
  
  admin$mylog("!!START PREP FOR SHINY - PNG")
  
  tables <- file.path(admin$sp_data, glue("NCRP_RRI_tables.RDS"))
  
  admin$mylog("Infographics - Start, takes ~20 min to create")
  
  #remove old infographs 
  #remove from sharepoint 
  png_lst <- list.files(file.path(admin$sp_data, "infographs"), pattern = "*.png")
  purrr::walk(png_lst, ~file.remove(file.path(file.path(admin$sp_data, "infographs", .x))))
  
  #remove from clone  
  png_lst <- list.files("app/data/infogs", pattern = "*.png")
  purrr::walk(png_lst, ~file.remove(file.path(file.path("app/data/infogs", .x))))
  
  params_for_loop <- tidyr::expand_grid(
    NCRP = names(tables)[1:2]
    , STATE = state_vec
    , POP = c("BJS", "CEN")
  )
  n_of_loops <- nrow(params_for_loop)
  
  for (i in 1:n_of_loops){
    admin$mylog(glue("{i} out of {n_of_loops}"))
    
    whichPOP   <- params_for_loop$POP[i]
    whichSTATE <- params_for_loop$STATE[i]
    whichNCRP  <- params_for_loop$NCRP[i]
    
    df        <- tables[[whichNCRP]][[whichPOP]][[whichSTATE]]$INFOGRAPH$DF
    dataavail <- tables[[whichNCRP]][[whichPOP]][[whichSTATE]]$INFOGRAPH$DATAAVAIL
    
    if (dataavail == 1){
      pwalk(
        list(
          rri_raw = df$S_RRI #do suppressed value (only 2 instances Idaho/West Virigina, both Hispanic)
          , suppress = df$SUPPRESS
          , race = df$RACE
          , label   = paste0(whichNCRP, "_", whichSTATE, "_", whichPOP, "_", df$RACE)
          , savefile= TRUE
          , infogs  = ifelse(df$S_RRI <= 11, 11, 22)
        )
        , infograph$create_infograph 
      )
    } else if (dataavail == 0) {
      admin$mylog(glue("{whichNCRP} {whichSTATE} {whichPOP} does NOT have data for infographics"))
    } else {
      stop("error with DF")
    }
    
  }
  
  admin$mylog("Infographic - End")
  
  
  admin$mylog("Copy files exported to sharepoint to local app repo")
  
  png_lst <- list.files(file.path(admin$sp_data, "infographs"), pattern = "*.png")
  
  walk(
    png_lst
    , ~file.copy(
      from = file.path(admin$sp_data, "infographs", .x)
      , to = file.path("app/data/infogs", .x)
      , overwrite = TRUE
    )
  )
  
  
  admin$mylog("!!END   PREP FOR SHINY - PNG")
  
}


#' Combine data + pngs prep 
#' total time: ~25 min
#'
#' @return
#' @export
#'
#' @examples
prep_for_shiny <- function(){
  
  prep_for_shiny_DATA() 
  prep_for_shiny_PNG()
}

