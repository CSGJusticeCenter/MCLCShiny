

box::use(
    csgjcr[csg_sp_path]
  , readxl[read_excel]
  , readr[read_csv]
  , glue[glue]
  , dplyr[...]
  , ./admin
)

# ME
datalib <- csg_sp_path("JR_DATA_LIBRARY/data/raw")



#' Import NCRP data frames: A: admissions, R: releases, N: year-end
#'
#' @return list of 3 data frames on individual level
#' @export
NCRP_ARN <- function(){

  admin$mylog("Start NCRP import, 3 data sets")

  #da38048.0002 -- prison admissions
  load(file.path(datalib,"bjs/ncrp/ICPSR_38492-V1/ICPSR_38492", "DS0002/38492-0002-Data.rda"))
  admin$mylog("Complete NCRP import: admissions")

  #da38048.0003 -- prison releases
  load(file.path(datalib,"bjs/ncrp/ICPSR_38492-V1/ICPSR_38492", "DS0003/38492-0003-Data.rda"))
  admin$mylog("Complete NCRP import: releases")

  #da38048.0004 -- yearend population
  load(file.path(datalib,"bjs/ncrp/ICPSR_38492-V1/ICPSR_38492", "DS0004/38492-0004-Data.rda"))
  admin$mylog("Complete NCRP import: population")

  raw <- list(
      "ADMISSIONS" = da38492.0002
    , "RELEASES"   = da38492.0003
    , "N_YEAREND"  = da38492.0004
  )

  admin$mylog("Complete NCRP import, 3 data sets")
  return(raw)
}






#' Import NCRP data frame for A: Admissions
#'
#' @return individual level DF of admissions
#' @export
NCRP_A <- function(){

  admin$mylog("Start - NCRP import for admissions")

  #da38492.0002 -- prison admissions
  load(file.path(datalib,"bjs/ncrp/ICPSR_38492-V1/ICPSR_38492", "DS0002/38492-0002-Data.rda"))

  raw <- da38492.0002

  admin$mylog("End   - NCRP import for admissions")
  return(raw)
}



#' Import NCRP data frame for N: year-end population
#'
#' @return individual level DF of population
#' @export
NCRP_N <- function(){

  admin$mylog("Start - NCRP import for year-end population ")

  #da38492.0004 -- year end population
  load(file.path(datalib,"bjs/ncrp/ICPSR_38492-V1/ICPSR_38492", "DS0004/38492-0004-Data.rda"))

  raw <- da38492.0004

  admin$mylog("End   - NCRP import for year-end population")
  return(raw)
}


#' State Characteristics Import
#'
#' @return
#' @export
#'
#' @examples
SC <- function(){

  admin$mylog("Start - SC import")
  raw <- read_csv(file.path(datalib, "census/SC-EST/SC-EST2020-ALLDATA6.csv"), show_col_types = FALSE) %>%
    mutate(POPTYPE = "SC")
  admin$mylog("End   - SC import")

  return(raw)

}




#' Annual Parole Survey
#'
#' @param startyr
#' @param endyr
#'
#' @return
#' @export
#'
#' @examples
APS <- function(startyr = NA, endyr = NA){


  folder <- "apars"
  metadata <- read_csv(file.path(datalib, glue("bjs/{folder}/{tolower(folder)}_idyear.csv")), show_col_types = FALSE)
  minyr <- ifelse(is.na(startyr), min(metadata$year), startyr)
  maxyr <- ifelse(is.na(endyr)  , max(metadata$year), endyr)
  thisdata <- metadata[metadata$year >= minyr & metadata$year <= maxyr, ]
  
  
  rda_pathway <- function(icpsr){
    paste0("ICPSR_", icpsr, "-V1/ICPSR_", icpsr,"/DS0001/", icpsr, "-0001-Data.rda")
  }
  
  rda_name <- function(icpsr){
    paste0("da", icpsr, ".0001")
  }


  admin$mylog(glue("Start - BJS APS (Parole) import for {minyr}-{maxyr}"))

  ## load files into R
  purrr::walk( thisdata$icpsr_id, ~load(file.path(datalib, "bjs", folder, rda_pathway(.x)), .GlobalEnv) )

  ## add rptyear and folder (type) to indv year data sets, bind year data sets together
  # need to switch from numeric -> factor, probation (2013, 2015-2018)
  rawcombined <- purrr::map2(
      rda_name(thisdata$icpsr_id[c(1:5)])
    , thisdata$year[c(1:5)]
    , ~mget(.x, envir = .GlobalEnv)[[1]] %>%
          mutate(
              POPTYPE = folder
            , RPTYEAR = .y
            , .before = 1
          ) %>%
          mutate_at(
            vars(ESGPSNUM, ESGPSSXNUM)
            , ~factor(., levels = c(
                  "(1) Estimated"
                , "(2) Actual"
                , "(3) Both estimated and actual from reporting entities"
                , "(4) BJS imputed data"))
          )
    ) %>% bind_rows() %>% as_tibble()

  ## remove originally loaded data frames (indv data frames by year)
  rm(list = rda_name(thisdata$icpsr_id), envir = .GlobalEnv)

  admin$mylog(glue("End   - BJS APS (Parole) import for {minyr}-{maxyr}"))


  return(rawcombined)

}








