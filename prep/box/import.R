

box::use(
    csgjcr[csg_sp_path]
  , readxl[read_excel]
  , readr[read_csv]
  , glue[glue]
  , dplyr[...]
  , ./admin
)


CC_path <- csg_sp_path("CC/data/raw")



#' Import NCRP data frames: A: admissions, R: releases, N: year-end 
#'
#' @return list of 3 data frames on individual level 
#' @export
NCRP_ARN <- function(){
  
  admin$mylog("Start NCRP import, 3 data sets")
  
  #da38048.0002 -- prison admissions
  load(file.path(CC_path,"NCRP/ICPSR_38048-V1/ICPSR_38048", "DS0002/38048-0002-Data.rda"))
  admin$mylog("Complete NCRP import: admissions")
  
  #da38048.0003 -- prison releases 
  load(file.path(CC_path,"NCRP/ICPSR_38048-V1/ICPSR_38048", "DS0003/38048-0003-Data.rda"))
  admin$mylog("Complete NCRP import: releases")
  
  #da38048.0004 -- yearend population 
  load(file.path(CC_path,"NCRP/ICPSR_38048-V1/ICPSR_38048", "DS0004/38048-0004-Data.rda"))
  admin$mylog("Complete NCRP import: population")
  
  raw <- list(
      "ADMISSIONS" = da38048.0002
    , "RELEASES"   = da38048.0003
    , "N_YEAREND"  = da38048.0004
  )

  admin$mylog("Complete NCRP import, 3 data sets")
  return(raw)
} 


#' Import NCRP data frame for A: Admissions 
#'
#' @return individual level DF of admissions 
#' @export
NCRP_A <- function(){
  
  admin$mylog("Start NCRP import for admissions")
  
  #da38048.0002 -- prison admissions
  load(file.path(CC_path,"NCRP/ICPSR_38048-V1/ICPSR_38048", "DS0002/38048-0002-Data.rda"))
  
  raw <- da38048.0002
  
  admin$mylog("Complete NCRP import for admissions")
  return(raw)
} 



#' State Characteristics Import 
#'
#' @return
#' @export
#'
#' @examples
SC <- function(){
  
  raw <- read_csv(file.path(CC_path, "SC-EST/SC-EST2020-ALLDATA6.csv"), show_col_types = FALSE) %>% 
    mutate(POPTYPE = "SC")
  admin$mylog("Complete SC import")
  
  return(raw)
  
}




load_combine_APSindyrs <- function(folder, startyr = NA, endyr = NA){
  
  
  metadata <- read_csv(file.path(CC_path, glue("{folder}/{tolower(folder)}_idyear.csv")), show_col_types = FALSE)
  minyr <- ifelse(is.na(startyr), min(metadata$RPTYEAR), startyr) 
  maxyr <- ifelse(is.na(endyr)  , max(metadata$RPTYEAR), endyr)
  thisdata <- metadata[metadata$RPTYEAR >= minyr & metadata$RPTYEAR <= maxyr, ]
  

  ## load files into R 
  purrr::walk( thisdata$rda_pathway, ~load(file.path(CC_path, folder, .x), .GlobalEnv) )
  
  ## add rptyear and folder (type) to indv year data sets, bind year data sets together
  # need to switch from numeric -> factor, probation (2013, 2015-2018)
  rawcombined <- purrr::map2(
      thisdata$rda_name[c(1:5)]
    , thisdata$RPTYEAR[c(1:5)]
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
  rm(list = thisdata$rda_name, envir = .GlobalEnv)
  
  admin$mylog(glue("Complete {folder} import for {minyr}-{maxyr}"))
  
  
  return(rawcombined)
  
}


#' Annual Parole/Probation Survey  
#'
#' @return
#' @export
#'
#' @examples
APS <- function(type = "both", startyr = NA, endyr = NA){
  
  
  if (tolower(type) == "both"){
    parole <- load_combine_APSindyrs("AParS",  startyr, endyr)
    probat <- load_combine_APSindyrs("AProbS", startyr, endyr)
    OUT <- list(
        "parole" = parole
      , "probat" = probat
    )
  } else if (tolower(type) == "parole"){
    parole <- load_combine_APSindyrs("AParS",  startyr, endyr)
    OUT <- parole
  } else if (tolower(type) %in% c("probat", "probation")){
    probat <- load_combine_APSindyrs("AProbS",  startyr, endyr)
    OUT <- probat
  } else { 
    stop("Issue with type specified")
  }
  
  return(OUT)
  

}
