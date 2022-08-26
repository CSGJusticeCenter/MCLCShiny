

box::use(
    csgjcr[csg_sp_path]
  , readxl[read_excel]
  , readr[read_csv]
  , ./admin
)


CC_path <- csg_sp_path("CC/DATA/Raw")



#' Import NCRP data frames: A: admissions, R: releases, N: year-end 
#'
#' @return list of 3 data frames on individual level 
#' @export
NCRP_ARN <- function(){
  
  admin$mylog("Start NCRP import, 3 data sets")
  
  #da38048.0002 -- prison admissions
  load(file.path(CC_path,"NCRP/ICPSR_38048-V1/ICPSR_38048", "DS0002/38048-0002-Data.rda"))
  
  #da38048.0003 -- prison releases 
  load(file.path(CC_path,"NCRP/ICPSR_38048-V1/ICPSR_38048", "DS0003/38048-0003-Data.rda"))
  
  #da38048.0004 -- yearend population 
  load(file.path(CC_path,"NCRP/ICPSR_38048-V1/ICPSR_38048", "DS0004/38048-0004-Data.rda"))
  
  raw <- list(
      "ADMISSIONS" = da38048.0002
    , "RELEASES"   = da38048.0003
    , "N_YEAREND"  = da38048.0004
  )

  admin$mylog("End   NCRP import, 3 data sets")
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
  
  admin$mylog("End   NCRP import for admissions")
  return(raw)
} 



#' State Characteristics Import 
#'
#' @return
#' @export
#'
#' @examples
SC <- function(){
  
  raw <- read_csv(file.path(CC_path, "SC-EST/SC-EST2020-ALLDATA6.csv"), show_col_types = FALSE)
  
  return(raw)
  
}


