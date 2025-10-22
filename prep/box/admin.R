
box::use(
    ./ROOT
  , glue[glue]
  , dplyr[...]
  , scales[comma]
)


#' @export
sp_survey <- csgjcr::csg_sp_path(file.path(ROOT$sp, "50 State Survey (2024)"))

#' @export
sp_data <- csgjcr::csg_sp_path(file.path(ROOT$sp, "MCLC Shiny App/data/analysis"))

#' @export
sp_data_del <- csgjcr::csg_sp_path(file.path(ROOT$sp, "MCLC Shiny App/data/deliverables"))

#' @export
sp_data_raw <- csgjcr::csg_sp_path(file.path(ROOT$sp, "MCLC Shiny App/data/raw"))

#' Log Message
#' \code{log_info(text)},logger outputs, DO NOT show in knitted RMD
#'
#' @param text string
#'
#' @return log message
#' @export
mylog <- function(text){
  message(text) 
}


#' Save RDS within repo and on sharepoint 
#'
#' @param obj object in r to save as rds; will be saved with obj name 
#'
#' @export
save_rds_twice <- function(obj, save_to_sp = TRUE, save_to_repo = TRUE){
  
  obj_name <- deparse(substitute(obj))
  
  if (save_to_sp == TRUE){
    # save on sharepoint 
    sp_path <- file.path(sp_data, paste0(obj_name, ".rds"))
    saveRDS(obj, sp_path)
    mylog(paste("export", obj_name, "to:", sp_path))
  }
  
  if (save_to_repo == TRUE){
    # save in repo 
    repo_path <- file.path("app/data", paste0(obj_name, ".rds"))
    saveRDS(obj, repo_path)
    mylog(paste("export", obj_name, "to:", repo_path))
  }
  
  
}


#' Rounded Value 
#'
#' @param val 
#' @param accuracy 
#'
#' @return
#' @export
#'
#' @examples
roundedval <- function(val, accuracy){
  
  ndigits <- -log(accuracy, base = 10)
  
  ifelse(
    val < accuracy & round(val, digits = ndigits) == 0 & val > 0 
    , paste0("<", accuracy)
    , comma(val, accuracy = accuracy)
  )
  
  
}


#' Add asterisk 
#'
#' @param char 
#'
#' @return
#' @export
#'
#' @examples
addsuppressasterick <- function(char){
  
  ifelse(
    substr(char, 1, 1) == "<"
    , paste0(char, "*")
    , paste0("<", char, "*")
  )
  
}