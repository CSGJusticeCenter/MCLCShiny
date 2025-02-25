
box::use(
    ./ROOT
  , glue[glue]
  , dplyr[...]
  , tidyr[pivot_longer]
  , scales[comma]
)


#' @export
sp_survey <- csgjcr::csg_sp_path(file.path(ROOT$sp, "50 State Survey (2022)"))

#' @export
sp_data <- csgjcr::csg_sp_path(file.path(ROOT$sp, "MCLC Shiny App/data/analysis"))

#' @export
sp_data_del <- csgjcr::csg_sp_path(file.path(ROOT$sp, "MCLC Shiny App/data/deliverables"))

#' @export
sp_data_raw <- csgjcr::csg_sp_path(file.path(ROOT$sp, "MCLC Shiny App/data/raw"))

#' Log Message
#'
#' @param text string
#'
#' @return log message
#' @export
mylog <- function(text){
  #log_info(text) #logger outputs DO NOT show in knitted RMD
  message(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "-- ", text)) #use this if want to show in knitted rmd
}



#' Save RDS on SP, overwrite and date stamp 
#'
#' @param SP_PATH
#' @param IN
#' @param OUT
#'
#' @return
#' @export
#'
#' @examples
SPsaveRDS <- function(IN, OUT){

  SP_PATH <- sp_data

  datestamp <- gsub("-", "", Sys.Date())
  outfile1 <- file.path(SP_PATH, "datestamp", paste0(datestamp, "_", OUT))
  outfile2 <- file.path(SP_PATH,                        OUT )

  saveRDS(IN, file=outfile1)
  saveRDS(IN, file=outfile2)
  mylog(glue("Saved {deparse(substitute(IN))} - {OUT} (included a date stamped version)"))


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