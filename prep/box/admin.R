
box::use(
    ./ROOT
  , glue[glue]
  , dplyr[...]
  , tidyr[pivot_longer]
)


sp_survey <- csgjcr::csg_sp_path(file.path(ROOT$sp))

#' @export
sp_survey <- gsub("/MCLC Shiny App", "/50 State Survey (2022)", sp_survey)

#' @export
sp_data <- csgjcr::csg_sp_path(file.path(ROOT$sp, "data/analysis"))

#' @export
sp_data_del <- csgjcr::csg_sp_path(file.path(ROOT$sp, "data/deliverables"))

#' @export
sp_data_raw <- csgjcr::csg_sp_path(file.path(ROOT$sp, "data/raw"))

#' Log Message
#'
#' @param text string
#'
#' @return log message
#' @export
mylog <- function(text){
  #log_info(text) #logger outputs DO NOT show in knitted RMD
  message(paste0(Sys.time(), " - ", glue(text))) #use this if want to show in knitted rmd
}

#' @export
idcols <- c("STATE", "FIPS", "ABB")

#' @export
groupcols <- c("STATE", "FIPS", "ABB", "RPTYEAR")


#' Variable Cross Section
#'
#' @param cs
#' @export
varcs <- function(cs, combinelst = FALSE, includeYR = TRUE){

  if (includeYR == TRUE){
    basevar <- groupcols
  } else {
    basevar <- idcols
  }

  list <- case_when(
      cs == "t"  ~ list(c(basevar), NA)
    , cs == "OR" ~ list(c(basevar), c("OFFGENERAL", "RACE"))
    , cs == "R"  ~ list(c(basevar), c("RACE"))
    , cs == "O"  ~ list(c(basevar), c("OFFGENERAL"))
  )

  if (combinelst == TRUE){
    out <- c(list[[1]], list[[2]])
    out <- out[!is.na(out)]
  } else {
    out <- list
  }

  return(out)

}


#' @export
lev_RACE <- c(
    "White"    # "(1) White, non-Hispanic"
  , "Black"    # "(2) Black, non-Hispanic"
  , "Hispanic" # "(3) Hispanic, any race"
  , "Other"    # "(4) Other race(s), non-Hispanic"
  , "missing"
)

#' @export
fctnum_white <- 1

#' @export
lev_OFFGENERAL <- c(
    "Violent"      # "(1) Violent"
  , "Property"     # "(2) Property"
  , "Drugs"        # "(3) Drugs"
  , "Public order" # "(4) Public order"
  , "Other"        # "(5) Other/unspecified"
  , "missing"
)


#' @export
lev_OFFGENERAL2 <- c(
    "All categories"
  , "Violent"      # "(1) Violent"
  , "Property"     # "(2) Property"
  , "Drugs"        # "(3) Drugs"
  , "Public order" # "(4) Public order"
  , "Other"        # "(5) Other/unspecified"
  , "missing"
)


#' NCRP levels with New levels as names
#' @export
NCRPlev_RACE <- function(){

  org_lev<-  c(
      "(1) White, non-Hispanic"
    , "(2) Black, non-Hispanic"
    , "(3) Hispanic, any race"
    , "(4) Other race(s), non-Hispanic"
  )

  #add new levels as names
  names(org_lev) <- lev_RACE[1:4]

  return(org_lev)

}



#' @export
lev_ADMTYPE <- c(
    "New"        #"(1) New court commitment"
  , "Revocation" #"(2) Parole return/revocation"
  , "Other"      #"(3) Other admission (including unsentenced, transfer, AWOL/escapee return)"
  , "missing"
)


#' NCRP levels with New levels as names
#' @export
NCRPlev_ADMTYPE <- function(){

  org_lev<-  c(
    c(
        "(1) New court commitment"
      , "(2) Parole return/revocation"
      , "(3) Other admission (including unsentenced, transfer, AWOL/escapee return)"
    )

  )

  #add new levels as names
  names(org_lev) <- lev_ADMTYPE[1:3]

  return(org_lev)

}


#' Convert SC RACE/ORIGIN --> NCPR RACE
#' @export
SC_RE <- function(DF){

  DF |>
    mutate(
      NCRP_RACE = case_when(
         ORIGIN == 1 & RACE == 1        ~ lev_RACE[1]
       , ORIGIN == 1 & RACE == 2        ~ lev_RACE[2]
       , ORIGIN == 1 & RACE %in% c(3:6) ~ lev_RACE[4]
       , ORIGIN == 2 & RACE %in% c(1:6) ~ lev_RACE[3]
        )
    ) |>
    #remove old RACE/ETHNICITY columns
    select(-c(ORIGIN, RACE)) |>
    #rename new column to 'RACE'
    rename(RACE = NCRP_RACE)

}



#' Convert APS race variables --> NCRP RACE
#' @export
APS_RE <- function(DF){

  DF |>
    ## calculate other race category
    rowwise() %>%
    mutate(
      OTHER = ifelse(
          UNKRACE == TOTRACE
        , NA
        , TOTRACE - sum(c(WHITE, BLACK, HISP, UNKRACE), na.rm = TRUE)
      )
    ) %>%
    ungroup() %>%
    #drop TOTRACE variable
    select(-TOTRACE) %>%
    #pivot longer: multiple race variables into 1 race variable
    pivot_longer(cols = c(WHITE, BLACK, HISP, UNKRACE, OTHER), names_to = "APS_RACE", values_to = "POPEST") %>%
    #recode race to match NCRP
    mutate(
      RACE = case_when(
          APS_RACE == "WHITE"   ~ lev_RACE[1]
        , APS_RACE == "BLACK"   ~ lev_RACE[2]
        , APS_RACE == "HISP"    ~ lev_RACE[3]
        , APS_RACE == "OTHER"   ~ lev_RACE[4]
        , APS_RACE == "UNKRACE" ~ lev_RACE[5]
      )
    ) %>%
    #remove old race variable
    select(-APS_RACE)

}


#' NCRP levels with New levels as names
#' @export
NCRPlev_OFFGENERAL <- function(){

  org_lev<-  c(
      "(1) Violent"
    , "(2) Property"
    , "(3) Drugs"
    , "(4) Public order"
    , "(5) Other/unspecified"
  )

  #add new levels as names
  names(org_lev) <- lev_OFFGENERAL[1:5]

  return(org_lev)

}



#' Quick check that a value includes 'white', used for calc rel rate
#'
#' @param VAL
#' @export
isWhite <- function(VAL){

  #see if the word 'WHITE' is detected in value
  #DONE in case cateogry label changes form White to White, non-hispanic or White, alone
  isWhite <- stringr::str_detect(toupper(VAL), "WHITE")
  error_message <- "RACE category is NOT WHITE, please check"
  if(isWhite == FALSE) stop(error_message)

}



#' Save RDS on SP, overwrite and datestamp
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
