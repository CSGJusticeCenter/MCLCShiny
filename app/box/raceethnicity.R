

box::use(
    reactable[...]
  , dplyr[...]
  , glue[glue]
)


#' Create table that will be shown in shiny app
#'
#' @param RRIDATA
#' @param whichPOP
#' @param whichSTATE
#' @param whichVAL
#' @param whichTABLE
#'
#' @return
#' @export
#'
#' @examples
create_tabledf <- function(RRIDATA, whichNCRP, whichPOP, whichSTATE, whichVAL, whichTABLE = "table_suppress"){


  table <- RRIDATA[[whichNCRP]][[whichPOP]][[whichSTATE]][[whichVAL]][[whichTABLE]]
  yrs <- colnames(table)[-c(1:2)]

  df <- table %>%
    mutate(
        OFFGENERALB = OFFGENERAL
      , OFFGENERAL  = as.character(OFFGENERAL)
      , OFFGENERAL  = ifelse( OFFGENERAL == lag(OFFGENERAL) & !is.na(lag(OFFGENERAL)), "", OFFGENERAL)
    )


  nodata <- df %>% filter(if_all(all_of(yrs), ~ is.na(.)))

  if (nrow(df) == nrow(nodata)){
    #all data missing
    out <-df[0, c("OFFGENERAL", "RACE", "OFFGENERALB")]
  }

  if (nrow(df) != nrow(nodata)){
    #some or none data missing
    out <-df
  }



  return(out)

}


# TESTING
# RRIDATA <- readRDS("app/data/NCRP_RRI_tables.RDS")
# whichPOP <- "BJS"
# whichSTATE <- "Arizona"
# whichVAL <- "RRI"
# whichTABLE <- "table_asis"
# DF <- create_tabledf(RRIDATA, whichPOP, whichSTATE, whichVAL, whichTABLE)


#' Create reactable tables for Shiny
#'
#' @param DF
#'
#' @return
#' @export
#'
#' @examples
create_reactable <- function(DF){


  reactable(
    DF
    , style = list(fontFamily = "Graphik, sans-serfit", fontsize = "1.4rem")
    , theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"))
    , defaultColDef = colDef(
        minWidth = 55
      , align = "right"
      , headerVAlign = "bottom"
    )
    , compact = TRUE
    , fullWidth = TRUE
    , searchable = FALSE
    , pagination = FALSE
    , sortable = FALSE
    , columns = list(
        OFFGENERAL = colDef(
            name = "Offense Category"
          , align = "left"
          , minWidth = 105
          #no border when witin offense category
          , style = JS(
            "function(rowInfo) {
              var value = rowInfo.row['OFFGENERAL']
              if (value == '') {
                var borderTop = 'none'
              }
              return {borderTop: borderTop}
              }")
        )
      , RACE       = colDef(name = "Race and Ethnicity", align = "left", minWidth = 105)
      , OFFGENERALB = colDef(show = FALSE)

    ) #end columns list
    , rowStyle = function(index) {
           if ( DF$OFFGENERALB[index] == "All categories"){list(`background`="rgba(0,0,0,0.05)")} #, fontWeight="bold"
      else if ( DF$OFFGENERAL [index] != "" & index != 1 ){list(`border-top`   = "1.25px solid rgba(0,0,0,0.10)") }
      else if (index == nrow(DF)                         ){list(`border-bottom`= "1.75px solid rgba(0,0,0,0.10)") }
      else    { }
    } #end function of index

  ) #end reactable




}


SUBHEAD_TEXT <- function(pop_denom, pop_or_adm_data){

  case_when(
      pop_denom == "BJS" & pop_or_adm_data == "Admissions" ~ "racial and ethnic disparities in prison admissions for parole revocations represent the accumulation of disparities across the system. Only a portion of these disparities can be attributed to parole revocations"
    , pop_denom == "CEN" & pop_or_adm_data == "Admissions" ~ "racial and ethnic disparities in prison admissions for parole revocations represent the accumulation of disparities across the system"
    , pop_denom == "BJS" & pop_or_adm_data == "Population" ~ "racial and ethnic disparities in populations incarcerated for parole revocations represent the accumulation of disparities across the system. Only a portion of these disparities can be attributed to parole revocations"
    , pop_denom == "CEN" & pop_or_adm_data == "Population" ~ "racial and ethnic disparities in populations incarcerated for parole revocations represent the accumulation of disparities across the system"
  )

}



#' Text to display the pop_denom
#'
#' @param pop_denom
#' @param pop_or_adm_data
#'
#' @return
#' @export
#'
#' @examples
pop_denom_text <- function(pop_denom, pop_or_adm_data){

  rlang::arg_match(pop_denom, c("BJS", "CEN"))
  rlang::arg_match(pop_or_adm_data, c("Admissions", "Population"))

  # outtext <- paste0(
  #     "<div class = 'resubtitle'>"
  #    , "To highlight "
  #    , SUBHEAD_TEXT(pop_denom, pop_or_adm_data)
  #    , ", rates are shown relative to White individuals."
  #    , "</div>"
  # )
  outtext <- ""

}



WHITE_INTRO_TEXT <- function(pop_denom, pop_or_adm_data){

  case_when(
      pop_denom == "BJS" & pop_or_adm_data == "Admissions" ~ "For every White person who is readmitted to prison from parole"
    , pop_denom == "CEN" & pop_or_adm_data == "Admissions" ~ "For every White person in the community who is readmitted to prison from parole"
    , pop_denom == "BJS" & pop_or_adm_data == "Population" ~ "For every White person who is incarcerated after being readmitted to prison from parole"
    , pop_denom == "CEN" & pop_or_adm_data == "Population" ~ "For every White person in the community who is incarcerated after being readmitted to prison from parole"
  )
}



#' Text to display the pop_denom
#'
#' @param dataavail
#' @param note
#'
#' @return
#' @export
#'
#' @examples
infographic_header <- function(dataavail, pop_denom, pop_or_adm_data, note){

  char <- as.character(dataavail)
  rlang::arg_match(char, c("1", "0"))


  if (dataavail == 1){

    fill <- WHITE_INTRO_TEXT(pop_denom, pop_or_adm_data)

    # outtext <- paste0("<h3 class = 'reh3'>", fill,"...", "</h3>")
    outtext <- ""
  }

  if (dataavail == 0){
    outtext <- paste0("<h3 class='nodata'>No Data</h3>"
                      , "<div class = 'notetxt'>"
                      , "Data to calculate disparities in readmissions to prison from parole are not available for this state.<br>"
                      , note
                      , "</div>"
                     )
  }

  return(outtext)

}


PAROLE_TEXT <- function(pop_denom){
  
  case_when(
      pop_denom == "BJS" ~ "people on parole are"
    , pop_denom == "CEN" ~ "people are"
  )
  
}

INFOGRAPH_RE_TEXT <- function(whichNCRP){

  case_when(
      whichNCRP == "Admissions" ~ "people are readmitted to prison from parole"
    , whichNCRP == "Population" ~ "people are in prison after being readmitted from parole"
  )

}


#' Create alt text for infographic
#'
#' @param RRIDATA
#' @param whichNCRP
#' @param whichPOP
#' @param whichRE
#' @param whichSTATE
#'
#' @export
infograph_alt <- function(RRIDATA, whichNCRP, whichPOP, whichRE, whichSTATE){


  thisdata <- RRIDATA[[whichNCRP]][[whichPOP]][[whichSTATE]]$INFOGRAPH$DF |> filter(RACE == whichRE)

  suppress <- thisdata$SUPPRESS
  rri_val <- thisdata$S_RRI

  display_value <- case_when(
      round(rri_val, 1) == 0 & rri_val > 0   ~ paste0(" less than 0.1")
    , round(rri_val, 1) >  0 & suppress == 1 ~ paste0(" less than ",  sprintf(glue("%.{1}f"), round(rri_val, 1)))
    , round(rri_val, 1) >  0 & suppress == 0 ~ paste0(" ",            sprintf(glue("%.{1}f"), round(rri_val, 1)))
  )
  
  VALUE_TEXT <- function(suppress,display_value,whichNCRP){
    
    case_when(
      suppress == 0 & as.numeric(display_value) > 1  & whichNCRP == "Admissions" ~ paste0(display_value," times more likely to be admitted to prison for a parole revocation than White people.")
      , suppress == 0 & as.numeric(display_value) > 1  & whichNCRP == "Population" ~ paste0(display_value," times more likely to be incarcerated for a parole revocation than White people.")
      , suppress == 0 & as.numeric(display_value) < 1  & whichNCRP == "Admissions" ~ paste0((1-as.numeric(display_value))*100,"% less likely to be admitted to prison for a parole revocation than White people.")
      , suppress == 0 & as.numeric(display_value) < 1  & whichNCRP == "Population" ~ paste0((1-as.numeric(display_value))*100,"% less likely to be incarcerated for a parole revocation than White people.")
      , suppress == 0 & as.numeric(display_value) == 1 & whichNCRP == "Admissions" ~ "equally likely to be admitted to prison for a parole revocation as White people."
      , suppress == 0 & as.numeric(display_value) == 1 & whichNCRP == "Population" ~ "equally likely to be incarcerated for a parole revocation as White people."
    )
    
  }

  suppress_suf <- ifelse(suppress == 0, "", " This estimate should be interpreted with caution because one racial or ethnic group included in its calculation had fewer than 5 people. See data tables below for details.")

  string_vec <- c(
    "The info-graphic for "
    , whichSTATE
    , " highlights "
    , SUBHEAD_TEXT(whichPOP, whichNCRP)
    , ". "
    , whichRE, " "
    , PAROLE_TEXT(pop_denom), " "
    , VALUE_TEXT(suppress,display_value,whichNCRP)
    , suppress_suf, "."
    
    #, WHITE_INTRO_TEXT(whichPOP, whichNCRP)
    #, display_value, " "
    #, whichRE, " "
    #, INFOGRAPH_RE_TEXT(whichNCRP), "."
    
  )


  alt_text <- paste0(string_vec[!is.na(string_vec)], collapse = "")

  return(alt_text)

}



#' Create alt text when there isn't an infographic
#'
#' @param whichNCRP
#' @param whichPOP
#' @param whichRE
#' @param whichSTATE
#'
#' @export
infograph_alt_noinfog <- function( whichNCRP, whichPOP, whichRE, whichSTATE){


  # [state] does not have enough data to show [text] for [Black/Hispanic]
  # individuals revoked.

  thistxt <- case_when(
      whichPOP == "BJS" & whichNCRP == "Admissions" ~ "racial and ethnic disparities in prison admissions for parole revocations representing the accumulation of disparities across the system. Only a portion of these disparities can be attributed to parole revocations"
    , whichPOP == "CEN" & whichNCRP == "Admissions" ~ "racial and ethnic disparities in prison admissions for parole revocations representing the accumulation of disparities across the system"
    , whichPOP == "BJS" & whichNCRP == "Population" ~ "racial and ethnic disparities in populations incarcerated for parole revocations representing the accumulation of disparities across the system. Only a portion of these disparities can be attributed to parole revocations"
    , whichPOP == "CEN" & whichNCRP == "Population" ~ "racial and ethnic disparities in populations incarcerated for parole revocations representing the accumulation of disparities across the system"
  )
  
  thistxt2 <- case_when(
        pop_denom == "BJS" ~ " on parole."
      , pop_denom == "CEN" ~ "."
    )


  string_vec <- c(
      whichSTATE
    , "does not have enough data to show"
    , thistxt
    , "for"
    , whichRE
    , "people"
    , thistxt2
  )


  alt_text <- paste(string_vec[!is.na(string_vec)], collapse = " ")

  return(alt_text)

}


#' Text to display the pop_denom
#'
#' @param pop_denom
#' @param pop_or_adm_data
#'
#' @return
#' @export
#'
#' @examples
rate_table_header <- function(pop_denom, pop_or_adm_data, mult){

  rlang::arg_match(pop_denom, c("BJS", "CEN"))
  rlang::arg_match(pop_or_adm_data, c("Admissions", "Population"))

  mult_txt <- scales::comma(mult)



  out <- case_when(
      pop_denom == "BJS" & pop_or_adm_data == "Admissions" ~ paste("Admissions for Parole Revocations per"    , mult_txt, "Individuals Serving Parole Sentences")
    , pop_denom == "CEN" & pop_or_adm_data == "Admissions" ~ paste("Admissions for Parole Revocations per"    , mult_txt, "Individuals from the Community")
    , pop_denom == "BJS" & pop_or_adm_data == "Population" ~ paste("Number of People Incarcerated for Parole Revocations per", mult_txt, "Individuals Serving Parole Sentences")
    , pop_denom == "CEN" & pop_or_adm_data == "Population" ~ paste("Number of People Incarcerated for Parole Revocations per", mult_txt, "Individuals from the Community")
  )


  paste0("<h4 class='reh4'>", out, "</h4>")


}

