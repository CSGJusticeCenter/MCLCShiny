

box::use(
    tibble[tribble]
  , dplyr[case_when]
  , glue[glue]
)

## df of names, fips, abbreviations 

statedf <- tribble(
  ~abb_usps,~abb_gpo,                      ~name, ~fips, ~eta_region, ~type
  , "AL", "Ala."   , "Alabama"                 ,  "01",           3, "state"
  , "AK", "Alaska" , "Alaska"                  ,  "02",           6, "state"
  , "AZ", "Ariz."  , "Arizona"                 ,  "04",           6, "state"
  , "AR", "Ark."   , "Arkansas"                ,  "05",           4, "state"
  , "CA", "Calif." , "California"              ,  "06",           6, "state"
  , "CO", "Colo."  , "Colorado"                ,  "08",           4, "state"
  , "CT", "Conn."  , "Connecticut"             ,  "09",           1, "state"
  , "DE", "Del."   , "Delaware"                ,  "10",           2, "state"
  , "DC", "D.C."   , "District of Columbia"    ,  "11",           2, "federal district"
  , "FL", "Fla."   , "Florida"                 ,  "12",           3, "state"
  , "GA", "Ga."    , "Georgia"                 ,  "13",           3, "state"
  , "HI", "Hawaii" , "Hawaii"                  ,  "15",           6, "state"
  , "ID", "Idaho"  , "Idaho"                   ,  "16",           6, "state"
  , "IL", "Ill."   , "Illinois"                ,  "17",           5, "state"
  , "IN", "Ind."   , "Indiana"                 ,  "18",           5, "state"
  , "IA", "Iowa"   , "Iowa"                    ,  "19",           5, "state"
  , "KS", "Kans."  , "Kansas"                  ,  "20",           5, "state"
  , "KY", "Ky."    , "Kentucky"                ,  "21",           3, "state"
  , "LA", "La."    , "Louisiana"               ,  "22",           4, "state"
  , "ME", "Maine"  , "Maine"                   ,  "23",           1, "state"
  , "MD", "Md."    , "Maryland"                ,  "24",           2, "state"
  , "MA", "Mass."  , "Massachusetts"           ,  "25",           1, "state"
  , "MI", "Mich."  , "Michigan"                ,  "26",           5, "state"
  , "MN", "Minn."  , "Minnesota"               ,  "27",           5, "state"
  , "MS", "Miss."  , "Mississippi"             ,  "28",           3, "state"
  , "MO", "Mo."    , "Missouri"                ,  "29",           5, "state"
  , "MT", "Mont."  , "Montana"                 ,  "30",           4, "state"
  , "NE", "Nebr."  , "Nebraska"                ,  "31",           5, "state"
  , "NV", "Nev."   , "Nevada"                  ,  "32",           6, "state"
  , "NH", "N.H."   , "New Hampshire"           ,  "33",           1, "state"
  , "NJ", "N.J."   , "New Jersey"              ,  "34",           1, "state"
  , "NM", "N. Mex.", "New Mexico"              ,  "35",           4, "state"
  , "NY", "N.Y."   , "New York"                ,  "36",           1, "state"
  , "NC", "N.C."   , "North Carolina"          ,  "37",           3, "state"
  , "ND", "N. Dak.", "North Dakota"            ,  "38",           4, "state"
  , "OH", "Ohio"   , "Ohio"                    ,  "39",           5, "state"
  , "OK", "Okla."  , "Oklahoma"                ,  "40",           4, "state"
  , "OR", "Oreg."  , "Oregon"                  ,  "41",           6, "state"
  , "PA", "Pa."    , "Pennsylvania"            ,  "42",           2, "state"
  , "RI", "R.I."   , "Rhode Island"            ,  "44",           1, "state"
  , "SC", "S.C."   , "South Carolina"          ,  "45",           3, "state"
  , "SD", "S. Dak.", "South Dakota"            ,  "46",           4, "state"
  , "TN", "Tenn."  , "Tennessee"               ,  "47",           3, "state"
  , "TX", "Tex."   , "Texas"                   ,  "48",           4, "state"
  , "UT", "Utah"   , "Utah"                    ,  "49",           4, "state"
  , "VT", "Vt."    , "Vermont"                 ,  "50",           1, "state"
  , "VA", "Va."    , "Virginia"                ,  "51",           2, "state"
  , "WA", "Wash."  , "Washington"              ,  "53",           6, "state"
  , "WV", "W. Va." , "West Virginia"           ,  "54",           2, "state"
  , "WI", "Wis."   , "Wisconsin"               ,  "55",           5, "state"
  , "WY", "Wyo."   , "Wyoming"                 ,  "56",           4, "state"
  , "AS", "A.S."   , "American Samoa"          ,  "60",           6, "outlying area" #under US sovereignty"
  , "GU", "Guam"   , "Guam"                    ,  "66",           6, "outlying area" #under US sovereignty"
  , "MP", "M.P."   , "Northern Mariana Islands",  "69",           6, "outlying area" #under US sovereignty"
  , "PR", "P.R."   , "Puerto Rico"             ,  "72",           1, "outlying area" #under US sovereignty"
  , "VI", "V.I."   , "Virgin Islands"          ,  "78",           1, "outlying area" #under US sovereignty"
  , "PW", "Palau"  , "Palau"                   ,  "70",           6, "freely associated state"
) 


validIN  <- colnames(statedf)[1:4]
validOUT <- colnames(statedf)



#' Convert State Indicator to Another Type 
#'
#' @param VALUE value currently representing a unique state
#' @param TYPEIN  the type of UNIQUE indicator of value, options are "abb_usps", "abb_gpo", "name", "fips"
#' @param TYPEOUT the type of indicator for output, options are "abb_usps", "abb_gpo", "name", "fips", "eta_region", "type"
#'
#' When using with mutate() on a DF, need to group rowwise() 
#'
#' @return
#' @export
#'
#' @examples
#' convert("04", "fips", "name")
#' convert(c("06", "27"), "fips", "name")
#' convert(c("03", "27"), "fips", "abb_usps")
#' convert(c("00", "03"), "fips", "abb_usps")
#' convert(c("03"), "fips", "name")
cnvrt <- function(VALUE, TYPEIN, TYPEOUT){
  
  #check validity of in/out types 
  if (!TYPEIN  %in% validIN ){ stop('Invalid TYPEIN, valid options are "abb_usps", "abb_gpo", "name", "fips"')}
  if (!TYPEOUT %in% validOUT){ stop('Invalid TYPEOUT, valid options are "abb_usps", "abb_gpo", "name", "fips", "eta_region", "type"')}
  
  validvalues <- VALUE %in% statedf[[TYPEIN]]
  validity    <- case_when(
      sum(validvalues) == 0                   ~ 0 #No valid values
    , sum(validvalues) != length(validvalues) ~ 1 #Some valid values 
    , sum(validvalues) == length(validvalues) ~ 2 #All valid values 
  )
  
  if (validity %in% c(0, 1)){
    warning(glue("Input value(s) are not valid {TYPEIN}: {paste(VALUE[which(validvalues == FALSE)], collapse=', ')}" ))
  }
  
  
  if (validity == 0){
    OUT <- rep(NA, length(VALUE))
  } else if (validity == 1){
    rownumber <- ifelse(validvalues == TRUE, which(statedf[[TYPEIN]] %in% VALUE), NA)
    OUT <- statedf[[TYPEOUT]][rownumber]
  } else if (validity == 2){
    rownumber <- which(statedf[[TYPEIN]] %in% VALUE)
    OUT <- statedf[[TYPEOUT]][rownumber]
  } else {
    stop("Something went wrong, please review function code")
  }
  
  return(OUT)
}
