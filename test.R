



# prep data for area chart
fnc_areachart_adm_data_prep <- function(state_name){
  df1 <- adm_pop_long %>%
    filter(state == state_name &
             adm_or_pop == "Admissions" &
             (metric == "Total" | metric == "Supervision Violation" |
                metric == "New Offense Violation" |
                metric == "Technical Violation")) %>%
    group_by(state, year, metric, adm_or_pop, probation_or_parole) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    ungroup() %>%
    mutate(total = ifelse(total == 0, NA, total))
}

fnc_areachart_pop_data_prep <- function(state_name){
  df1 <- adm_pop_long %>%
    filter(state == state_name &
             adm_or_pop == "Population" &
             (metric == "Total" | metric == "Supervision Violation" |
                metric == "New Offense Violation" |
                metric == "Technical Violation")) %>%
    group_by(state, year, metric, adm_or_pop, probation_or_parole) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
    ungroup() %>%
    mutate(total = ifelse(total == 0, NA, total))
}

# function to create highchart area chart for state page with logo
# and data label adjustments
fnc_highchart_state_areachart_logo <-
  function(df,
           title_name,
           sup_viol_y,
           tech_y,
           new_off_y
  ){

    subtitle_name <- df %>% filter(metric == "Supervision Violation") %>%
      select(probation_or_parole)
    subtitle_name <- unique(subtitle_name$probation_or_parole)

    highchart() %>%

      hc_chart(type="area",
               events = list(render = render_image),
               marginBottom = 80,
               marginRight = 20) %>%
      hc_add_series(data = subset(df, metric == "Total"),
                    name = "Total",
                    type = "area",
                    hcaes(x = year, y = total),
                    color = total_co,
                    dataLabels = list(enabled = TRUE,
                                      format='{point.total:,.0f}')) %>%
      hc_add_series(data = subset(df, metric == "Supervision Violation"),
                    name = "Supervision Violation",
                    type = "area",
                    hcaes(x = year, y = total),
                    color = viol_co,
                    dataLabels = list(enabled = TRUE,
                                      y = sup_viol_y,
                                      format='{point.total:,.0f}')) %>%
      hc_add_series(data = subset(df, metric == "Technical Violation"),
                    name = "Technical Violation",
                    type = "area",
                    hcaes(x = year, y = total),
                    color = tech_co,
                    dataLabels = list(enabled = TRUE,
                                      y = tech_y,
                                      format='{point.total:,.0f}')) %>%
      hc_add_series(data = subset(df, metric == "New Offense Violation"),
                    name = "New Offense Violation",
                    type = "area",
                    hcaes(x = year, y = total),
                    color = new_o_co,
                    dataLabels = list(enabled = TRUE,
                                      y = new_off_y,
                                      format='{point.total:,.0f}')) %>%

      hc_xAxis(title = "", tickPositions = c(2018, 2019, 2020, 2021)) %>%
      hc_yAxis(title = "", labels=list(format="{value:,.0f}")) %>%

      hc_title(text = title_name) %>%
      hc_subtitle(text = subtitle_name) %>%

      hc_add_theme(hc_theme_jc) %>%

      hc_plotOptions(
        series = list(
          dataLabels = list(
            enabled = TRUE,
            allowOverlap = TRUE)))
  }



####################################

# STATE REPORTS - State area chart for admissions

####################################

######
# Data labels: Adjusted
######

# states with adjustments to their data labels
states <- c(
  "Alabama",
  "Arizona",
  "Arkansas",
  "California",
  "Delaware",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Indiana",
  "Iowa",
  "Kansas",
  "Louisiana",
  "Maine",
  "Minnesota",
  "Mississippi",
  "Montana",
  "Nebraska",
  "South Dakota",
  "Tennessee",
  "Utah",
  "Vermont",
  "Virginia",
  "Wyoming"
)

# generate list of state highcharts to call in app (admissions)
all_state_area_adm_adjusted <- map(.x = states,  .f = function(x) {
  df1 <- fnc_areachart_adm_data_prep(state_name = x)
  admin$mylog(glue("hc: Prison Admissions, {x}"))
  highcharts <- fnc_highchart_state_areachart_logo(df1,
                                                   "Prison Admissions",
                                                   sup_viol_y = 0,
                                                   tech_y = 4,
                                                   new_off_y = -2)
  return(highcharts)
})

all_state_area_adm_adjusted <- setNames(all_state_area_adm_adjusted, states)




######
# Data labels: Regular
######

# States with no changes to their data labels
states <- c(
  "Missouri",
  "Nevada",
  "New Hampshire",
  "New Mexico",
  "North Carolina",
  "North Dakota",
  "Ohio",
  "Oregon",
  "Rhode Island",
  "South Carolina",
  "Wisconsin"
)

# generate list of state highcharts to call in app (admissions)
all_state_area_adm_regular <- map(.x = states,  .f = function(x) {
  df1 <- fnc_areachart_adm_data_prep(state_name = x)
  admin$mylog(glue("hc: Prison Admissions, {x}"))
  highcharts <- fnc_highchart_state_areachart_logo(df1,
                                                   "Prison Admissions",
                                                   sup_viol_y = 0,
                                                   tech_y = 0,
                                                   new_off_y = 0)
  return(highcharts)
})

all_state_area_adm_regular <- setNames(all_state_area_adm_regular, states)




######
# Data labels: Manual changes
######

# States with manual changes to data labels
states <- c(
  "Alaska",
  "Colorado",
  "Connecticut",
  "Florida",
  "Illinois",
  "Kentucky",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "New Jersey",
  "New York",
  "Oklahoma",
  "Pennsylvania",
  "Washington",
  "West Virginia"
)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Alaska")
Alaska <- fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                             sup_viol_y = -2, tech_y = 0, new_off_y = 10)

# create graph for state ___ ISSUE
df1 <- fnc_areachart_adm_data_prep(state_name = "Colorado")
Colorado <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = -2, tech_y = 0, new_off_y = 12)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Connecticut")
Connecticut <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 10, new_off_y = 5)

# create graph for state ___ ISSUES
df1 <- fnc_areachart_adm_data_prep(state_name = "Florida")
Florida <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 0, new_off_y = 15)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Illinois")
Illinois <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 10, new_off_y = 0)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Kentucky")
Kentucky <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 10, new_off_y = 0)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Maryland")
Maryland <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 10, new_off_y = 0)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Massachusetts")
Massachusetts <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 5, new_off_y = 5)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Michigan")
Michigan <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 0, new_off_y = 10)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "New Jersey")
`New Jersey` <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = -5, tech_y = 5, new_off_y = 10)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "New York")
`New York` <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = -3, tech_y = 0, new_off_y = 0)

# create graph for state ___ ISSUES
df1 <- fnc_areachart_adm_data_prep(state_name = "Oklahoma")
Oklahoma <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 0, new_off_y = 15)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Pennsylvania")
Pennsylvania <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 0, new_off_y = 15)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "Washington")
Washington <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 15, new_off_y = 0)

# create graph for state
df1 <- fnc_areachart_adm_data_prep(state_name = "West Virginia")
`West Virginia` <-
  fnc_highchart_state_areachart_logo(df1, "Prison Admissions",
                                     sup_viol_y = 0, tech_y = 0, new_off_y = 5)

# combine manual charts into a list
all_state_area_adm_manual <-
  list(Alaska,
       Colorado,
       Connecticut,
       Florida,
       Illinois,
       Kentucky,
       Maryland,
       Massachusetts,
       Michigan,
       `New Jersey`,
       `New York`,
       Oklahoma,
       Pennsylvania,
       Washington,
       `West Virginia`)

# add state name to the respective graph
all_state_area_adm_manual <- setNames(all_state_area_adm_manual, states)

# combine lists into final area chart list for prison admissions by state
all_state_area_adm <- c(all_state_area_adm_adjusted,
                        all_state_area_adm_regular,
                        all_state_area_adm_manual)







####################################

# STATE REPORTS - State area chart for population

####################################

######
# Data labels: Regular
######

# regular
states <- c(
  "Alabama",
  "Alaska",
  "Arizona",
  "Arkansas",
  "Colorado",
  "Connecticut",
  "Delaware",
  "Hawaii",
  "Idaho",
  "Iowa",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Michigan",
  "Minnesota",
  "Mississippi",
  "Missouri",
  "Montana",
  "Nevada",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "North Dakota",
  "Ohio",
  "Oklahoma",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Utah",
  "Vermont",
  "Virginia",
  "Washington",
  "Wisconsin",
  "Wyoming"
)

# generate list of state highcharts to call in app (admissions)
all_state_area_pop_regular <- map(.x = states,  .f = function(x) {
  df1 <- fnc_areachart_pop_data_prep(state_name = x)
  admin$mylog(glue("hc: Prison Population, {x}"))
  highcharts <- fnc_highchart_state_areachart_logo(df1,
                                                   "Prison Population",
                                                   sup_viol_y = 0,
                                                   tech_y = 0,
                                                   new_off_y = 0)
  return(highcharts)
})

all_state_area_pop_regular <- setNames(all_state_area_pop_regular, states)





######
# Data labels: Manual changes
######

# manual
states <- c(
  "California",
  "Hawaii",
  "Illinois",
  "Indiana",
  "Kansas",
  "Massachusetts",
  "New York",
  "North Carolina",
  "Oregon",
  "Tennessee",
  "West Virginia"
)

# Create graph for California
df1 <- fnc_areachart_pop_data_prep(state_name = "California")
California <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 10)

# Create graph for Hawaii
df1 <- fnc_areachart_pop_data_prep(state_name = "Hawaii")
Hawaii <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 20, new_off_y = 0)

# Create graph for Illinois
df1 <- fnc_areachart_pop_data_prep(state_name = "Illinois")
Illinois <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 10)

# Create graph for Indiana ____ ISSUES
df1 <- fnc_areachart_pop_data_prep(state_name = "Indiana")
Indiana <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 10, new_off_y = 0)

# Create graph for Kansas ____ ISSUES
df1 <- fnc_areachart_pop_data_prep(state_name = "Kansas")
Kansas <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 15)

# Create graph for Massachusetts
df1 <- fnc_areachart_pop_data_prep(state_name = "Massachusetts")
Massachusetts <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = -20, tech_y = -10, new_off_y = 0)

# Create graph for New York
df1 <- fnc_areachart_pop_data_prep(state_name = "New York")
`New York` <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 8, new_off_y = 5)

# Create graph for North Carolina
df1 <- fnc_areachart_pop_data_prep(state_name = "North Carolina")
`North Carolina` <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 10)

# Create graph for Oregon ____ ISSUES
df1 <- fnc_areachart_pop_data_prep(state_name = "Oregon")
Oregon <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 0, new_off_y = 12)

# Create graph for Tennessee
df1 <- fnc_areachart_pop_data_prep(state_name = "Tennessee")
Tennessee <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 10, new_off_y = 5)

# Create graph for West Virginia
df1 <- fnc_areachart_pop_data_prep(state_name = "West Virginia")
`West Virginia` <- fnc_highchart_state_areachart_logo(df1, "Prison Population", sup_viol_y = 0, tech_y = 10, new_off_y = 10)


# combine manual charts into a list
all_state_area_pop_manual <-
  list(California,
       Hawaii,
       Illinois,
       Indiana,
       Kansas,
       Massachusetts,
       `New York`,
       `North Carolina`,
       Oregon,
       Tennessee,
       `West Virginia`)

# add state name to the respective graph
all_state_area_pop_manual <- setNames(all_state_area_pop_manual, states)

# combine lists into final area chart list for prison population by state
all_state_area_pop <- c(all_state_area_pop_regular,
                        all_state_area_pop_manual)





all_state_area_adm$Alabama
all_state_area_adm$Alaska
all_state_area_adm$Arizona
all_state_area_adm$Arkansas
all_state_area_adm$California
all_state_area_adm$Colorado
all_state_area_adm$Connecticut
all_state_area_adm$Delaware
all_state_area_adm$Florida
all_state_area_adm$Georgia
all_state_area_adm$Hawaii
all_state_area_adm$Idaho
all_state_area_adm$Illinois
all_state_area_adm$Indiana
all_state_area_adm$Iowa
all_state_area_adm$Kansas
all_state_area_adm$Kentucky
all_state_area_adm$Louisiana
all_state_area_adm$Maine
all_state_area_adm$Maryland
all_state_area_adm$Massachusetts
all_state_area_adm$Michigan
all_state_area_adm$Minnesota
all_state_area_adm$Mississippi
all_state_area_adm$Missouri
all_state_area_adm$Montana
all_state_area_adm$Nebraska
all_state_area_adm$Nevada
all_state_area_adm$`New Hampshire`
all_state_area_adm$`New Jersey`
all_state_area_adm$`New Mexico`
all_state_area_adm$`New York`
all_state_area_adm$`North Carolina`
all_state_area_adm$`North Dakota`
all_state_area_adm$Ohio
all_state_area_adm$Oklahoma
all_state_area_adm$Oregon
all_state_area_adm$Pennsylvania
all_state_area_adm$`Rhode Island`
all_state_area_adm$`South Carolina`
all_state_area_adm$`South Dakota`
all_state_area_adm$Tennessee
all_state_area_adm$Texas
all_state_area_adm$Utah
all_state_area_adm$Vermont
all_state_area_adm$Virginia
all_state_area_adm$Washington
all_state_area_adm$`West Virginia`
all_state_area_adm$Wisconsin
all_state_area_adm$Wyoming



all_state_area_pop$Alabama
all_state_area_pop$Alaska
all_state_area_pop$Arizona
all_state_area_pop$Arkansas
all_state_area_pop$California
all_state_area_pop$Colorado
all_state_area_pop$Connecticut
all_state_area_pop$Delaware
all_state_area_pop$Florida
all_state_area_pop$Georgia
all_state_area_pop$Hawaii
all_state_area_pop$Idaho
all_state_area_pop$Illinois
all_state_area_pop$Indiana
all_state_area_pop$Iowa
all_state_area_pop$Kansas
all_state_area_pop$Kentucky
all_state_area_pop$Louisiana
all_state_area_pop$Maine
all_state_area_pop$Maryland
all_state_area_pop$Massachusetts
all_state_area_pop$Michigan
all_state_area_pop$Minnesota
all_state_area_pop$Mississippi
all_state_area_pop$Missouri
all_state_area_pop$Montana
all_state_area_pop$Nebraska
all_state_area_pop$Nevada
all_state_area_pop$`New Hampshire`
all_state_area_pop$`New Jersey`
all_state_area_pop$`New Mexico`
all_state_area_pop$`New York`
all_state_area_pop$`North Carolina`
all_state_area_pop$`North Dakota`
all_state_area_pop$Ohio
all_state_area_pop$Oklahoma
all_state_area_pop$Oregon
all_state_area_pop$Pennsylvania
all_state_area_pop$`Rhode Island`
all_state_area_pop$`South Carolina`
all_state_area_pop$`South Dakota`
all_state_area_pop$Tennessee
all_state_area_pop$Texas
all_state_area_pop$Utah
all_state_area_pop$Vermont
all_state_area_pop$Virginia
all_state_area_pop$Washington
all_state_area_pop$`West Virginia`
all_state_area_pop$Wisconsin
all_state_area_pop$Wyoming
