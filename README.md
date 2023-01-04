# MCLCShiny

Links current live version (vNA): **add after create first production version, v1.0.00**  

- https://csgjc.shinyapps.io/MCLCShiny/#mapexplorer  
- https://csgjc.shinyapps.io/MCLCShiny/#statereports 
- https://csgjc.shinyapps.io/MCLCShiny/#downloaddata  


# Structure 

## Branches (proposed)

```
trunk                     # production branch 
|
|-- develop              # development branch 
    |
    |-- taskbranch1      # working branch 
    |-- taskbranch2      # working branch 
        |
        |--taskbranch2_b # working branch 
```


## Files 

```
MCLCShiny 
|
|-- app
|   |-- box           # box modules 
|   |
|   |-- data          # data for shiny app 
|   |   |-- infogs    # infographs png files 
|   |
|   |-- www             
|   |   |-- theme.R   # custom CSS for app  
|   |
|   |-- app.R         # run app  
|   |-- colors. R     # assigned colors
|   |-- dataframes.R  # loads converted data, fonts  
|   |-- functions.R   # custom functions  
|   |-- library.R     # load packages  
|   |-- ui.R          # user interface  
|   |-- server.R      # server  
|
|-- shiny_prep.R      # run shiny prep 
|-- shiny_prep_log.txt #log file 
|-- import.R          # imports MCLC data and shapefiles 
|-- highchart.R       # create and save highcharts for app
|-- reactable.R       # create and save reactable tables for app
|-- run_save.R        # shiny prep for R/E infographs & tables
| 
|-- prep              # folder for prep data/info for app 
    |-- box           # box modules for preping R/E data 
    |-- infographics  # exploration data 
    |...              # various files for exploration/documentation 
```

# Run App 


After creating a clone, there are a few other steps you will need to take in order to run the app.  

1. Download necessary package, look at `library.R` file for package list
1. Create a new folder, called `data` within the `app` folder
1. Run the `shiny_prep.R` file to create the data 
    * *Note:* You may need to change your root folder variable depending on how you sync your SharePoint folder
    * Open the `prep/box/ROOT.R` file, and edit the `sp` string to reflect your root folder 
    * The root folder should be the string you would entere into the `csgjc::csg_sp_path()` function to get the pathway to the `MCLC Shiny App` folder on SharePoint 
    * This will take ~15 minutes to run this folder (majority of the time is creating the infographic pngs) 
    * runnig this script will produce a log txt file 
    
After completing these steps you can run the app, either by opening `app.R`/`ui.R`/`server.R` and click the **Run App** button OR by entering `shiny::runApp()` into the console.


# Shiny

## Shiny Releases 

Releases are made up of 3 values and shown in the format of v0.0.00

**First Number: `1`.0.00** - indicates verion that is 'live' (e.g. shipped to client, publicly available on website)  
**Second Number: 0.`1`.00** - indicates a version that was shared with the broader external working group  
**Third Number: 0.0.`01`** - any time a new version is pushed to the R Shiny servers, but is not shared externally from research or live  

# Data

## MCLC Survey 

[MCLC](https://github.com/CSGJusticeCenter/cc_survey)   
[Research Sharepoint](https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I)  


## Racial and Ethnic Disparites in Revocations 

Additional documention can be found here: [~/50 State Revocations Project/MCLC Shiny App/products/General_Documentation.html](https://csgorg.sharepoint.com/sites/Team-JC-Research/Shared%20Documents/Forms/AllItems.aspx?ct=1652815580832&or=Teams%2DHL&ga=1&id=%2Fsites%2FTeam%2DJC%2DResearch%2FShared%20Documents%2F50%20State%20Revocations%20Project%2FMCLC%20Shiny%20App%2Fproducts%2FGeneral%5FDocumentation%2Ehtml&viewid=134e5f2c%2Df80c%2D46ef%2D8491%2D1eac7193eb98&parent=%2Fsites%2FTeam%2DJC%2DResearch%2FShared%20Documents%2F50%20State%20Revocations%20Project%2FMCLC%20Shiny%20App%2Fproducts)

### NCRP (National Corrections Reporting Progam)


|   |    |
|:--------------|:-------------------------------------------------------------|
| **Usage**     | Revocation Counts by state/year/race/offense general |
| **Source**    | [National Corrections Reporting Program 1991-2020 ICPSR 38492](https://www.icpsr.umich.edu/web/ICPSR/studies/38492)  |
| **SharePoint**| [JC Research - Documents/CC/data/raw/NCRP](https://csgorg.sharepoint.com/sites/Team-JC-Research/Shared%20Documents/Forms/AllItems.aspx?ct=1652815580832&or=Teams%2DHL&ga=1&id=%2Fsites%2FTeam%2DJC%2DResearch%2FShared%20Documents%2FCC%2Fdata%2Fraw%2FNCRP&viewid=134e5f2c%2Df80c%2D46ef%2D8491%2D1eac7193eb98) |


- 2 options for data used: 
    * A: Prison Admissions, individual records, `DS0002/38492-0002-Data.rda`
    * N: Year-end population, individual records, `DS0004/38492-0004-Data.rda`
- filter to only include `ADMTYPE` == "(2) Parole return/revocation" and `RPTYEAR` >= 2015
- recode `RACE` variable to shorter names 
- Count based on different cross sections 

### PUMS American Community Survey Data (Community Population)

|   |    |
|:--------------|:-------------------------------------------------------------|
| **Usage**     | Population Estimates by state/year/race, denominator to calculate rates  |
| **Source**    | [Census American Community Survey Public Use Microdata Sample](https://www.census.gov/programs-surveys/acs/microdata/documentation.html)  |
| **SharePoint**| [JC Research - Documents/50 State Revocations Project/MCLC Shiny App/data/raw/PUMS](https://csgorg.sharepoint.com/sites/Team-JC-Research/Shared%20Documents/Forms/AllItems.aspx?ct=1652815580832&or=Teams%2DHL&ga=1&id=%2Fsites%2FTeam%2DJC%2DResearch%2FShared%20Documents%2F50%20State%20Revocations%20Project%2FMCLC%20Shiny%20App%2Fdata%2Fraw%2FPUMS&viewid=134e5f2c%2Df80c%2D46ef%2D8491%2D1eac7193eb98)
| **Years**     | 2015-2019, 2021 (2020 missing due to covid) | 


Public Use Microdata Sample (PUMS).  Data is pulled from PUMS API using the `{tidycensus}` package.  The data is pulled and saved on SharePoint.  

**PUMS data is unavailable for 2020 due to covid19.  For 2020 revocations counts, rates per population will use 2019 PUMS data.**  

-   filter data to be 18+
-   re-code Race/Ethnicity categories to match RACE NCRP categories
-   sum across categories
-   add state ids
-   use population estimates for 2015-2019 (population estimate for a specific year is used for calculating rates for that NCRP rates for that year)


#### Race/Ethnicity Re-code 


| PUMS variables | `RACE` Re-code | Description | 
|:---------------|:----------|:--------------|
| ` HISP %in% c("1", "01") & RAC1P  == "1"` | White | Not Hispanic and White alone | 
| `!HISP %in% c("1", "01") & RACBLK == "0"` | Hispanic | Hispanic, not Black  | 
| `RACBLK == 1`                             | Black | Black alone, or in combination with any ethnicity | 
| Everything else                           | Other | All others | 


### BJS APS Data (Parole Data) 

|   |    |
|:--------------|:--------------------------------------------------------------------------------------------------------|
| **Usage**     | Projection of population on parole or probation                                                                 |
| **Source**    | [BJS Annual Probation Survey and Annual Parole Survey -> Documentation -> Codebooks and datasets](lection/annual-probation-survey-and-annual-parole-survey#documentation-0)<br>Parole: [Annual Parole Survey series](https://www.icpsr.umich.edu/web/NACJD/series/328) | 
| **SharePoint**| Parole: [JC Research - Documents/CC/data/raw/BJS/AParS](https://csgorg.sharepoint.com/sites/Team-JC-Research/Shared%20Documents/Forms/AllItems.aspx?ct=1652815580832&or=Teams%2DHL&ga=1&id=%2Fsites%2FTeam%2DJC%2DResearch%2FShared%20Documents%2FCC%2Fdata%2Fraw%2FBJS%2FAParS&viewid=134e5f2c%2Df80c%2D46ef%2D8491%2D1eac7193eb98) | 
| **Years** | 2015-2018



- only available to 2018
- released on a report year basis, have to combine
- **only use parole data** (because that's what matches the NCRP data) 
- use the following variables from APS
  * `STATE`: state abbreviation, can be used to create other state id columns 
  * Race variables: 
    * `WHITE`
    * `BLACK` 
    * `HISP` 
    * `UNKRACE` 
    * `TOTRACE` 
  * Note that `TOTRACE` == `TOTEND` (total at year end) in all instances for Parole/Probation 2013-2018
  * `RPTYEAR` year of survey (added variable during import)
  


## Proposed Layout

[Version 1](https://csgorg.sharepoint.com/:b:/s/Team-JC-Research/ETqrxyp6YxtNukNUEUFb7AgB9vSEH1ruToRtqsqWkVPSrQ?e=VxK02R)   

## Notes

- Must download csgjcr package. If you run into issues, make sure in your Renviron that CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"  


  



