# MCLCShiny

# Repository Structure

```
MCLCShiny 
|
| #code/app files 
|-- library.R     # load packages  
|-- import.R      # imports MCLC data and shapefiles    
|-- dataframes.R  # loads converted data, colors, fonts  
|-- functions.R   # custom functions  
|-- ui.R          # user interface  
|-- server.R      # server  
|-- app.R         # run app  
|
|-- Data          # folder with data, add to clone 
|
|--www             
   |-- theme.R    # custom CSS for app    
```


# Data

[MCLC](https://github.com/CSGJusticeCenter/cc_survey)   
[Research Sharepoint](https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I)  

# Proposed Layout

[Version 1](https://csgorg.sharepoint.com/:b:/s/Team-JC-Research/ETqrxyp6YxtNukNUEUFb7AgB9vSEH1ruToRtqsqWkVPSrQ?e=VxK02R)   

# Notes

- Must download csgjcr package. If you run into issues, make sure in your Renviron that CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"  


# Getting Started 

After creating a clone, there are a few other steps you will need to take in order to run the app.  

1. Download necessary package, look at `library.R` file for package list
1. Create a new folder, called `Data` 
1. Run the `import.R` file to create the data 
    * *Note:* You may need to change the `FULL_JC_FOLDER` variable depending on how you sync your SharePOint folder
    * `FULL_JC_FOLDER <- TRUE`: You sync the entire JC Rersearch - Documents folder to your machine, thus the folder path will include the project folder which is '50 State Revocations Project' 
    * `FULL_JC_FOLDER <- FALSE`: You sync just the specific sub-folder 'MCLC Shiny App' to your machine, thus the folder path does not include the project folder 
    
After completing these steps you can run the app, either by opening `app.R`/`ui.R`/`server.R` and click the **Run App** button OR by entering `shiny::runApp()` into the console.  



