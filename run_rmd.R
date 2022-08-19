
## save rmd's to sharepoint file 

sp_path <- csgjcr::csg_sp_path("50 State Revocations Project/MCLC Shiny App/prep_NCRP")
datestamp <- gsub("-", "", Sys.Date())



infile <- "prep/NCRP/NCRP_demo.Rmd"
outfile<- file.path(sp_path, paste0(datestamp, "_NCRP_Revocations_Demo.html"))

rmarkdown::render(
    input       = infile
  , output_file = outfile
)


file.show(outfile)


