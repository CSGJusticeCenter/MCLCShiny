
sp_path <- csgjcr::csg_sp_path("50 State Revocations Project/MCLC Shiny App/products")


###############################
### QMD's 

## save qmd's to sharepoint path 
run_qmd <- function(SP_PATH, IN, OUT){
  
  datestamp <- gsub("-", "", Sys.Date())
  outfile1 <- file.path(SP_PATH, "datestamp", paste0(datestamp, "_", OUT))
  outfile2 <- file.path(SP_PATH,                        OUT )
  
  #https://stackoverflow.com/questions/72346829/two-problems-rendering-a-qmd-file-with-quarto-render-from-r
  #quarto::quarto_render(input = "prep/NCRP/gen_documentation.qmd", output_file = "prep/NCRP/test/gen_documentation.html")
  
  inpath_end <- stringr::str_locate_all(IN, "/")[[1]][,2] |> tail(1)
  inpath     <- stringr::str_sub(IN, 1, inpath_end)
  outpath    <- paste0(inpath, OUT)
  
  quarto::quarto_render(input = IN, output_file = outpath)
  file.copy(from = outpath, to = outfile1, overwrite = TRUE)
  file.copy(from = outpath, to = outfile2, overwrite = TRUE)
  file.remove(outpath)
  file.show(outfile2)
  
}

run_qmd(sp_path, "prep/gen_documentation.qmd", "General_Documentation.html")
run_qmd(sp_path, "prep/rate_comp.qmd"        , "Rate_Comparison.html")


###############################
### RMD's 

## save Rmd's to sharepoint path 
run_Rmd <- function(SP_PATH, IN, OUT){
  
  datestamp <- gsub("-", "", Sys.Date())
  outfile1 <- file.path(SP_PATH, "datestamp", paste0(datestamp, "_", OUT))
  outfile2 <- file.path(SP_PATH,                        OUT )
  
  rmarkdown::render(input = IN, output_file = outfile1)
  rmarkdown::render(input = IN, output_file = outfile2)
  file.show(outfile2)
  
}

run_Rmd(sp_path, "prep/NCRP_demo.Rmd", "NCRP_Revocations_Demo.html")







