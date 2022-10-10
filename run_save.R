
box::use(
  csgjcr[csg_sp_path, csg_render_ds]
)


sp <- csg_sp_path("50 State Revocations Project/MCLC Shiny App/products")


csg_render_ds("prep/gen_documentation.qmd", file.path(sp, "General_Documentation.html"))
csg_render_ds("prep/rate_comp.qmd"        , file.path(sp, "Rate_Comparison.html"))
csg_render_ds("prep/NCRP_demo.Rmd"        , file.path(sp, "NCRP_Revocations_Demo.html"))





