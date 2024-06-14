output.filename <- paste(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "IgM_plsm", "Validation.docx", sep = "_")
rmarkdown::render("autoValOffcDwnWrd.Rmd", 
                  output_file = output.filename, 
                  output_format = "all", 
                  params = list(method = selectedMethod()))
