library(rmarkdown)


render("rMarkdown\\dataCleaning.Rmd",
       output_format=md_document(variant = "markdown_github"),
       output_dir = "Output")

render("rMarkdown\\dataAnalysis.Rmd",
       output_format=md_document(variant = "markdown_github"),
       output_dir = "Output")
