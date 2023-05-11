library(rmarkdown)

render("rMarkdown\\dataCleaning.Rmd",
       md_document(variant = "markdown_github"),
       output_dir = "Output")
