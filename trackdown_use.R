
library("trackdown")

rel_file <- "role_of_stats.Rmd"
rel_file <- "research_designs.Rmd"
rel_file <- "tidyverse-intro.Rmd"
rel_file <- "tidyverse-examples-exercises.Rmd"

## step 1: upload file the first time you want to use it:
upload_file(rel_file)

## step 2: make changes on Google doc
## THEN ACCEPT ALL CHANGES
## then:
download_file(rel_file)
writeLines(iconv(readLines(rel_file), from = "", to = "UTF8"), 
           file(rel_file, encoding="UTF-8"))

## then update again on Google docs:
update_file(file = rel_file)

