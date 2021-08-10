
library("trackdown")

## step 1: upload file the first time you want to use it:
upload_file("role_of_stats.Rmd")

## step 2: make changes on Google doc
## THEN ACCEPT ALL CHANGES
## then:
download_file("role_of_stats.Rmd")

## then update again on Google docs:
update_file(file = "role_of_stats.Rmd")
