
library("trackdown")

rel_file <- "role_of_stats.Rmd"
rel_file <- "research_designs.Rmd"
rel_file <- "tidyverse-intro.Rmd"
rel_file <- "tidyverse-examples-exercises.Rmd"
rel_file <- "ggplot-intro.Rmd"
rel_file <- "ggplot-exercises.Rmd"
rel_file <- "02-standard_approach.Rmd"

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

#### Google Doc Links:

# Chapter 1: Role of Stats
# https://docs.google.com/document/d/1HXOxOW8HZC3dWSCSKa0Rxylpotq7GJF3uthXUIRhHV8/edit?usp=sharing

# Chapter 2: Research Designs 
# https://docs.google.com/document/d/1HXOxOW8HZC3dWSCSKa0Rxylpotq7GJF3uthXUIRhHV8/edit?usp=sharing

# Chapter 3: Tidyverse Intro
# https://docs.google.com/document/d/1Eajb4HL9gtecbBJSKjMAk3k-LzLOaNUBQoonoRWNBhQ/edit?usp=sharing

# Chapter 3: Tidyverse Exercises
# https://docs.google.com/document/d/1ujTfFehV1DCxVNNKJUu9gsFuEUHdV99RWJYi5IIsAqY/edit?usp=sharing

# Chapter 4: ggplot introduction
# https://docs.google.com/document/d/1xM9XnX4R9iwBFAnhwinpMsATu-bREPPkxqZObDl1ICo/edit?usp=sharing

# Chapter 4: ggplot2 exercises
# https://docs.google.com/document/d/1q11La7Caz-ewwTMtqZ_WKdhhWP7QGAV1sOCMYdvk3f4/edit?usp=sharing

# Chapter 5: Standard approach
# https://docs.google.com/document/d/1z9zDpbqX_6-A8P35GgVDCQiPwLTswKoEelZ9vLpaIuM/edit?usp=sharing