rmarkdown::clean_site(preview = FALSE)

# Build the book first: downlit creates its links during this step.
bookdown::render_book(
  "index.Rmd",
  output_format = "bookdown::bs4_book"
)

# Correct the generated ggbeeswarm documentation links.
topics <- c("geom_beeswarm", "geom_quasirandom")

old_urls <- paste0(
  "https://rdrr.io/pkg/ggbeeswarm/man/",
  topics, ".html"
)

new_urls <- paste0(
  "https://search.r-project.org/CRAN/refmans/ggbeeswarm/html/",
  topics, ".html"
)

html_files <- list.files(
  "docs",
  pattern = "\\.html$",
  recursive = TRUE,
  full.names = TRUE
)

changed <- 0L

for (file in html_files) {
  original <- readLines(file, warn = FALSE, encoding = "UTF-8")
  updated <- original

  for (i in seq_along(old_urls)) {
    updated <- gsub(
      old_urls[i], new_urls[i], updated,
      fixed = TRUE
    )
  }

  if (!identical(original, updated)) {
    writeLines(updated, file, useBytes = TRUE)
    message("Fixed links in: ", file)
    changed <- changed + 1L
  }
}

message("Updated ", changed, " HTML files.")
