set.seed(667)

options(width = 60)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  #cache = TRUE,
  out.width = "100%",
  #fig.align = 'center',
  #fig.width = 4,
  fig.asp = 0.618 #,  # 1 / phi
  #fig.show = "hold"
)

options(dplyr.print_min = 6, dplyr.print_max = 6)

# Activate crayon output
options(
  crayon.enabled = TRUE,
  pillar.bold = TRUE,
  stringr.html = FALSE
)
