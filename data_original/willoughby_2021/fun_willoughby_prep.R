
make_parent_score <- function(score_m, score_f) {
  case_when(
    is.na(score_f) & is.na(score_m) ~ NA_real_,
    is.na(score_f) ~ score_m,
    is.na(score_m) ~ score_f,
    TRUE ~ rowMeans(cbind(score_m, score_f), na.rm = TRUE)
  )
}

make_parent_code <- function(score_m, score_f) {
  case_when(
    is.na(score_f) & is.na(score_m) ~ NA_character_,
    is.na(score_f) ~ "m",
    is.na(score_m) ~ "f",
    TRUE ~ "c"
  )
}
