## code to prepare `eg_coverage` dataset goes here

eg_coverage <- readr::read_csv(
  "inst/extdata/coverage_scenarios.csv",
  show_col_types = FALSE
)

usethis::use_data(eg_coverage, overwrite = TRUE)
