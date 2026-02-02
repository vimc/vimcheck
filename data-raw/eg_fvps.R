## code to prepare `eg_fvps` dataset goes here

eg_fvps <- readr::read_csv(
  "inst/extdata/coverage_scenarios.csv",
  show_col_types = FALSE
)

# add cols to make eg_FVPs data
eg_fvps$target <- seq(1.5e6, 1.6e6, length.out = nrow(eg_fvps))
eg_fvps$job <- 1
eg_fvps$fvps <- seq(9e5, 1.5e6, length.out = nrow(eg_fvps))
eg_fvps$fvps_adjusted <- eg_fvps$fvps # assumed same as FVPs
eg_fvps$target_adjusted <- eg_fvps$target # assumed same as target
eg_fvps$coverage_adjusted <- eg_fvps$fvps_adjusted / eg_fvps$target_adjusted

usethis::use_data(eg_fvps, overwrite = TRUE)
