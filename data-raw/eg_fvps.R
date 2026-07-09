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

## A second example of FVP data
eg_fvps_2 <- tibble::tibble(
  country = c("AGO", "AGO", "BEN", "BEN"),
  year = c(2020, 2021, 2020, 2021),
  activity_type = c("routine", "campaign", "routine", "campaign"),
  scenario_type = c("default", "default", "default", "default"),
  vaccine = c("measles", "measles", "measles", "measles"),
  coverage_adjusted = c(0.8, 0.85, 0.4, 0.7),
  fvps = c(1000000, 1200000, 800000, 900000)
)

usethis::use_data(eg_fvps_2, overwrite = TRUE)
