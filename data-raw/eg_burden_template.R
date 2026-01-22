## code to prepare `eg_burden_template` dataset goes here

eg_burden_template <- readr::read_csv(
  "inst/extdata/central-burden-template.csv"
)

# assign dummy cohort size value from random draw
# 10e6 is the value in `eg_wpp`, allow negative differences
eg_burden_template$cohort_size <- withr::with_seed(
  1,
  rnorm(nrow(eg_burden_template), 10e6, 1e4)
)

eg_burden_template <- tidyr::crossing(
  eg_burden_template,
  scenario = c("disease_rout", "No vaccination")
)

usethis::use_data(eg_burden_template, overwrite = TRUE)
