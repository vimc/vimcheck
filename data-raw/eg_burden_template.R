## code to prepare `eg_burden_template` dataset goes here

library(readr)

eg_burden_template <- read_csv("inst/extdata/central-burden-template.csv")

usethis::use_data(eg_burden_template, overwrite = TRUE)
