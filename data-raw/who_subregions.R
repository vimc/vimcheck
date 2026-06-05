## code to prepare `who_subregions` dataset goes here

library(readr)
library(dplyr)

who_subregions <- read_csv("inst/extdata/who_sub_regions.csv")

usethis::use_data(who_subregions, overwrite = TRUE)
