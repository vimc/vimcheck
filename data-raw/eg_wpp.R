## code to prepare `eg_wpp` dataset goes here

# this code creates a template file that conforms to current VIMC-used
# population estimate format from the UN-WPP https://population.un.org/wpp/

library(tidyr)

country <- "RFP"

gender <- c("Male", "Female", "Both") # taken from existing reports

year_start <- 1885
year_end <- 2100
year <- seq(year_start, year_end)

value <- 10e6 # assuming a constant, medium-size pop. value

age_min <- 0
age_max <- 100
age <- seq(age_min, age_max)

eg_wpp <- crossing(
  country = country,
  year = year,
  age = age,
  gender = gender,
  value = value
)

# NOTE that this table has more entries than seen in reports
# as historical estimates are not available for all age groups

usethis::use_data(eg_wpp, overwrite = TRUE)
