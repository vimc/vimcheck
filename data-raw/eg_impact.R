## code to prepare `eg_impact` dataset goes here

library(dplyr)
library(countrycode)

eg_impact <- rbind(
  readRDS("inst/extdata/impact_method2a.rds"),
  readRDS("inst/extdata/impact_method2b.rds")
)

# assign disease inferred from vaccine names
# infer MCV to be for MenA from report
eg_impact <- mutate(
  eg_impact,
  disease = case_when(
    vaccine %in% c("HepB", "HepB_BD") ~ "HepB",
    vaccine %in% c("MCV1", "MCV2") ~ "MenA",
    .default = vaccine
  )
)

# infer country name from ISO code (only includes PINE but for future-proofing)
eg_impact <- mutate(
  eg_impact,
  country_name = countrycode::countrycode(country, "iso3c", "country.name")
)

usethis::use_data(eg_impact, overwrite = TRUE)

# second impact dataset
eg_impact_2 <- tibble::tibble(
  country = c("A", "A", "B", "B"),
  year = c(2020, 2021, 2020, 2021),
  birth_cohort = c(2000, 2001, 2000, 2001),
  burden_outcome = c("deaths", "cases", "deaths", "cases"),
  impact = c(15, 5, 14, 8),
  short_name = c("short1", "short2", "short3", "short4")
)

usethis::use_data(eg_impact_2, overwrite = TRUE)
