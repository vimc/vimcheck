#' Package constants
#'
#' @name constants
#' @rdname constants
#'
#' @keywords constants
#'
#' @export
file_dict_colnames <- c(
  "scenario_type",
  "scenario_type_description",
  "scenario",
  "scenario_description",
  "file"
)

#' @name constants
scenario_data_colnames <- c(
  "scenario_type",
  "scenario_type_description",
  "scenario",
  "scenario_description"
)

#' @name constants
burden_outcome_names <- c(
  "cases",
  "deaths",
  "dalys",
  "yll",
  "deaths_cwyx",
  "cases_cwyx",
  "dalys_cwyx",
  "yll_cwyx",
  "rubella_deaths_congenital",
  "rubella_cases_congenital"
)

#' @name constants
colnames_plot_demog_compare <- c(
  "variable",
  "scenario",
  "year",
  "age",
  "country",
  "value",
  "value_millions"
)

#' @name constants
colnames_df_missing_cols <- c(
  "country_name",
  "vaccine",
  "activity_type",
  "year",
  "modelling_group"
)

#' @name constants
COLNAMES_KEY_PRESSURE_TEST <- c(
  "country",
  "country_name",
  "vaccine",
  "activity_type",
  "year",
  "disease",
  "modelling_group"
)

#' @name constants
COLNAMES_INTEREST_PRESSURE_TEST <- union(
  COLNAMES_KEY_PRESSURE_TEST,
  c(
    "fvps",
    "target_population",
    "coverage",
    "deaths_averted",
    "dalys_averted",
    "deaths_averted_rate",
    "dalys_averted_rate"
  )
)

#' @name constants
EXCLUDED_DISEASES <- c("Hib", "PCV", "Rota", "JE")

#' @name constants
N_TS_MIN_CHARS <- 6L

#' @name constants
N_TS_YEAR_CHARS <- 4L

#' @name constants
MIN_TS_YEAR <- 2000

#' @name constants
MAX_TS_YEAR <- 2100

#' @name constants
MIN_TS_MONTH <- 1

#' @name constants
MAX_TS_MONTH <- 12

#' @name constants
DEF_TOUCHSTONE_OLD <- "201910"

#' @name constants
DEF_TOUCHSTONE_NEW <- "202310"

#' @name constants
DEF_TOUCHSTONE_OLD_OLD <- "202110"
