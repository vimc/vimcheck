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
EXCLUDED_DISEASES <- c("Hib", "PCV", "Rota", "JE")

#' @name constants
TOUCHSTONE_OLD <- "201910"
