#' Package constants
#'
#' @description
#' Constant values used in _vimcheck_. See the **Examples** section for the
#' constant values.
#'
#' @name constants
#' @rdname constants
#'
#' @keywords constants
#'
#' @examples
#' file_dict_colnames
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
#'
#' @examples
#' scenario_data_colnames
#'
#' @export
scenario_data_colnames <- c(
  "scenario_type",
  "scenario_type_description",
  "scenario",
  "scenario_description"
)

#' @name constants
#'
#' @examples
#' burden_outcome_names
#'
#' @export
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
#'
#' @examples
#' colnames_plot_demog_compare
#'
#' @export
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
#'
#' @examples
#' colnames_df_missing_cols
#'
#' @export
colnames_df_missing_cols <- c(
  "country_name",
  "vaccine",
  "activity_type",
  "year",
  "modelling_group"
)

#' @name constants
#'
#' @examples
#' COLNAMES_KEY_PRESSURE_TEST
#'
#' @export
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
#'
#' @examples
#' COLNAMES_INTEREST_PRESSURE_TEST
#'
#' @export
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
#'
#' @examples
#' IMPACT_OUTCOMES
#'
#' @export
IMPACT_OUTCOMES <- c("deaths_averted", "dalys_averted")

IMPACT_GROUP_VARS <- c("activity_type", "vaccine")

#' @name constants
#'
#' @examples
#' EXCLUDED_DISEASES
#'
#' @export
EXCLUDED_DISEASES <- c("Hib", "PCV", "Rota", "JE")

#' @name constants
#'
#' @examples
#' N_TS_MIN_CHARS
#'
#' @export
N_TS_MIN_CHARS <- 6L

#' @name constants
#'
#' @examples
#' N_TS_YEAR_CHARS
#'
#' @export
N_TS_YEAR_CHARS <- 4L

#' @name constants
#'
#' @examples
#' MIN_TS_YEAR
#'
#' @export
MIN_TS_YEAR <- 2000

#' @name constants
#'
#' @examples
#' MAX_TS_YEAR
#'
#' @export
MAX_TS_YEAR <- 2100

#' @name constants
#'
#' @examples
#' MIN_TS_MONTH
#'
#' @export
MIN_TS_MONTH <- 1

#' @name constants
#'
#' @examples
#' MAX_TS_MONTH
#'
#' @export
MAX_TS_MONTH <- 12

#' @name constants
#'
#' @examples
#' DEF_TOUCHSTONE_OLD
#'
#' @export
DEF_TOUCHSTONE_OLD <- "201910"

#' @name constants
#'
#' @examples
#' DEF_TOUCHSTONE_NEW
#'
#' @export
DEF_TOUCHSTONE_NEW <- "202310"

#' @name constants
#'
#' @examples
#' DEF_TOUCHSTONE_OLD_OLD
#'
#' @export
DEF_TOUCHSTONE_OLD_OLD <- "202110"

#' @name constants
#'
#' @examples
#' COLOUR_VIMC
#'
#' @export
COLOUR_VIMC <- "#008080"

#' @name constants
#' 
#' @examples
#' pine
#' 
#' @export
pine <- c("PAK", "IND", "NGA", "ETH")