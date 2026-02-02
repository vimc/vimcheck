#' Example of UN-WPP time-series data
#'
#' An example of the population estimate data used by VIMC.
#'
#' @format ## `eg_wpp`
#' A data frame with 65,448 rows and 5 columns:
#' \describe{
#'   \item{country}{Country name; this is a placeholder name.}
#'   \item{year}{Year}
#'   \item{age}{Age}
#'   \item{gender}{Sex given as three categories, "Male", "Female", or "Both"}.
#'   \item{value}{Population size}
#' }
#'
#' @keywords data
#'
#' @source Derived from data originally prepared by the United Nations as part
#' of the World Population Prospects: <https://population.un.org/wpp/>.
"eg_wpp"

#' Example of VIMC burden template provided to modellers
#'
#' An example of the central burden template provided by VIMC to modelling
#' groups.
#'
#' @format ## `eg_burden_template`
#' A data frame with 10,201 rows and 11 columns:
#' \describe{
#'   \item{disease}{Disease name}
#'   \item{year}{Year}
#'   \item{age}{Age}
#'   \item{country}{Country name in short format; this is a placeholder name.}
#'   \item{country_name}{Country name in long format; this is a placeholder.}
#'   \item{cases}{Cases of the disease averted}.
#'   \item{dalys}{DALYs averted}.
#'   \item{deaths}{Deaths averted}.
#'   \item{yll}{Years of life-loss averted}.
#'   \item{cohort_size}{Population size of the country in a year}.
#'   \item{scenario}{Vaccination scenario.}
#' }
#'
#' @keywords data
#'
#' @source Prepared by the VIMC secretariat.
"eg_burden_template"

#' Example of scenario coverage data
#'
#' @format ## `eg_coverage`
#' A data frame with 11 rows and 19 columns.
#' \describe{
#'   \item{scenario_type}{Scenario type name.}
#'   \item{scenario_type_description}{Scenario type description string.}
#'   \item{scenario}{Scenario name string.}
#'   \item{scenario_description}{Scenario description string.}
#'   \item{coverage_set}{Coverage set string.}
#'   \item{gavi_support_level}{String for whether GAVI supported the scenario.}
#'   \item{source_from}{String identifier for the source.}
#'   \item{disease}{Infection identifier.}
#'   \item{vaccine}{Vaccine identifier.}
#'   \item{activity_type}{Vaccination activity identifier.}
#'   \item{year}{Year}
#'   \item{country}{Country name in short format; this is a placeholder name.}
#'   \item{age_from}{Age limit lower limit.}
#'   \item{age_to}{Age limit upper limit.}
#'   \item{age_range_verbatim}{Description of age range.}
#'   \item{target}{Target for vaccination.}
#'   \item{coverage}{Proportional coverage.}
#'   \item{gender}{Sex to which data applies, may be "Male", "Female", or
#'   "Both".}
#'   \item{proportion_risk}{Proportional risk value.}
#' }
#'
#' @keywords data
#'
#' @source Prepared by the VIMC secretariat.
"eg_coverage"

#' Example of FVP estimate data
#'
#' Example data of fully-vaccinated persons (FVPs).
#'
#' @format ## `eg_fvps`
#' A data frame with 11 rows and 24 columns:
#' \describe{
#'   \item{scenario_type}{Scenario type name.}
#'   \item{scenario_type_description}{Scenario type description string.}
#'   \item{scenario}{Scenario name string.}
#'   \item{scenario_description}{Scenario description string.}
#'   \item{coverage_set}{Coverage set string.}
#'   \item{gavi_support_level}{String for whether GAVI supported the scenario.}
#'   \item{source_from}{String identifier for the source.}
#'   \item{disease}{Infection identifier.}
#'   \item{vaccine}{Vaccine identifier.}
#'   \item{activity_type}{Vaccination activity identifier.}
#'   \item{year}{Year}
#'   \item{country}{Country name in short format; this is a placeholder name.}
#'   \item{age_from}{Age limit lower limit.}
#'   \item{age_to}{Age limit upper limit.}
#'   \item{age_range_verbatim}{Description of age range.}
#'   \item{target}{Target for vaccination.}
#'   \item{coverage}{Proportional coverage.}
#'   \item{gender}{Sex to which data applies, may be "Male", "Female", or
#'   "Both".}
#'   \item{proportion_risk}{Proportional risk value.}
#'   \item{job}{Job code as a numeric.}
#'   \item{fvps}{Count of FVPs.}
#'   \item{fvps_adjusted}{Count of adjusted FVPs.}
#'   \item{target_adjusted}{Adjusted vaccination target.}
#'   \item{coverage_adjusted}{Ratio of adjusted FVPs to adjusted target.}
#' }
#'
#' @keywords data
#'
#' @source Prepared by the VIMC secretariat.
"eg_fvps"
