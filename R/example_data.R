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
