#' Prepare data for plotting
#'
#' @name plotting_prep
#' @rdname plotting_prep
#'
#' @description
#' Transform burden estimate data from modelling groups to make them suitable
#' for plotting using an appropriate [plotting function][plotting]. Each
#' preparation function corresponds to a plotting function.
#'
#' @param burden For `prep_plot_demography()`, a `<tibble>` output from
#' [check_demography_alignment()].
#' For other functions, a burden dataset similar to [eg_burden_template].
#'
#' @return
#'
#' - For `prep_plot_demography()`: a `<tibble>` in long-format, with the
#' identifier-columns, "scenario", "age", and "year", with the added column
#' "value_millions".
#'
#' - For `prep_plot_age()`: a `<tibble>` with the columns "scenario",
#' "burden_outcome", "age", "value_millions".
#'
#' - For `prep_plot_burden_decades()`: a `<tibble>` with the columns "scenario",
#' "burden_outcome", "decade_label", and "value_millions".
#'
#' - For `prep_plot_global_burden()`: a nested `<tibble>` with the string
#' column "burden_outcome", and a list column of tibbles "burden_data".
#'
#' - For `prep_plot_coverage_set()`: WIP
#'
#' - For `prep_plot_fvp()`: WIP.
#'
#' @export
prep_plot_demography <- function(burden) {
  checkmate::assert_tibble(burden)

  # NOTE: this data is expected to come from `check_demography_alignment()`
  # there are expected to be more colnames: abs_diff, prop_diff
  names_melting_data <- c(
    "scenario",
    "age",
    "year",
    "expected",
    "provided",
    "difference",
    "country"
  )

  checkmate::assert_names(
    colnames(burden),
    must.include = names_melting_data
  )

  names_melting_by <- c("scenario", "age", "year", "country")

  burden <- dplyr::select(
    burden,
    {{ names_melting_data }}
  )

  burden_long <- tidyr::pivot_longer(
    burden,
    !{{ names_melting_by }},
    names_to = "variable"
  )

  burden_long <- dplyr::summarise(
    burden_long,
    value = sum(.data$value),
    .by = c("variable", "scenario", "year", "age", "country")
  )
  burden_long <- dplyr::mutate(
    burden_long,
    value_millions = .data$value / 1e6
  )
  burden_long <- dplyr::arrange(
    burden_long,
    "age"
  )

  # set factor levels for variable
  burden_long$variable <- forcats::fct_relevel(
    burden_long$variable,
    c("expected", "provided", "difference")
  )

  burden_long
}

#' @name plotting_prep
#'
#' @export
prep_plot_age <- function(burden) {
  checkmate::assert_tibble(burden)

  burden_long <- tidyr::pivot_longer(
    burden,
    {{ burden_outcome_names }},
    names_to = "burden_outcome"
  )

  burden_summary <- dplyr::summarise(
    burden_long,
    value_millions = sum(.data$value) / 1e6,
    .by = c("scenario", "burden_outcome", "age")
  )

  burden_summary
}

#' @name plotting_prep
#' 
#' @param year_max The maximum year to be represented in a subsequent figure.
#' For `prep_plot_burden_decades()`, must be a decade, i.e., multiple of 10.
#'
#' @export
prep_plot_burden_decades <- function(burden, year_max) {
  # TODO: add colnames check
  # TODO: general: make validator for burden data
  checkmate::assert_tibble(burden)

  is_decade <- year_max %% 10 == 0
  if (!is_decade) {
    cli::cli_abort(
      "Expected {.code year_max} to be a multiple of 10, but got value \\
      {.code year_max}."
    )
  }

  last_decade <- year_max - 10

  burden_data <- burden[burden$year <= year_max, ]
  burden_data <- dplyr::mutate(
    burden_data,
    year = pmin(
      .data$year,
      year_max - 1
    ),
    decade = floor(.data$year / 10) * 10,
    decade_label = dplyr::if_else(
      .data$decade == last_decade,
      glue::glue("{.data$decade}-{.data$decade + 10}"),
      glue::glue("{.data$decade}-{.data$decade + 9}")
    )
  )

  burden_data <- tidyr::pivot_longer(
    burden_data,
    {{ burden_outcome_names }},
    names_to = "burden_outcome"
  )

  burden_data <- dplyr::summarise(
    burden_data,
    value_millions = sum(.data$value) / 1e6,
    .by = c("scenario", "burden_outcome", "decade_label")
  )

  burden_data
}

#' @name plotting_prep
#'
#' @export
prep_plot_global_burden <- function(burden) {
  # TODO: add colnames check
  checkmate::assert_tibble(burden)

  nesting_cols <- "burden_outcome"

  burden_long <- tidyr::pivot_longer(
    burden,
    {{ burden_outcome_names }},
    names_to = nesting_cols
  )

  burden_long$value_millions <- burden_long$value / 1e6

  # create a nested tibble with a list column named "burden_data"
  burden_nested <- tidyr::nest(
    burden_long,
    .by = {{ nesting_cols }},
    .key = "burden_data"
  )

  burden_nested
}

#' @name plotting_prep
#'
#' @param coverage WIP. Coverage data.
#'
#' @export
prep_plot_coverage_set <- function(coverage) {
  checkmate::assert_tibble(coverage)

  years <- unique(coverage$year)
  countries <- unique(coverage$country)
  coverage_level <- 0
  delivery <- "none"
  scenario_description <- "No vaccination"

  no_vax <- tidyr::crossing(
    year = years,
    country = countries,
    coverage = coverage_level,
    delivery = delivery,
    scenario_description = scenario_description
  )

  coverage_set <- dplyr::mutate(
    coverage,
    delivery = glue::glue("{.data$vaccine}-{.data$activity_type}")
  )
  cols_to_select <- c(
    "scenario_description",
    "delivery",
    "country",
    "year",
    "coverage"
  )
  coverage_set <- dplyr::select(
    coverage_set,
    {{ cols_to_select }}
  )
  coverage_set <- dplyr::bind_rows(
    coverage_set,
    no_vax
  )

  coverage_set
}

#' @name plotting_prep
#'
#' @param fvp WIP. Data on counts of fully vaccinated persons.
#'
#' @param year_min Minimum year.
#'
#' @export
prep_plot_fvp <- function(fvp, year_min, year_max) {
  checkmate::assert_tibble(fvp)
  checkmate::assert_count(year_min)
  checkmate::assert_count(year_max)

  years <- unique(fvp$year)
  countries <- unique(fvp$country)
  fvps_adjusted <- 0
  scenario_description <- "No vaccination"

  no_vax <- tidyr::crossing(
    year = years,
    country = countries,
    fvps_adjusted = fvps_adjusted,
    scenario_description = scenario_description
  )

  fvp_final <- dplyr::bind_rows(fvp, no_vax)
  fvp_final <- dplyr::filter(
    fvp_final,
    dplyr::between(.data$year, year_min, year_max)
  )
  fvp_final <- dplyr::mutate(
    fvp_final,
    scenario = forcats::fct_relevel(
      .data$scenario_description,
      "No vaccination"
    ) # convert characters to factors and set first level
  )

  # TODO: need to see an example to figure this out
  fvp_final$scenario <- gsub(tolower(fvp$disease[1L]), "", fvp_final$scenario)
  fvp_final$scenario <- gsub("-", " ", fvp_final$scenario, fixed = TRUE)

  # determine scenario order in terms of total adjusted FVPs per scenario
  scenario_order <- names(sort(
    tapply(
      fvp_final$fvps_adjusted,
      fvp_final$scenario,
      sum,
      na.rm = TRUE
    )
  ))

  fvp_final$scenario <- forcats::fct_relevel(fvp_final$scenario, scenario_order)

  fvp_agg <- dplyr::summarise(
    fvp = sum(.data$fvps_adjusted, na.rm = TRUE),
    .groups = c("year", "scenario", "disease")
  )

  fvp_agg
}
