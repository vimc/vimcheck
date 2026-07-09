#' Plot central impact estimates by cohort and year
#'
#' Produces faceted plots of central impact estimates for priority countries,
#' stratified either by birth cohort or by year of vaccination.
#' Impact metrics include cases, deaths, DALYs, and YLLs.
#'
#' @param data A tibble containing impact estimates.
#'
#' @param country The country names as a character vector. Defaults to PINE
#' countries.
#'
#' @param burden_type Burden metric used to evaluate impact. burden_type can be:
#' cases, deaths, dalys, yll.
#'
#' @param view Charactar scalar. The way impact is assigned, either by birth
#' cohort ("cohort")  or by year of vaccination ("year").
#'
#' @param title Title of the plot to be rendered. Defaults to `NULL`.
#'
#' @return ggplot object showing central impact estimates
#'
#' @examples
#' impact_data <- eg_impact_2
#'
#' plot_impact(
#'   data = impact_data,
#'   "A",
#'   burden_type = "cases",
#'   title = "Cases averted",
#'   view = "year"
#' )
#'
#' @export
plot_impact <- function(
  data,
  country = PINE,
  burden_type = c("cases", "deaths", "dalys", "yll"),
  view = c("cohort", "year"),
  title = NULL
) {
  required_cols <- c("country", "burden_outcome", "impact", "short_name")

  checkmate::assert_data_frame(
    data,
    min.rows = 1L,
    min.cols = length(required_cols)
  )
  checkmate::assert_names(colnames(data), must.include = required_cols)

  checkmate::assert_character(country, any.missing = FALSE)

  burden_type <- rlang::arg_match(burden_type)
  view <- rlang::arg_match(view)

  checkmate::assert_string(title, null.ok = TRUE)

  # check if country is in data
  if (!all(country %in% data[["country"]])) {
    missing_country <- setdiff(country, data[["country"]]) # nolint used in err
    cli::cli_abort(
      "Impact data `data` expected to have country {.str {missing_country}} \
      but it is missing."
    )
  }

  impact <- dplyr::filter(
    data,
    .data$country %in% country,
    .data$burden_outcome == burden_type,
    .data$impact != 0 # can this be safely written as impact > 0?
  )

  if (nrow(impact) > 0) {
    if (view == "cohort") {
      checkmate::assert_names(names(data), must.include = "birth_cohort")
      x_var <- "birth_cohort"
      x_lab <- "Birth cohort"
    } else {
      checkmate::assert_names(names(data), must.include = view)
      x_var <- view
      x_lab <- "Year"
    }

    ggplot(
      impact,
      aes(
        x = .data[[x_var]],
        y = .data$impact,
        ymin = .data$impact,
        ymax = .data$impact,
        fill = .data$short_name
      )
    ) +
      ggplot2::geom_ribbon(alpha = 0.3) +
      ggplot2::geom_line(aes(colour = .data$short_name), linewidth = 0.5) +
      ggplot2::geom_point(aes(colour = .data$short_name), size = 0.5) +
      # TODO: theme definition may not be right for this plot
      theme_vimc() +
      facet_wrap(ggplot2::vars("country"), scales = "free_y") +
      labs(
        x = x_lab,
        y = glue::glue("{burden_type} averted"),
        title = title
      ) +
      theme(
        legend.position = "bottom",
        legend.key.size = ggplot2::unit(0.5, "cm"),
        legend.key.width = ggplot2::unit(0.3, "cm")
      )
  } else {
    cli::cli_abort(
      "No estimates remaining in the data after filtering for \\
      countries: {.str {country}} and impact != 0 for `burden_type`: \\
      {.str {burden_type}}."
    )
  }
}

#' Plot coverage and fully vaccinated persons (FVPs)
#'
#' Generates plots of routine vaccine coverage and fully vaccinated
#' persons (FVPs) over time for selected countries.
#'
#' @param fvps A data.frame (or class extending it) showing the number of
#' FVPs (fully vaccinated persons) by country, year and scenario/activity type.
#'
#' @param country A character vector of country identifiers, with all
#' identifiers expected to be found in `fvps`. Defaults to PINE countries.
#'
#' @return A named list with two ggplot objects:
#'   \describe{
#'     \item{coverage}{A plot of routine vaccine coverage over time.}
#'     \item{fvps}{A plot of fully vaccinated persons over time.}
#'   }
#'
#' If there is no data on routine vaccination in the dataset, the `coverage`
#' element of the return will be an empty `<ggplot>` object, and a warning is
#' thrown.
#'
#' @examples
#' fvps <- eg_fvps_2
#'
#' plots <- plot_coverage_fvps(fvps, "AGO")
#' plots$coverage
#' plots$fvps
#'
#' @export
plot_coverage_fvps <- function(fvps, country = PINE) {
  required_cols <- c(
    "country",
    "activity_type",
    "scenario_type",
    "vaccine",
    "coverage_adjusted",
    "year",
    "fvps"
  )

  checkmate::assert_data_frame(
    fvps,
    min.rows = 1L,
    min.cols = length(required_cols)
  )
  checkmate::assert_names(colnames(fvps), must.include = required_cols)

  country <- checkmate::assert_character(country, any.missing = FALSE)
  if (!all(country %in% fvps[["country"]])) {
    missing_country <- setdiff(country, fvps[["country"]]) # nolint used in err
    cli::cli_abort(
      "Impact data `fvps` expected to have country {.str {missing_country}} \
      but it is missing."
    )
  }

  # handle FVPs plot
  fvps <- dplyr::filter(fvps, .data$country %in% country)
  cov <- dplyr::filter(fvps, .data$activity_type == "routine")

  fvps <- dplyr::mutate(
    fvps,
    vaccine_delivery = paste(
      .data$scenario_type,
      .data$activity_type,
      sep = "_"
    )
  )
  cols_to_select <- c("country", "vaccine_delivery", "year", "fvps")

  fvps <- dplyr::select(fvps, dplyr::all_of(cols_to_select))

  fvps <- dplyr::group_by(
    fvps,
    .data$country,
    .data$vaccine_delivery,
    .data$year
  )

  fvps <- dplyr::summarise(
    fvps,
    fvps = round(sum(.data$fvps) / 1e6, 2),
    .groups = "drop"
  )

  # handle coverage plot
  cov <- dplyr::mutate(
    cov,
    vaccine_delivery = paste(.data$scenario_type, .data$vaccine, sep = "_"),
    coverage_adjusted = round(.data$coverage_adjusted * 100, 2)
  )

  cols_to_select <- c(
    "country",
    "vaccine_delivery",
    "year",
    "coverage_adjusted"
  )
  cov <- dplyr::select(cov, dplyr::all_of(cols_to_select))
  cov <- dplyr::rename(cov, coverage = "coverage_adjusted")

  if (nrow(cov) > 0) {
    p <- .plot_cov_fvp(
      cov,
      "coverage",
      "Coverage (%)",
      "Routine vaccine coverage"
    )
  } else {
    p <- ggplot()
    cli::cli_warn(
      "There is no routine coverage in the database after filtering for \
      country: {.str {country}}"
    )
  }

  # assumed FVP data always available
  q <- .plot_cov_fvp(fvps, "fvps", "FVPs (in millions)", "FVPs")

  list(coverage = p, fvps = q)
}

#' @keywords internal
.plot_cov_fvp <- function(data, col, ylab, title) {
  ggplot(
    data,
    aes(
      x = .data$year,
      y = .data[[col]],
      fill = .data$vaccine_delivery
    )
  ) +
    geom_point(aes(colour = .data$vaccine_delivery), size = 0.5) +
    theme_vimc() + # TODO: same note above on theme
    facet_wrap(
      ggplot2::vars("country"),
      scales = "free_y"
    ) +
    labs(
      x = "Year",
      y = ylab,
      title = title
    ) +
    theme(
      legend.position = "bottom",
      legend.key.size = ggplot2::unit(0.5, "cm"),
      legend.key.width = ggplot2::unit(0.3, "cm")
    )
}
