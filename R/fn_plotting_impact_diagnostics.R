#' Create impact diagnostics plots
#'
#' @name plot_impact_diagnostics
#' @rdname plot_impact_diagnostics
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_hline facet_wrap facet_grid
#' scale_fill_distiller scale_x_continuous scale_y_continuous labs vars
#' labeller label_wrap_gen theme geom_segment geom_point
#'
#' @importFrom rlang .data
#'
#' @description
#' Plotting functions for impact diagnostics. See
#' [plotting-preparation functions][plot_prep_impact_diagnostics] for a set of
#' helper functions that prepare impact diagnostics for plotting.
#'
#' @param data A data.frame that gives the
#'
#' @param outcome
#'
#' @export
plot_sig_diff <- function(data, outcome = IMPACT_OUTCOMES) {
  checkmate::assert_tibble(data)
  outcome <- rlang::arg_match(outcome, IMPACT_OUTCOMES)

  # retained here as this is a small df and a small operation
  data$label <- glue::glue(
    "{df$country_name} | {df$vaccine} | {df$activity_type} | {df$year}"
  )

  ggplot(
    df,
    aes(
      .data$diff,
      reorder(.data$label, .data$diff),
      color = .data$modelling_group
    )
  ) +
    geom_segment(
      aes(x = 0, xend = .data$diff, y = .data$label, yend = .data$label),
      size = 1
    ) +
    geom_point(size = 2) +
    labs(
      x = "Difference",
      y = NULL,
      title = glue::glue(
        "Significant Differences in {outcome} by Country, Vaccine, \\
        Activity Type and Year"
      )
    ) +
    theme_vimc(x_text_angle = 0)
}

#' @name plot_impact_diagnostics
#'
#' @param group_vars A single string for the grouping variables. May be any of
#' `"activity_type"` and `"vaccine"`.
#'
#' @param touchstone_old A string for the previous touchstone in
#' format `"YYYYMM"`.
#'
#' @param touchstone_new A string for the current or new touchstone in
#' format `"YYYYMM"`.
#'
#' @export
plot_diff <- function(
  data,
  outcome = IMPACT_OUTCOMES,
  group_vars = IMPACT_GROUP_VARS,
  touchstone_old = DEF_TOUCHSTONE_OLD,
  touchstone_new = DEF_TOUCHSTONE_NEW
) {
  checkmate::assert_tibble(data)
  outcome <- rlang::arg_match(outcome, IMPACT_OUTCOMES)
  group_vars <- rlang::arg_match(group_vars, IMPACT_GROUP_VARS, multiple = TRUE)

  touchstone_old <- validate_ts_year(touchstone_old)
  touchstone_new <- validate_ts_year(touchstone_new)

  x_var <- glue::glue("{outcome}_new")
  y_var <- glue::glue("{outcome}_old")

  # small operations retained
  combined <- dplyr::filter(
    combined,
    dplyr::filter(
      !is.na({{ x_var }}),
      !is.na({{ y_var }})
    )
  )

  # nolint start
  n_facets <- nrow(
    dplyr::distinct(
      combined,
      .data$activity_type,
      .data$vaccine
    )
  )
  # nolint end

  ncol_dynamic <- dplyr::case_when(
    n_facets <= 4 ~ 2,
    n_facets <= 9 ~ 3,
    n_facets <= 16 ~ 4,
    n_facets <= 25 ~ 6,
    TRUE ~ 8
  )

  p <- ggplot(
    combined,
    aes({{ x_var }}, {{ y_var }})
  ) +
    ggplot2::geom_point(alpha = 0.5, colour = COLOUR_VIMC) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    facet_wrap(
      facets = c("activity_type", "vaccine,"),
      scales = "free",
      ncol = ncol_dynamic
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    # TODO: check if VIMC theme okay here
    theme_vimc() +
    theme(
      strip.text = ggplot2::element_text(size = 7),
      panel.spacing = ggplot2::unit(0.05, "lines"),
      axis.text = ggplot2::element_text(size = 6.5)
    ) +
    labs(
      title = glue::glue("{variable}: Current vs Previous Report"),
      x = glue::glue("{new} - {variable}"),
      y = glue::glue("{old} - {variable}")
    )

  p
}

#' @name plot_impact_diagnostics
#'
#' @export
plot_modelling_group_variation <- function(data, outcome) {
  ggplot(data) +
    aes(
      fill = as.character(.data$mod_num),
      x = .data$adj_outc,
      y = reorder(.data$vaccine, .data$mean_outc)
    ) +
    ggridges::geom_density_ridges(
      alpha = 0.5,
      stat = "binline",
      bins = 200,
      draw_baseline = FALSE
    ) +
    facet_grid(cols = ggplot2::vars("activity_type"), scales = "fixed") +
    theme_vimc +
    theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    ) +
    ggplot2::scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    ggplot2::scale_fill_viridis_d() +
    labs(
      x = paste0(
        "Burden averted (",
        ifelse(outcome == "dalys", "DALYs", outcome),
        ")"
      ),
      y = "Vaccine"
    )
}

# Gavi plot - future deaths and DALYS averted, 2021-2024
# (current time window Gavi looking at, can be amended)
#' @name plot_impact_diagnostics
#'
#' @export
plot_vaccine_gavi <- function(df, outcome = "deaths_averted") {
  ggplot(
    df,
    aes(
      x = reorder(.data$disease, .data$yearly_outcome),
      y = .data$yearly_outcome,
      fill = factor(.data$year)
    )
  ) +
    geom_col(position = "dodge") +
    ggplot2::scale_fill_manual(
      values = c(
        "2021" = "#008080",
        "2022" = "#E68424",
        "2023" = "#9573B5",
        "2024" = "#A1D15C"
      )
    ) +
    facet_wrap(~dataset, scales = "free_y") +
    scale_y_continuous(labels = scales::scientific) +
    theme_vimc() +
    theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    labs(x = "Disease", y = paste("Impact -", outcome), fill = "Year")
}

# Gavi Cumulative Plot (modelling group + average)
#' @name plot_impact_diagnostics
#'
#' @export
plot_cumul <- function(df, outcome, disease_filter) {
  p <- ggplot(
    df,
    aes(
      x = .data$year,
      y = .data$value,
      color = .data$modelling_group,
      linetype = .data$line_type
    )
  ) +
    ggplot2::geom_step(direction = "hv", linewidth = 0.7, alpha = 0.9) +
    ggplot2::scale_linetype_manual(
      values = c(solid = "solid", dashed = "dashed")
    ) +
    ggplot2::guides(linetype = "none") +
    scale_y_continuous(labels = scales::scientific) +
    theme_vimc() +
    labs(
      x = "Year",
      y = paste("Cumulative", outcome),
      color = "Modelling Group",
      title = paste("Cumulative", outcome, "Over Time –", disease_filter)
    ) +
    theme(legend.position = "bottom")

  p
}
