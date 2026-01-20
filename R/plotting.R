#' Plotting theme for vimcheck
#'
#' @description
#' A simple plotting theme building on [ggplot2::theme_bw()].
#'
#'
#' @name plotting_theme
#' @rdname plotting_theme
#'
#' @param x_text_angle
#'
#' @param y_text_angle
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Other arguments passed to
#' [ggplot2::theme()].
#'
#' @return A `ggplot2` theme that can be added to `ggplot2` plots or objects.
#'
#' @keywords plotting
#'
#' @export
theme_vimc <- function(x_text_angle = 45, y_text_angle = 0, ...) {
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = 10,
        angle = x_text_angle
      ),
      strip.text.y = ggplot2::element_text(
        angle = y_text_angle
      ),
      plot.margin = ggplot2::margin(1, 0, 1, 0, "cm"),
      ...
    )
}

#' Plot burden and impact diagnostics
#'
#' @name plotting
#' @rdname plotting
#'
#' @importFrom ggplot2 ggplot
#'
#' @description
#' Plotting functions for burden and impact diagnostics. This documentation
#' holds all plotting functions for now.
#'
#' @param data A `<data.frame>` that gives the comparison between VIMC-provided
#' and modeller-used demography values, in long-format. This is primarily
#' expected to be the output of `check_demography_alignment()` processed by
#' `prep_plot_demography()`.
#'
#' @param fig_number The figure number.
#'
#' @return A `<ggplot>` object that can be printed to screen in the plot frame
#' or saved to an output device (i.e., saved as an image file).
#'
#' @keywords plotting
#'
#' @export
plot_compare_demography <- function(data, fig_number) {
  # NOTE: not implementing a lot of checks on inputs
  checkmate::assert_tibble(data)
  checkmate::assert_count(fig_number, positive = TRUE) # no zeros allowed

  # TODO: use .data and .vars, import or namespace functions
  g <- ggplot(
    data = data,
    aes(x = year, y = millions, fill = age)
  ) +
    geom_col() +
    facet_wrap(~ scenario + variable, ncol = 3) +
    scale_fill_distiller(palette = "PuOr") +
    geom_hline(yintercept = 0, colour = "red") +
    labs(
      x = "calendar year",
      y = "people (in millions",
      title = glue::glue(
        "Fig. {fig_number}. Comparison between interpolated population and \\
        cohort size ({num_countries} countries)."
      )
    ) +
    theme_vimc()

  g
}

#' @name plotting
#'
#' @param burden_age A `<tibble>` with the minimum column names
#' "age", "millions"
#'
#' @export
plot_age_patterns <- function(burden_age, fig_number) {
  checkmate::assert_tibble(burden_age)
  checkmate::assert_count(fig_number, positive = TRUE) # no zeros allowed

  max_age <- max(burden_age$age) + 1

  g <- ggplot(burden_age, aes(x = age, y = value_millions)) +
    geom_col() +
    facet_grid(
      burden_outcome ~ scenario,
      scales = "free_y",
      labeller = labeller(scenario = label_wrap_gen(10))
    ) +
    coord_cartesian(xlim = c(NA_real_, max_age)) + # no lower limit, expect > 0
    labs(
      y = "people (in millions)",
      title = glue::glue(
        "Fig. {fig_number}. Burden age patterns across all countries and years"
      )
    ) +
    theme_vimc()

  g
}

#' @name plotting
#'
#' @param burden_decades A `<tibble>` giving the burden by decade, up to
#' `year_max`. This should be the output of [prep_plot_burden_decades()].
#'
#' @export
plot_global_burden_decades <- function(burden_decades, fig_number) {
  g <- ggplot(
    burden_decades,
    aes(x = scenario, y = millions, fill = scenario)
  ) +
    geom_col() +
    facet_grid(burden_outcome ~ decade_label, scales = "free_y") +
    labs(
      y = "people (in millions)",
      title = glue::glue(
        "Fig. {fig_number}. Global disease burden trends"
      )
    ) +

    # TODO: reconcile with theme_vimc() or make new theme
    theme_bw() %+replace%
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )

  g
}

#' @name plotting
#'
#' @param burden_data This is expected to be a `<tibble>` from a
#' nested-`<tibble>` constructed using [prep_plot_global_burden()].
#'
#' @export
plot_global_burden <- function(burden_data, outcome_name, fig_number) {
  checkmate::assert_tibble(burden_data)
  checkmate::assert_subset(
    outcome_name,
    burden_outcome_names
  )
  checkmate::assert_count(fig_number)

  scenarios_per_gridcol <- 10

  g <- ggplot(
    burden_data,
    aes(year, millions, fill = age)
  ) +
    geom_col() +
    facet_grid(
      ~scenario,
      scales = "free_y",
      labeller = labeller(
        scenario = label_wrap_gen(scenarios_per_gridcol)
      )
    ) +
    scale_fill_distiller(palette = "Spectral") +
    labs(
      x = "calendar year",
      y = glue::glue("{outcome_name} (in millions)"),
      title = glue::glue(
        "Fig. {fig_number}: Global trends of disease burden ({outcome_name})."
      )
    ) +
    theme_vimc(x_text_angle = 90)

  g
}

#' @name plotting
#'
#' @param coverage_set A `<tibble>` that is the output of
#' [prep_plot_coverage_set()].
#'
#' @export
plot_coverage_set <- function(coverage_set, figure_number) {
  checkmate::assert_tibble(coverage_set)
  checkmate::assert_count(figure_number)

  dodge_width <- 0.9
  col_opacity <- 0.7
  scenarios_per_gridcol <- 10

  disease_name <- unique(coverage_set$disease) # expecting a single value

  g <- ggplot(
    coverage_set,
    aes(x = year, y = coverage, fill = delivery)
  ) +
    geom_col(
      # TODO: check if dodging is really needed
      position = position_dodge(dodge_width),
      alpha = col_opacity
    ) +
    facet_grid(
      rows = vars(country),
      cols = vars(scenario_description),
      scales = "free_y",
      labeller = labeller(
        scenario_description = label_wrap_gen(scenarios_per_gridcol)
      )
    ) +
    scale_x_continuous() +
    labs(
      x = "calendar year",
      y = "Proportion of target population",
      title = glue::glue(
        "Fig. {figure_number}: Coverage sets for {disease_name}"
      )
    ) +
    theme_vimc(90)

  g
}

#' @name plotting
#'
#' @param fvp_data A `<tibble>` of estimates of fully-vaccinated persons (FVPs) per
#' scenario, with scenarios as factors in order of the number of adjusted-FVPs.
#' Expected to be the output of [prep_plot_fvp()].
#'
#' @export
plot_fvp <- function(fvp_data, figure_number) {
  checkmate::assert_tibble(fvp_data)
  checkmate::assert_count(figure_number)

  scenarios_per_gridcol <- 10
  scale_millions <- 1e-6
  disease_name <- unique(fvp_data$disease)

  g <- ggplot(
    fvp_data,
    aes(x = year, y = fvp, fill = scenario)
  ) +
    geom_col(
      fill = "midnightblue"
    ) +
    facet_grid(
      ~scenario,
      scales = "free_y",
      labeller = labeller(scenario = label_wrap_gen(scenarios_per_gridcol))
    ) +
    scale_x_continuous() +
    scale_y_continuous(
      labels = scales::label_number(scale = scale_millions)
    ) +
    labs(
      x = "calendar year",
      y = "FVP (in millions)",
      title = glue::glue(
        "Fig. {figure_number}: Fully Vaccinated Persons at global level by \\
        scenario for {disease_name}"
      )
    ) +
    theme_vimc(90, legend.position = "none")

  g
}
