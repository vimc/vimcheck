#' Plotting theme for vimcheck
#'
#' @description
#' A simple plotting theme building on [ggplot2::theme_bw()].
#'
#' @name plotting_theme
#' @rdname plotting_theme
#'
#' @param x_text_angle The angle for X-axis labels. Defaults to 45 degrees.
#'
#' @param y_text_angle The angle for Y-axis labels. Defaults to 0 degrees.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Other arguments passed to
#' [ggplot2::theme()]. These will be applied in addition to, or in place of,
#' pre-existing elements defined by this theme. See the examples for this
#' theme's appearance.
#'
#' @return A `ggplot2` theme that can be added to `ggplot2` plots or objects.
#'
#' @keywords plotting
#'
#' @examples
#' # using an inbuilt dataset
#' data(mtcars)
#'
#' # standard theme
#' ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
#'   ggplot2::geom_point() +
#'   theme_vimc()
#'
#' # with X-axis suppression
#' ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
#'   ggplot2::geom_point() +
#'   theme_vimc_noxaxis()
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

#' @name plotting_theme
#'
#' @importFrom ggplot2 '%+replace%'
#'
#' @export
theme_vimc_noxaxis <- function() {
  theme_vimc() %+replace%
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
}

#' Plot burden and impact diagnostics
#'
#' @name plotting
#' @rdname plotting
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_hline facet_wrap facet_grid
#' scale_fill_distiller scale_x_continuous scale_y_continuous labs vars
#' labeller label_wrap_gen
#'
#' @description
#' Plotting functions for burden and impact diagnostics. All functions operate
#' on data prepared for plotting by a corresponding
#' [plotting-preparation function][plotting_prep].
#'
#' @param fig_number The figure number displayed in the plot title.
#'
#' @param data A `<data.frame>` that gives the comparison between VIMC-provided
#' and modeller-used demography values, in long-format. This is expected to be
#' the output of `check_demography_alignment()` processed by
#' `prep_plot_demography()`.
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

  n_cols_allowed <- 3
  num_countries <- length(unique(data$country))

  # TODO: use .data and .vars, import or namespace functions
  g <- ggplot(
    data,
    aes(.data$year, .data$value_millions, fill = .data$age)
  ) +
    geom_col() +
    facet_wrap(vars(.data$scenario, .data$variable), ncol = n_cols_allowed) +
    scale_fill_distiller(palette = "PuOr") +
    geom_hline(yintercept = 0, colour = "red") +
    labs(
      x = "calendar year",
      y = "people (in millions)",
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
#' "age", "value_millions", "burden_outcome", and "scenario"; expected to be the
#' output of [prep_plot_age()].
#'
#' @export
plot_age_patterns <- function(burden_age, fig_number) {
  checkmate::assert_tibble(burden_age)
  checkmate::assert_count(fig_number, positive = TRUE) # no zeros allowed

  max_age <- max(burden_age$age) + 1

  g <- ggplot(burden_age, aes(.data$age, .data$value_millions)) +
    geom_col() +
    facet_grid(
      vars(.data$burden_outcome),
      vars(.data$scenario),
      scales = "free_y",
      labeller = labeller(scenario = label_wrap_gen(10))
    ) +
    # TODO: check if needed if max age comes from data
    ggplot2::coord_cartesian(xlim = c(NA_real_, max_age)) +
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
#' `year_max`; expected to be the output of [prep_plot_burden_decades()].
#'
#' @export
plot_global_burden_decades <- function(burden_decades, fig_number) {
  g <- ggplot(
    burden_decades,
    aes(.data$scenario, .data$value_millions, fill = .data$scenario)
  ) +
    geom_col() +
    facet_grid(
      vars(.data$burden_outcome),
      vars(.data$decade_label),
      scales = "free_y"
    ) +
    labs(
      y = "people (in millions)",
      title = glue::glue(
        "Fig. {fig_number}. Global disease burden trends"
      )
    ) +
    theme_vimc_noxaxis()

  g
}

#' @name plotting
#'
#' @param burden_data This is expected to be a `<tibble>` from a
#' nested-`<tibble>` constructed using [prep_plot_global_burden()].
#' 
#' @param outcome_name A string for an outcome name. Allowed outcome names are
#' given in the package constant [constants][burden_outcome_names].
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
    aes(.data$year, .data$value_millions, fill = .data$age)
  ) +
    geom_col() +
    facet_grid(
      cols = vars(.data$scenario), # this plot facets as cols only, w/ 1 row
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
plot_coverage_set <- function(coverage_set, fig_number) {
  checkmate::assert_tibble(coverage_set)
  checkmate::assert_count(fig_number)

  dodge_width <- 0.9
  col_opacity <- 0.7
  scenarios_per_gridcol <- 10

  disease_name <- unique(coverage_set$disease) # expecting a single value

  g <- ggplot(
    coverage_set,
    aes(.data$year, .data$coverage, fill = .data$delivery)
  ) +
    geom_col(
      # TODO: check if dodging is really needed
      position = ggplot2::position_dodge(dodge_width),
      alpha = col_opacity
    ) +
    facet_grid(
      vars(.data$country),
      vars(.data$scenario_description),
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
        "Fig. {fig_number}: Coverage sets for {disease_name}"
      )
    ) +
    theme_vimc(90)

  g
}

#' @name plotting
#'
#' @param fvp_data A `<tibble>` of estimates of fully-vaccinated persons (FVPs)
#' per scenario, with scenarios as factors in order of the number of
#' adjusted-FVPs. Expected to be the output of [prep_plot_fvp()].
#'
#' @export
plot_fvp <- function(fvp_data, fig_number) {
  checkmate::assert_tibble(fvp_data)
  checkmate::assert_count(fig_number)

  scenarios_per_gridcol <- 10
  scale_millions <- 1e-6
  disease_name <- unique(fvp_data$disease)

  g <- ggplot(
    fvp_data,
    aes(.data$year, .data$fvp)
  ) +
    geom_col(
      fill = "midnightblue"
    ) +
    facet_grid(
      cols = vars(.data$scenario),
      scales = "free_y",
      labeller = labeller(scenario = label_wrap_gen(scenarios_per_gridcol))
    ) +
    scale_y_continuous(
      labels = scales::label_number(scale = scale_millions)
    ) +
    labs(
      x = "calendar year",
      y = "FVP (in millions)",
      title = glue::glue(
        "Fig. {fig_number}: Fully Vaccinated Persons at global level by \\
        scenario for {disease_name}"
      )
    ) +
    theme_vimc(90, legend.position = "none")

  g
}
