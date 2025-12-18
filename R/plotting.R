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

#' Plot burden and impact diagostics
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
#' @param d
#'
#' @param fig_number
#'
#' @return A `<ggplot>` object that can be printed to screen in the plot frame
#' or saved to an output device (i.e., saved as an image file).
#'
#' @examples
#'
#' @export
plot_compare_demography <- function(d, fig_number) {
  num_countries <- length(unique(d$country))
  names_melting_data <- c(
    "scenario",
    "age",
    "year",
    "expected",
    "provided",
    "difference"
  )
  names_melting_by <- c("scenario", "age", "year")

  # TODO: use tidyr; BIG PICTURE: NO DATA PREP IN PLOTTING FUNCTIONS!
  tot <- reshape2::melt(d[, names_melting_data], id.vars = names_melting_by)
  dat <- tot %>%
    dplyr::group_by(variable, scenario, year, age) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(millions = value / 1e6) %>%
    dplyr::arrange(age)

  # TODO: use .data and .vars, import or namespace functions
  g <- ggplot(data = dat, aes(x = year, y = millions, fill = age)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ scenario + variable, ncol = 3) +
    scale_fill_distiller(palette = "PuOr") +
    geom_hline(yintercept = 0, color = 'red') +
    labs(
      x = "calendar year",
      y = glue::glue("people (in millions"),
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
#' @param burden
#'
#' @export
plot_age_patterns <- function(burden, fig_number) {
  # TODO: REMOVE DATA PREP FROM PLOTTING FNS
  d <- burden %>%
    group_by(scenario, burden_outcome, age) %>%
    summarise(millions = sum(value) / 1e6)

  g <- ggplot(d, aes(x = age, y = millions)) +
    geom_bar(stat = "identity") +
    facet_grid(
      burden_outcome ~ scenario,
      scales = "free_y",
      labeller = labeller(scenario = label_wrap_gen(10))
    ) +
    coord_cartesian(xlim = c(0, max(d$age) + 1)) +
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
#' @param year_max
#'
#' @export
plot_global_burden_decades <- function(burden, year_max, fig_number) {
  # TODO: prefer moving these conditional checks elsewhere
  # TODO: prefer moving data prep outside plotting fn
  stopifnot(year_max %% 10 == 0)
  d <- burden %>%
    filter(year <= year_max) %>%
    mutate(year2 = ifelse(year == year_max, year_max - 1, year)) %>%
    mutate(decade = floor(year2 / 10) * 10) %>%
    mutate(
      decade_label = ifelse(
        decade == year_max - 10,
        paste0(decade, "-", decade + 10),
        paste0(decade, "-", decade + 9)
      )
    ) %>%
    group_by(scenario, burden_outcome, decade_label) %>%
    summarise(millions = sum(value) / 1e6)

  g <- ggplot(d, aes(x = scenario, y = millions, fill = scenario)) +
    geom_bar(stat = "identity") +
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
#' @param d
#'
#' @param outcome
#'
#' @export
plot_global_burden <- function(d, outcome, fig_number) {
  data_ <- dplyr::filter(
    d,
    .data$burden_outcome == outcome
  )

  g <- ggplot(
    data,
    aes(x = year, y = millions, fill = age)
  ) +
    geom_bar(stat = "identity", aes(fill = age)) +
    facet_grid(
      ~scenario,
      scales = "free_y",
      labeller = labeller(scenario = label_wrap_gen(10)) # TODO: avoid magic numbers
    ) +
    scale_fill_distiller(palette = "Spectral") +
    labs(
      x = "calendar year",
      y = paste(outcomes_list[i], "(in millions)"), # TODO: where is outcomes_list!?
      title = glue::glue(
        "Fig. {fig_number}: Global trends of disease burden ({outcome})."
      )
    ) +
    theme_vimc(x_text_angle = 90)

  g
}

#' @name plotting
#'
#' @param cov
#'
#' @export
plot_coverage_set <- function(cov, figure_number) {
  # TODO: remove data prep
  no_vacc <- expand_grid(
    year = unique(cov$year),
    country = unique(cov$country)
  ) %>%
    mutate(
      coverage = 0,
      delivery = "none",
      scenario_description = "No vaccination"
    )

  cov1 <- cov %>%
    mutate(delivery = paste(vaccine, activity_type, sep = "-")) %>%
    select(scenario_description, delivery, country, year, coverage) %>%
    bind_rows(no_vacc)

  g <- ggplot(cov1, aes(x = year, y = coverage, fill = delivery)) +
    geom_bar(
      stat = "identity",
      position = position_dodge(width = 0.9),
      alpha = 0.7
    ) +
    facet_grid(
      "country ~ scenario_description",
      scales = "free_y",
      labeller = labeller(scenario_description = label_wrap_gen(10))
    ) +
    scale_x_continuous(breaks = pretty(unique(cov1$year))) +
    labs(
      x = "calendar year",
      y = "Proportion of target population",
      title = glue::glue(
        "Fig. {figure_number}: Coverage sets for {cov$disease[1]}"
      )
    ) +
    theme_vimc(90)

  g
}

#' @name plotting
#'
#' @param fvp
#'
#' @param year_min
#'
#' @param year_max
#'
#' @export
plot_fvp <- function(fvp, year_min, year_max, figure_number) {
  # TODO: PREFER TO REMOVE DATA PREP CODE
  no_vacc <- expand_grid(
    year = unique(fvp$year),
    country = unique(fvp$country)
  ) %>%
    mutate(fvps_adjusted = 0, scenario_description = "No vaccination")

  fvp_final <- bind_rows(fvp, no_vacc) %>%
    filter(year >= year_min & year <= year_max) %>%
    mutate(scenario = as.factor(scenario_description))

  fvp_final$scenario <- relevel(fvp_final$scenario, "No vaccination")
  fvp_final$scenario <- gsub(tolower(fvp$disease[1L]), "", fvp_final$scenario)
  fvp_final$scenario <- gsub("-", " ", fvp_final$scenario)

  scenario_order <- c(names(sort(tapply(
    fvp_final$fvps_adjusted,
    fvp_final$scenario,
    sum,
    na.rm = TRUE
  ))))

  fvp_final$scenario <- forcats::fct_relevel(fvp_final$scenario, scenario_order)

  fvp_agg <-
    fvp_final %>%
    dplyr::group_by(year, scenario, disease) %>%
    dplyr::summarise(fvp = sum(fvps_adjusted, na.rm = TRUE))

  # TODO: prefer to use a scale transform rather than touching data
  g <- ggplot(fvp_agg, aes(x = year, y = fvp / 1e6, fill = scenario)) +
    geom_bar(
      stat = "identity",
      colour = "midnightblue",
      fill = "midnightblue"
    ) +
    facet_grid(
      ~scenario,
      scales = "free_y",
      labeller = labeller(scenario = label_wrap_gen(10))
    ) +
    scale_x_continuous(breaks = pretty(unique(fvp_agg$year))) +
    ylab(paste("FVP (in millions)")) +
    labs(
      x = "calendar year",
      y = "FVP (in millions)",
      title = glue::glue(
        "Fig. {figure_number}: Fully Vaccinated Persons at global level by \\
        scenario for {fvp$disease[1L]}"
      )
    ) +
    theme_vimc(90, legend.position = "none")

  g
}
