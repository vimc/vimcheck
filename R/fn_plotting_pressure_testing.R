#' Plot significant changes
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_hline facet_wrap facet_grid
#' scale_fill_distiller scale_x_continuous scale_y_continuous labs vars
#' labeller label_wrap_gen
#'
#' @keywords internal
#'
#' @export
plot_sig_diff <- function(df, outcome) {
  # retained here as this is a small df and a small operation
  df$label <- glue::glue(
    "{df$country_name} | {df$vaccine} | {df$activity_type} | {df$year}"
  )

  ggplot(df, aes(x = diff, y = reorder(label, diff), color = modelling_group)) +
    geom_segment(aes(x = 0, xend = diff, y = label, yend = label), size = 1) +
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

plot_diff <- function(
  combined,
  variable,
  group_vars = c("activity_type", "vaccine")
) {
  x_var <- paste0(variable, "_new")
  y_var <- paste0(variable, "_old")
  x_sym <- rlang::sym(x_var)
  y_sym <- rlang::sym(y_var)

  combined <- combined %>%
    filter(!is.na(!!x_sym) & !is.na(!!y_sym))

  n_facets <- combined %>%
    distinct(activity_type, vaccine) %>%
    nrow()

  ncol_dynamic <- case_when(
    n_facets <= 4 ~ 2,
    n_facets <= 9 ~ 3,
    n_facets <= 16 ~ 4,
    n_facets <= 25 ~ 6,
    TRUE ~ 8
  )

  p <- ggplot(combined, aes(x = !!x_sym, y = !!y_sym)) +
    geom_point(alpha = 0.5, colour = "#008080") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    facet_wrap(
      ~ activity_type + vaccine,
      scales = "free",
      ncol = ncol_dynamic
    ) +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw() +
    theme(
      strip.text = element_text(size = 7),
      panel.spacing = unit(0.05, "lines"),
      axis.text = element_text(size = 6.5)
    ) +
    labs(
      title = glue("{variable}: Current vs Previous Report"),
      x = glue("{new} - {variable}"),
      y = glue("{old} - {variable}")
    )

  return(p)
}

plot_modelling_group_variation <- function(df, outcome) {
  ggplot(df) +
    aes(
      fill = as.character(mod_num),
      x = adj_outc,
      y = reorder(vaccine, mean_outc)
    ) +
    geom_density_ridges(
      alpha = 0.5,
      stat = "binline",
      bins = 200,
      draw_baseline = FALSE
    ) +
    facet_grid(. ~ activity_type, scales = "fixed") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", math_format(10^.x))
    ) +
    scale_fill_viridis_d() +
    labs(
      x = paste0(
        "Burden averted (",
        ifelse(outcome == "dalys", "DALYs", outcome),
        ")"
      ),
      y = "Vaccine"
    )
}


#' Gavi plot - future deaths and DALYS averted, 2021-2024
#' (current time window Gavi looking at, can be amended)
plot_vaccine_gavi <- function(df, outcome = "deaths_averted") {
  ggplot(
    df,
    aes(
      x = reorder(disease, yearly_outcome),
      y = yearly_outcome,
      fill = factor(year)
    )
  ) +
    geom_col(position = "dodge") +
    scale_fill_manual(
      values = c(
        "2021" = "#008080",
        "2022" = "#E68424",
        "2023" = "#9573B5",
        "2024" = "#A1D15C"
      )
    ) +
    facet_wrap(~dataset, scales = "free_y") +
    scale_y_continuous(labels = scales::scientific) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Disease", y = paste("Impact -", outcome), fill = "Year")
}

### Gavi Cumulative Plot (modelling group + average)
plot_cumul <- function(df, outcome, disease_filter) {
  p <- ggplot(
    df,
    aes(
      x = year,
      y = value,
      color = modelling_group,
      linetype = line_type
    )
  ) +
    geom_step(direction = "hv", linewidth = 0.7, alpha = 0.9) +
    scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed")) +
    guides(linetype = "none") +
    scale_y_continuous(labels = scales::scientific) +
    theme_minimal() +
    labs(
      x = "Year",
      y = paste("Cumulative", outcome),
      color = "Modelling Group",
      title = paste("Cumulative", outcome, "Over Time –", disease_filter)
    ) +
    theme(legend.position = "bottom")

  return(p)
}
