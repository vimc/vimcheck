## Plot significant changes
significant_diff_plot <- function(df, outcome) {
  df$label <- paste(
    df$country_name,
    df$vaccine,
    df$activity_type,
    df$year,
    sep = " | "
  )

  ggplot(df, aes(x = diff, y = reorder(label, diff), color = modelling_group)) +
    geom_segment(aes(x = 0, xend = diff, y = label, yend = label), size = 1) +
    geom_point(size = 2) +
    labs(
      x = "Difference",
      y = "",
      title = glue(
        "Significant Differences in {outcome} by Country, Vaccine, Activity Type and Year"
      )
    ) +
    theme_minimal()
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

plot_modelling_group_variation <- function(df2, df3, outc = "deaths") {
  offset <- 1e-6

  df2 %>%
    left_join(df3, by = join_by(modelling_group, vaccine)) %>%
    mutate(adj_outc = !!as.name(paste0(outc, "_averted")) + offset) %>%
    group_by(vaccine) %>%
    mutate(mean_outc = weighted.mean(adj_outc, fvps, na.rm = TRUE)) %>%
    ggplot() +
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
        ifelse(outc == "dalys", "DALYs", outc),
        ")"
      ),
      y = "Vaccine"
    )
}


# Gavi plot - future deaths and DALYS averted, 2021-2024 (current time window Gavi looking at, can be amended)
plot_vaccine_gavi <- function(df, prev_dat = NULL, outcome = "deaths_averted") {
  df_cur <- df %>%
    select(all_of(key_cols), !!outcome) %>%
    filter(year >= 2021, year <= 2024, disease != "COVID") %>%
    group_by(disease, year) %>%
    summarise(
      yearly_outcome = sum(.data[[outcome]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(dataset = as.character(new))

  df_prev <- prev_dat %>%
    select(all_of(key_cols), !!outcome) %>%
    filter(year >= 2021, year <= 2024, disease != "COVID") %>%
    group_by(disease, year) %>%
    summarise(
      yearly_outcome = sum(.data[[outcome]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(dataset = as.character(old))

  df_combined <- bind_rows(df_cur, df_prev)

  df_diff <- df_cur %>%
    left_join(
      df_prev,
      by = c("disease", "year"),
      suffix = c("_curr", "_prev")
    ) %>%
    mutate(
      yearly_outcome = yearly_outcome_curr - yearly_outcome_prev,
      dataset = "Difference"
    ) %>%
    select(disease, year, yearly_outcome, dataset)

  df_combined <- bind_rows(df_combined, df_diff)

  df_combined$dataset <- factor(
    df_combined$dataset,
    levels = c(as.character(old), "Difference", as.character(new))
  )

  ggplot(
    df_combined,
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
  outcome_cols <- names(df)[str_detect(names(df), paste0("^", outcome, "_"))]

  outcome_sym <- sym(outcome)
  cum_col <- paste0("cum_", outcome)
  avg_col <- paste0("avg_", outcome)

  col_old <- paste0(outcome, "_old")
  col_new <- paste0(outcome, "_new")

  combined2 <- df %>%
    select(
      country,
      country_name,
      disease,
      vaccine,
      activity_type,
      year,
      modelling_group,
      all_of(outcome_cols)
    ) %>%
    pivot_longer(
      cols = all_of(outcome_cols),
      names_to = "touchstone",
      values_to = "value"
    ) %>%
    mutate(
      touchstone = str_remove(touchstone, paste0("^", outcome, "_")),
      touchstone = recode(
        touchstone,
        "old" = as.character(old),
        "new" = as.character(new),
        .default = touchstone
      ),
      touchstone = factor(
        touchstone,
        levels = c(as.character(old), as.character(new))
      )
    )
  # Cumulative values by modelling group
  df_cum <- combined2 %>%
    filter(disease == disease_filter) %>%
    group_by(modelling_group, touchstone) %>%
    complete(year = full_seq(year, 1)) %>%
    arrange(year) %>%
    mutate(
      first_valid = min(year[!is.na(value)]),
      !!cum_col := ifelse(
        year < first_valid,
        NA,
        cumsum(replace_na(value, 0))
      )
    ) %>%
    select(-first_valid) %>%
    ungroup() %>%
    mutate(modelling_group = paste(modelling_group, touchstone, sep = "-"))

  # Model average
  df_avg <- df_cum %>%
    group_by(year, touchstone) %>%
    summarise(
      !!avg_col := mean(!!sym(cum_col), na.rm = TRUE),
      n_models = sum(!is.na(!!sym(cum_col))),
      .groups = "drop"
    ) %>%
    filter(n_models >= 1) %>%
    mutate(modelling_group = paste("Model Average", touchstone, sep = "-"))

  # Combine for plot
  df_plot <- bind_rows(
    df_cum %>%
      select(year, modelling_group, touchstone, value = !!sym(cum_col)),
    df_avg %>%
      select(year, modelling_group, touchstone, value = !!sym(avg_col))
  )

  df_plot <- df_plot %>%
    group_by(modelling_group) %>%
    filter(sum(value, na.rm = TRUE) > 0) %>%
    ungroup() %>%
    mutate(
      line_type = ifelse(
        grepl("Model Average", modelling_group),
        "dashed",
        "solid"
      )
    )

  if (nrow(df_plot) == 0 || all(df_plot$value == 0)) {
    message("No non-zero data to plot for ", disease_filter, ". Skipping plot.")
    return(NULL)
  }

  p <- ggplot(
    df_plot,
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
