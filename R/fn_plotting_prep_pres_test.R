prep_plot_mod_grp_varn <- function(df2, df3, outc = "deaths") {
  offset <- 1e-6

  df2 %>%
    left_join(df3, by = join_by(modelling_group, vaccine)) %>%
    mutate(adj_outc = !!as.name(paste0(outc, "_averted")) + offset) %>%
    group_by(vaccine) %>%
    mutate(mean_outc = weighted.mean(adj_outc, fvps, na.rm = TRUE))
}

prep_plot_vax_gavi <- function(
  df,
  prev_dat = NULL,
  outcome = "deaths_averted"
) {
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

  df_combined
}

prep_plot_cumul <- function(df, outcome, disease_filter) {
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

  df_plot
}
