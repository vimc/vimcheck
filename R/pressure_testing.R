### All functions for pressure testing

# Flexible rounding

# Fix for scenario_type variable being included from 202310 onwards
filter_recent_ts <- function(df, threshold = 202310) {
  touchstone_year <- unique(df$touchstone)

  # TODO: check that touchstone year is 6 digit - can there be more digits?
  ts_number <- str_as_ts_year(touchstone_year) # see R/helpers.R

  if (ts_number >= threshold) {
    df <- dplyr::filter(
      df,
      scenario_type == "default"
    )
  }

  df
}

# Helper for removing excluded diseases post-202110
filter_excluded_diseases_ts <- function(df, threshold = 202110) {
  exclude_dis <- c("Hib", "PCV", "Rota", "JE")

  touchstone_year <- unique(df$touchstone)
  ts_number <- as.numeric(substr(touchstone_year, 1, 6))

  if (ts_number <= threshold) {
    df %>% filter(!disease %in% exclude_dis)
  } else {
    df
  }
}

# Identify duplicates
flag_duplicates <- function(df, key_cols) {
  df %>%
    add_count(across(all_of(key_cols)), name = "n_key") %>%
    filter(n_key > 1)
}

# Identify rows where deaths_averted went from non-NA to NA
comparison_prev <- function(df, prev_dat, outcome) {
  prev_df <- prev_dat %>%
    select(all_of(key_cols), all_of(outcome)) %>%
    rename(outcome_prev = !!sym(outcome))

  current_df <- df %>%
    select(all_of(key_cols), all_of(outcome)) %>%
    rename(outcome_cur = !!sym(outcome))

  result <- prev_df %>%
    inner_join(current_df, by = key_cols) %>%
    filter(!is.na(outcome_prev) & is.na(outcome_cur))

  return(result)
}

# Explore significant changes in key outcomes (i.e. deaths/dalys)
generate_diffs <- function(prev_df, curr_df, interest_cols, key_cols) {
  #fix for erroneous duplicated YF data in 201910 dataset
  prev_df <- prev_df %>%
    {
      if (identical(pars$touchstone_old, "201910")) {
        filter(., !(disease == "YF" & support_type == "other" & coverage == 0))
      } else {
        .
      }
    }

  # Fix for multiple campaigns per year (i.e. not true duplicates) - only applicable for 2019 true non-duplicates.
  add_campaign_id <- function(df, key_cols) {
    df %>%
      group_by(across(all_of(key_cols))) %>%
      mutate(campaign_id = row_number()) %>%
      ungroup()
  }

  prev_df <- add_campaign_id(prev_df, key_cols)
  curr_df <- add_campaign_id(curr_df, key_cols)

  diff_keys <- c(key_cols, "campaign_id")
  cols_needed <- unique(c(diff_keys, interest_cols))

  diff <- diffdf::diffdf(
    prev_df[, cols_needed],
    curr_df[, cols_needed],
    keys = diff_keys
  )

  changes <- setNames(
    lapply(interest_cols, function(v) {
      nm <- paste0("VarDiff_", v)
      if (nm %in% names(diff)) diff[[nm]] else NULL
    }),
    interest_cols
  )

  return(changes)
}

# Generate IQR for key outcomes - for threshold of "significant"
gen_national_iqr <- function(
  df,
  group_cols,
  value_cols,
  prefix = "national_iqr_"
) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      across(
        all_of(value_cols),
        \(x) IQR(x, na.rm = TRUE),
        .names = "{prefix}{.col}"
      ),
      .groups = "drop"
    )
}

## Flag significant changes
flag_large_diffs <- function(
  changes_list,
  iqr_df,
  variable,
  group_cols,
  threshold
) {
  iqr_col <- paste0("national_iqr_", variable)

  changes_list[[variable]] %>%
    mutate(diff = COMPARE - BASE) %>%
    left_join(
      iqr_df %>% select(all_of(group_cols), all_of(iqr_col)),
      by = group_cols
    ) %>%
    mutate(
      flag = abs(diff) > threshold * .data[[iqr_col]] & .data[[iqr_col]] > 0
    ) %>%
    filter(flag) %>%
    select(
      country,
      country_name,
      year,
      vaccine,
      modelling_group,
      activity_type,
      BASE,
      COMPARE,
      diff
    ) %>%
    rename(!!as.character(old) := BASE, !!as.character(new) := COMPARE) %>%
    arrange(desc(diff))
}

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

### Generate combined df
gen_combined_df <- function(prev_dat, df2, interest_cols, key_cols) {
  prev_df <- prev_dat[, interest_cols]
  cur_df <- df2[, interest_cols]

  combined <- full_join(
    prev_df,
    cur_df,
    by = key_cols,
    suffix = c("_old", "_new")
  )

  combined <- combined %>%
    select(
      country,
      country_name,
      disease,
      vaccine,
      activity_type,
      year,
      modelling_group,
      deaths_averted_old,
      deaths_averted_new,
      dalys_averted_old,
      dalys_averted_new
    )
  return(combined)
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

### Subregional v national estimate comparison
compare_national_to_subregional <- function(
  df,
  outcome,
  activity_filter,
  threshold
) {
  df <- df %>%
    filter(activity_type == activity_filter) %>%
    select(all_of(key_cols), subregion, !!outcome)

  results <- purrr::map_dfr(outcome, function(outcome) {
    subregional_summary <- df %>%
      group_by(subregion, disease, activity_type) %>%
      summarise(
        subregional_mean = mean(.data[[outcome]], na.rm = TRUE),
        subregional_iqr = IQR(.data[[outcome]], na.rm = TRUE),
        .groups = "drop"
      )

    national_summary <- df %>%
      select(all_of(key_cols), subregion, !!outcome) %>%
      rename(national_value = !!outcome)

    comparison <- national_summary %>%
      left_join(subregional_summary, by = c("subregion", "disease")) %>%
      mutate(
        outcome = outcome,
        difference = national_value - subregional_mean,
        iqr_score = abs(difference) / subregional_iqr
      )

    dynamic_threshold <- quantile(comparison$iqr_score, 0.99, na.rm = TRUE)

    comparison <- comparison %>%
      mutate(
        flag_iqr = iqr_score > dynamic_threshold & subregional_iqr > 0
      ) %>%
      filter(flag_iqr) %>%
      select(
        country_name,
        vaccine,
        year,
        modelling_group,
        national_value,
        subregional_mean,
        subregional_iqr,
        difference,
        iqr_score
      ) %>%
      arrange(desc(iqr_score))

    comparison
  })

  return(results)
}

### Modelling group variations
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

save_outputs <- function() {
  saveRDS(
    round_numeric(
      missing_in_current %>%
        select(all_of(c(
          "country_name",
          "vaccine",
          "activity_type",
          "year",
          "modelling_group"
        )))
    ),
    "outputs/missing_in_current.rds"
  )
  saveRDS(round_numeric(missing_deaths), "outputs/missing_deaths.rds")
  saveRDS(round_numeric(missing_dalys), "outputs/missing_dalys.rds")
  saveRDS(round_numeric(changes_deaths), "outputs/changes_deaths.rds")
  saveRDS(round_numeric(changes_dalys), "outputs/changes_dalys.rds")
  saveRDS(
    round_numeric(subregional_flags_deaths_camp),
    "outputs/subregional_flags_deaths_camp.rds"
  )
  saveRDS(
    round_numeric(subregional_flags_deaths_rout),
    "outputs/subregional_flags_deaths_rout.rds"
  )
  saveRDS(
    round_numeric(subregional_flags_dalys_camp),
    "outputs/subregional_flags_dalys_camp.rds"
  )
  saveRDS(
    round_numeric(subregional_flags_dalys_rout),
    "outputs/subregional_flags_dalys_rout.rds"
  )
}
