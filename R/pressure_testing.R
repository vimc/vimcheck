### All functions for pressure testing

# Flexible rounding

# Fix for scenario_type variable being included from 202310 onwards
filter_recent_ts <- function(df, threshold = 202310) {
  touchstone_year <- unique(df$touchstone)

  # TODO: check that touchstone year is 6 digit - can there be more digits?
  ts_number <- str_as_ts_year(touchstone_year) # see R/helpers.R

  if (ts_number >= threshold) {
    dplyr::filter(
      df,
      scenario_type == "default"
    )
  } else {
    df
  }
}

# Helper for removing excluded diseases post-202110
filter_excluded_diseases_ts <- function(df, threshold = 202110) {
  touchstone_year <- unique(df$touchstone)
  ts_number <- str_as_ts_year(touchstone_year)

  if (ts_number <= threshold) {
    filter(df, !disease %in% exclude_dis)
  } else {
    df
  }
}

# Identify duplicates
flag_duplicates <- function(df, key_cols) {
  df <- dplyr::add_count(
    df,
    dplyr::across(dplyr::all_of(key_cols)),
    name = "n_key"
  )

  filter(df, n_key > 1)
}

# Identify rows where deaths_averted went from non-NA to NA
comparison_prev <- function(df, prev_dat, outcome) {
  prev_df <- select(prev_data, all_of(key_cols), all_of(outcome))
  prev_df <- rename(prev_df, outcome_prev = !!sym(outcome))

  current_df <- select(current_df, all_of(key_cols), all_of(outcome))
  current_df <- rename(current_df, outcome_cur = !!sym(outcome))

  result <- inner_join(prev_df, current_df, by = key_cols)
  result <- filter(result, !is.na(outcome_prev) & is.na(outcome_cur))

  result
}

# Explore significant changes in key outcomes (i.e. deaths/dalys)
generate_diffs <- function(prev_df, curr_df, interest_cols, key_cols) {
  #fix for erroneous duplicated YF data in 201910 dataset
  if (identical(pars$touchstone_old, TOUCHSTONE_OLD)) {
    prev_df <- filter(
      prev_df,
      !(disease == "YF" & support_type == "other" & coverage == 0)
    )
  }

  # Fix for multiple campaigns per year (i.e. not true duplicates)
  # only applicable for 2019 true non-duplicates.
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

  changes
}

# Generate IQR for key outcomes - for threshold of "significant"
gen_national_iqr <- function(
  df,
  group_cols,
  value_cols,
  prefix = "national_iqr_"
) {
  df <- group_by(df, across(all_of(group_cols)))
  df <- summarise(
    df,
    across(
      all_of(value_cols),
      \(x) IQR(x, na.rm = TRUE),
      .names = "{prefix}{.col}"
    ),
    .groups = "drop"
  )
}

# TODO: I don't like how this looks - this should probably be a simpler
# functional that maps over a list in a separate function
## Flag significant changes
flag_large_diffs <- function(
  changes_list,
  iqr_df,
  variable,
  group_cols,
  threshold
) {
  iqr_col <- paste0("national_iqr_", variable)

  # returns a list so that the function can accept multiple variables
  lapply(
    changes_list[[variable]],
    temp_fn,
    iqr_df,
    variable,
    group_cols,
    threshold
  )
}

temp_fn <- function(df, iqr_df, variable, group_cols, threshold) {
  mutate(
    df,
    diff = COMPARE - BASE
  ) %>%
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

### Subregional v national estimate comparison
compare_national_to_subregional <- function(
  df,
  outcome,
  activity_filter,
  threshold
) {
  df <- filter(df, activity_type == activity_filter)
  df <- select(df, all_of(key_cols), subregion, !!outcome)

  results <- purrr::map_dfr(outcome, function(otc) {
    subregional_summary <-
      group_by(df, subregion, disease, activity_type)
    subregional_summary <- summarise(
      subregional_summary,
      subregional_mean = mean(.data[[otc]], na.rm = TRUE),
      subregional_iqr = IQR(.data[[otc]], na.rm = TRUE),
      .groups = "drop"
    )

    national_summary <-
      select(df, all_of(key_cols), subregion, !!outcome)
    national_summary <- rename(national_summary, national_value = !!outcome)

    comparison <- left_join(
      national_summary,
      subregional_summary,
      by = c("subregion", "disease")
    )
    comparison <- mutate(
      comparison,
      outcome = outcome,
      difference = national_value - subregional_mean,
      iqr_score = abs(difference) / subregional_iqr
    )

    dynamic_threshold <- quantile(comparison$iqr_score, 0.99, na.rm = TRUE)

    comparison <- mutate(
      comparison,
      flag_iqr = iqr_score > dynamic_threshold & subregional_iqr > 0
    )
    comparison <- filter(comparison, flag_iqr)
    comparison <- select(
      comparison,
      country_name,
      vaccine,
      year,
      modelling_group,
      national_value,
      subregional_mean,
      subregional_iqr,
      difference,
      iqr_score
    )
    comparison <- arrange(comparison, desc(iqr_score))

    comparison
  })

  results
}

### Modelling group variations
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
