#' Fix for scenario_type variable being included from 202310 onwards
#'
#' @keywords pressure_testing
#'
#' @export
filter_recent_ts <- function(df, threshold = 202310) {
  touchstone_year <- unique(df$touchstone)

  # TODO: check that touchstone year is 6 digit - can there be more digits?
  ts_number <- str_as_ts_year(touchstone_year) # see R/helpers.R

  if (ts_number >= threshold) {
    dplyr::filter(
      df,
      .data$scenario_type == "default"
    )
  } else {
    df
  }
}

#' Helper for removing excluded diseases post-202110
#'
#' @export
filter_excluded_diseases_ts <- function(df, threshold = 202110) {
  touchstone_year <- unique(df$touchstone)
  ts_number <- str_as_ts_year(touchstone_year)

  if (ts_number <= threshold) {
    dplyr::filter(df, !.data$disease %in% EXCLUDED_DISEASES)
  } else {
    df
  }
}

#' Identify duplicates
#'
#' @export
flag_duplicates <- function(df, key_cols) {
  df <- dplyr::add_count(
    df,
    dplyr::across(dplyr::all_of(key_cols)),
    name = "n_key"
  )

  dplyr::filter(df, .data$n_key > 1)
}

#' Identify rows where deaths_averted went from non-NA to NA
#'
#' @export
comparison_prev <- function(df, prev_data, outcome) {
  prev_df <- dplyr::select(
    prev_data,
    dplyr::all_of(key_cols),
    dplyr::all_of(outcome)
  )
  prev_df <- dplyr::rename(prev_df, outcome_prev = {{ outcome }})

  current_df <- dplyr::select(
    df,
    dplyr::all_of(key_cols),
    dplyr::all_of(outcome)
  )
  current_df <- dplyr::rename(current_df, outcome_cur = {{ outcome }})

  result <- dplyr::inner_join(prev_df, current_df, by = key_cols)
  # `,` replaces `&` for dplyr syntax
  result <- dplyr::filter(result, !is.na(outcome_prev), is.na(outcome_cur))

  result
}

#' Explore significant changes in deaths and DALYs
#'
#' @keywords pressure_testing
#'
#' @export
generate_diffs <- function(
  prev_df,
  curr_df,
  interest_cols,
  key_cols,
  touchstone = TOUCHSTONE_OLD
) {
  # TODO: replace use of `pars$touchstone_old` with arg `touchstone`
  #fix for erroneous duplicated YF data in 201910 dataset
  if (identical(touchstone, TOUCHSTONE_OLD)) {
    prev_df <- dplyr::filter(
      prev_df,
      !(.data$disease == "YF" &
        .data$support_type == "other" &
        .data$coverage == 0)
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

  changes <- stats::setNames(
    lapply(interest_cols, function(v) {
      nm <- glue::glue("VarDiff_{v}")
      if (nm %in% names(diff)) diff[[nm]] else NULL
    }),
    interest_cols
  )

  changes
}

#' Generate IQR for key outcomes
#'
#' @keywords pressure_testing
#'
#' @export
gen_national_iqr <- function(
  df,
  group_cols,
  value_cols,
  prefix = "national_iqr_"
) {
  df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group_cols)))
  df <- dplyr::summarise(
    df,
    dplyr::across(
      dplyr::all_of(value_cols),
      function(x) {
        IQR(x, na.rm = TRUE)
      },
      .names = "{prefix}{.col}"
    ),
    .groups = "drop"
  )
}

#' Flag significant changes
#'
#' @keywords pressure_testing
#'
#' @export
flag_large_diffs <- function(
  changes_list,
  iqr_df,
  variable = c("deaths_averted", "dalys_averted"),
  group_cols,
  threshold
) {
  # TODO: input checking
  variable <- rlang::arg_match(variable)

  iqr_col <- glue::glue("national_iqr_{variable}")

  df <- dplyr::mutate(
    changes_list[[variable]],
    diff = .data$COMPARE - .data$BASE
  )

  iqr_df <- dplyr::select(
    iqr_df,
    dplyr::all_of(group_cols),
    dplyr::all_of(iqr_col)
  )

  df <- left_join(
    df,
    iqr_df,
    by = group_cols
  )

  df <- dplyr::mutate(
    df,
    flag = abs(.data$diff) > threshold * .data[[iqr_col]] & .data[[iqr_col]] > 0
  )

  df <- dplyr::filter(df, .data$flag)

  cols_to_select <- c(
    "country",
    "country_name",
    "year",
    "vaccine",
    "modelling_group",
    "activity_type",
    "BASE",
    "COMPARE",
    "diff"
  )

  df <- dplyr::select(
    df,
    {{ cols_to_select }}
  )

  # TODO: replace `old` and `new` with defined objs --- see scratch.R
  # unsure why this syntax was used
  df <- dplyr::rename(
    df,
    !!as.character(old) := BASE,
    !!as.character(new) := COMPARE
  )

  dplyr::arrange(df, dplyr::desc(diff))
}

#' Generate combined df
#'
#' @keywords pressure_testing
#'
#' @export
gen_combined_df <- function(prev_dat, df2, interest_cols, key_cols) {
  # TODO: input checks
  # TODO: df2 needs a better name
  prev_df <- dplyr::select(prev_dat, {{ interest_cols }})
  cur_df <- dplyr::select(df2, {{ interest_cols }})

  combined <- dplyr::full_join(
    prev_df,
    cur_df,
    by = key_cols,
    suffix = c("_old", "_new")
  )

  dplyr::select(
    combined,
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
}

### Subregional v national estimate comparison
compare_national_to_subregional <- function(
  df,
  outcome,
  activity_filter,
  threshold
) {
  df <- dplyr::filter(df, activity_type == activity_filter)
  df <- dplyr::select(df, dplyr::all_of(key_cols), subregion, !!outcome)

  results <- purrr::map_dfr(outcome, function(otc) {
    subregional_summary <-
      dplyr::group_by(df, subregion, disease, activity_type)

    subregional_summary <- dplyr::summarise(
      subregional_summary,
      subregional_mean = mean(.data[[otc]], na.rm = TRUE),
      subregional_iqr = IQR(.data[[otc]], na.rm = TRUE),
      .groups = "drop"
    )

    national_summary <-
      dplyr::select(df, dplyr::all_of(key_cols), subregion, !!outcome)
    national_summary <- dplyr::rename(
      national_summary,
      national_value = !!outcome
    )

    comparison <- dplyr::left_join(
      national_summary,
      subregional_summary,
      by = c("subregion", "disease")
    )
    comparison <- dplyr::mutate(
      comparison,
      outcome = outcome,
      difference = .data$national_value - .data$subregional_mean,
      iqr_score = abs(.data$difference) / .data$subregional_iqr
    )

    dynamic_threshold <- stats::quantile(
      comparison$iqr_score,
      0.99,
      na.rm = TRUE
    )

    comparison <- dplyr::mutate(
      comparison,
      flag_iqr = .data$iqr_score > dynamic_threshold & .data$subregional_iqr > 0
    )
    comparison <- dplyr::filter(comparison, .data$flag_iqr)
    comparison <- dplyr::select(
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
    comparison <- dplyr::arrange(comparison, dplyr::desc(.data$iqr_score))

    comparison
  })

  results
}

# TODO: reconsider function name, add explicit arguments
#' Modelling group variations
#'
#' @keywords pressure_testing
#'
#' @export
save_outputs <- function() {
  saveRDS(
    round_numeric(
      missing_in_current %>%
        dplyr::select(dplyr::all_of(c(
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
