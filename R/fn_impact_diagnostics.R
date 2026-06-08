#' Filter data for touchstones or diseases
#'
#' @name filter_impact_data
#' @rdname filter_impact_data
#'
#' @description
#' A pair of helper functions allowing filtering out of recent touchstone values
#' and excluded diseases.
#'
#' @param df A `<data.frame>` holding impact data. This data.frame is not
#' checked for contents
#'
#' @param threshold A six-digit number that is checked as a valid touchstone
#' identifier (YYYYMM format) using [validate_ts_year()]. Defaults to
#' [DEF_TOUCHSTONE_NEW] (`"202310"`).
#'
#' @keywords impact_diagnostics
#'
#' @return A filtered `<data.frame>`.
#'
#' - `filter_recent_ts()` returns `df` with rows where the touchstone condition
#' is not met excluded.
#'
#' - `filter_excluded_diseases_ts()` returns `df` with rows where rows relating
#' to the [EXCLUDED_DISEASES], when the touchstone year in `df` is less than the
#' `threshold`, excluded.
#'
#' - `flag_duplicates()` returns `df` with duplicated combinations of
#' `key_cols` flagged using the column `n_key` (or a user-defined name).
#'
#' - `filter_invalid_trajectories()` returns `df` with bad outcome trajectories
#' (`NA` to non-`NA`) removed.
#'
#' @export
filter_recent_ts <- function(df, threshold = DEF_TOUCHSTONE_NEW) {
  # NOTE: exact min cols to be updated - fn implies at least 2
  checkmate::assert_data_frame(df, min.rows = 1L, min.cols = 2L)
  checkmate::assert_names(
    names(df),
    must.include = "touchstone"
  )
  threshold <- validate_ts_year(threshold) # apply same rule as data ts year

  touchstone_year <- unique(df[["touchstone"]])

  ts_number <- validate_ts_year(touchstone_year) # see R/helpers.R

  df <- tibble::as_tibble(df)

  # NOTE: consider converting to Date and checking - numeric comparison
  # works okay for now
  if (ts_number >= threshold) {
    dplyr::filter(
      df,
      .data$scenario_type == "default"
    )
  } else {
    df
  }
}

#' @name filter_impact_data
#'
#' @export
filter_excluded_diseases_ts <- function(
  df,
  threshold = DEF_TOUCHSTONE_OLD_OLD
) {
  checkmate::assert_data_frame(df, min.rows = 1L, min.cols = 1L)
  checkmate::assert_names(
    names(df),
    must.include = "touchstone"
  )

  threshold <- validate_ts_year(threshold)

  touchstone_year <- unique(df$touchstone)
  ts_number <- validate_ts_year(touchstone_year)

  df <- tibble::as_tibble(df)

  if (ts_number <= threshold) {
    dplyr::filter(df, !.data$disease %in% EXCLUDED_DISEASES)
  } else {
    df
  }
}

#' @name filter_impact_data
#'
#' @param key_cols Key columns in `df` to check for duplicates.
#'
#' @export
flag_duplicates <- function(df, key_cols = COLNAMES_KEY_PRESSURE_TEST) {
  checkmate::assert_data_frame(df, min.cols = length(key_cols), min.rows = 1L)
  checkmate::assert_character(key_cols)

  has_cols <- checkmate::test_names(
    colnames(df),
    must.include = key_cols
  )
  if (!has_cols) {
    missing_cols <- setdiff(colnames(df), key_cols) # nolint used in cli
    cli::cli_abort(
      "Expected {.code df} to have columns {.str {key_cols}}, but columns \
    {.str {.strong {missing_cols}}} were missing!"
    )
  }

  # data may have a `burden_outcome` column which should not be counted as
  # a duplicate
  df <- dplyr::add_count(
    df,
    dplyr::across(dplyr::all_of(c(key_cols, "burden_outcome"))),
    name = "n_key"
  )

  if (any(df$n_key > 1)) {
    n_duplicates <- sum(df$n_key > 1) # nolint used below
    cli::cli_warn(
      "{n_duplicates} duplicates found in data; please check for plausibility!"
    )
  }

  tibble::as_tibble(df)
}

#' @name filter_impact_data
#'
#' @param prev_data A `<data.frame>` holding data from a previous touchstone for
#' the same scenarios as `df`.
#'
#' @param outcome A string giving the outcome of interest; may be one of
#' `"deaths_averted"` or `"dalys_averted"`.
#'
#' @export
filter_invalid_trajectories <- function(
  df,
  prev_data,
  outcome = c("deaths_averted", "dalys_averted")
) {
  checkmate::assert_data_frame(df, min.cols = 1L, min.rows = 1L)

  checkmate::assert_data_frame(
    prev_data,
    min.rows = nrow(df)
  )

  outcome <- rlang::arg_match(outcome)

  has_cols <- checkmate::test_names(
    colnames(df),
    must.include = c(COLNAMES_KEY_PRESSURE_TEST, outcome)
  )
  if (!has_cols) {
    missing_cols <- setdiff(colnames(df), COLNAMES_KEY_PRESSURE_TEST) # nolint
    cli::cli_abort(
      "Expected {.code df} to have columns \
      {.str {COLNAMES_KEY_PRESSURE_TEST}}, but columns \
      {.str {.strong {missing_cols}}} were missing!"
    )
  }

  has_cols <- checkmate::test_names(
    colnames(prev_data),
    must.include = c(COLNAMES_KEY_PRESSURE_TEST, outcome)
  )
  if (!has_cols) {
    missing_cols <- setdiff(colnames(prev_data), COLNAMES_KEY_PRESSURE_TEST)
    cli::cli_abort(
      "Expected {.code prev_data} to have columns \
      {.str {COLNAMES_KEY_PRESSURE_TEST}}, but columns \
      {.str {.strong {missing_cols}}} were missing!"
    )
  }

  prev_df <- dplyr::select(
    prev_data,
    dplyr::all_of(COLNAMES_KEY_PRESSURE_TEST),
    dplyr::all_of(outcome)
  )
  prev_df <- dplyr::rename(prev_df, outcome_prev = {{ outcome }})

  current_df <- dplyr::select(
    df,
    dplyr::all_of(COLNAMES_KEY_PRESSURE_TEST),
    dplyr::all_of(outcome)
  )
  current_df <- dplyr::rename(current_df, outcome_cur = {{ outcome }})

  result <- dplyr::inner_join(
    prev_df,
    current_df,
    by = COLNAMES_KEY_PRESSURE_TEST
  )

  # `,` replaces `&` for dplyr syntax
  result <- dplyr::filter(
    result,
    !is.na(.data$outcome_prev),
    is.na(.data$outcome_cur)
  )

  tibble::as_tibble(result)
}

#' Explore significant changes in deaths and DALYs
#'
#' @param prev_df A `<data.frame>` of impact estimates from the previous
#' touchstone.
#'
#' @param curr_df A `<data.frame>` of impact estimates for the current
#' touchstone.
#'
#' @param interest_cols A character vector of columns to check for differences.
#' Defaults to [COLNAMES_INTEREST_PRESSURE_TEST].
#'
#' @param key_cols A character vector of columns to use when assigning campaign
#' identifiers. Passed to [add_campaign_id()], defaults to
#' [COLNAMES_KEY_PRESSURE_TEST].
#'
#' @param touchstone A six character string that can be converted to a six digit
#' numeric giving a touchstone identifier in `YYYYMM` format.
#'
#' @return A list of tibbles of differences between `prev_df` and `curr_df`,
#' with one list element per element of `interest_cols`.
#'
#' @keywords impact_diagnostics
#'
#' @export
generate_diffs <- function(
  prev_df,
  curr_df,
  interest_cols = COLNAMES_INTEREST_PRESSURE_TEST,
  key_cols = COLNAMES_KEY_PRESSURE_TEST,
  touchstone = DEF_TOUCHSTONE_OLD
) {
  checkmate::assert_data_frame(
    prev_df,
    min.rows = 1L,
    min.cols = length(interest_cols)
  )
  checkmate::assert_data_frame(
    curr_df,
    min.rows = 1L,
    min.cols = length(interest_cols)
  )

  checkmate::assert_character(interest_cols, min.len = 1L)
  checkmate::assert_character(key_cols, min.len = 1L)

  # check interest cols in dfs. key cols are check in `add_campaign_id`
  checkmate::assert_names(
    colnames(prev_df),
    must.include = c(interest_cols, "support_type", "coverage")
  )
  checkmate::assert_names(
    colnames(curr_df),
    must.include = c(interest_cols, "support_type", "coverage")
  )

  touchstone <- validate_ts_year(touchstone)

  # fix for erroneous duplicated YF data in 201910 dataset
  if (touchstone == DEF_TOUCHSTONE_OLD) {
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
  cols_needed <- union(diff_keys, interest_cols)

  df_diff <- diffdf::diffdf(
    prev_df[, cols_needed],
    curr_df[, cols_needed],
    keys = diff_keys
  )

  # diffdf's Vardiff_* returns a tibble, no need to convert
  changes <- stats::setNames(
    lapply(interest_cols, function(v) {
      nm <- glue::glue("VarDiff_{v}")
      if (nm %in% names(df_diff)) df_diff[[nm]] else NULL
    }),
    interest_cols
  )

  changes # a list of tibbles
}

#' Generate IQR for key outcomes
#'
#' @keywords impact_diagnostics
#'
#' @param df A data.frame of impact estimates.
#'
#' @param group_cols A character vector of grouping columns. Defaults to
#' "country", "vaccine", "activity_type".
#'
#' @param value_cols A character vector of value columns. Defaults to
#' "deaths_averted" and "dalys_averted".
#'
#' @param prefix A string for the prefix applied to every IQR summary column.
#' Defaults to "national_iqr".
#'
#' @return A `<data.frame>` with the inter-quartile range of the columns
#' in `value_cols`, with the column name constructed as `{prefix}_{value_col}`
#' using string interpolation.
#'
#' @export
gen_national_iqr <- function(
  df,
  group_cols = c("country", "vaccine", "activity_type"),
  value_cols = c("deaths_averted", "dalys_averted"),
  prefix = "national_iqr"
) {
  checkmate::assert_data_frame(
    df,
    min.rows = 1L,
    min.cols = length(c(group_cols, value_cols))
  )
  checkmate::assert_character(group_cols, min.len = 1L, any.missing = FALSE)

  # NOTE: restricting value columns to deaths and dalys averted
  checkmate::assert_subset(
    value_cols,
    c("deaths_averted", "dalys_averted")
  )

  checkmate::assert_string(prefix)

  checkmate::assert_names(
    colnames(df),
    must.include = union(group_cols, value_cols)
  )

  df <- tibble::as_tibble(df)

  # long-winded syntax to pass grouping variables as char vec
  df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group_cols)))
  df <- dplyr::summarise(
    df,
    dplyr::across(
      dplyr::all_of(value_cols),
      function(x) {
        stats::IQR(x, na.rm = TRUE)
      },
      .names = "{prefix}_{.col}"
    ),
    .groups = "drop"
  )

  df
}

#' Flag significant changes in impact estimates
#'
#' @description Calculates and flags whether the difference in impact estimates
#' between touchstones is greater than expected. A row is flagged if the
#' difference is greater than `threshold` \eqn{\times} the inter-quartile range
#' for cases where the IQR is greater than zero.
#'
#' @param changes_list A list of data.frames with one element per variable of
#' interest (see `variable`). Usually generated using [generate_diffs()].
#'
#' @param iqr_df A data.frame of inter-quartile differences generated using
#' [gen_national_iqr()].
#'
#' @param variable A string specifying the variable of interest. Must be one of
#' "deaths_averted" or "dalys_averted", and must be present as a name and
#' element of `changes_list`.
#'
#' @inheritParams gen_national_iqr
#'
#' @param threshold A single numeric value for the IQR multiplier. Defaults to
#' 100.
#'
#' @param touchstone_old The previous touchstone identifier. Defaults to
#' [DEF_TOUCHSTONE_OLD_OLD].
#'
#' @param touchstone_new The new touchstone identifier. Defaults to
#' [DEF_TOUCHSTONE_NEW].
#'
#' @return A filtered data.frame of differences in impact estimates flagged
#' as too large. Rows with differences within tolerance are removed.
#'
#' @keywords impact_diagnostics
#'
#' @export
flag_large_diffs <- function(
  changes_list,
  iqr_df,
  variable = c("deaths_averted", "dalys_averted"),
  group_cols = c("country", "vaccine", "activity_type"),
  threshold = 100,
  touchstone_old = DEF_TOUCHSTONE_OLD_OLD,
  touchstone_new = DEF_TOUCHSTONE_NEW
) {
  checkmate::assert_list(changes_list, c("data.frame", "NULL"))
  checkmate::assert_data_frame(
    iqr_df,
    min.rows = 1L,
    min.cols = length(group_cols)
  )

  variable <- rlang::arg_match(variable)
  checkmate::assert_character(group_cols, min.len = 1L, any.missing = FALSE)

  checkmate::assert_number(threshold, lower = 1.0, finite = TRUE)

  touchstone_old <- validate_ts_year(touchstone_old)
  touchstone_new <- validate_ts_year(touchstone_new)

  # cross checking
  has_var <- variable %in% names(changes_list)
  if (!has_var) {
    cli::cli_abort(
      "Expected list {.code changes_list} to have an element with the name \
    {.str {variable}}, but it does not."
    )
  }
  df_compare <- tibble::as_tibble(changes_list[[variable]])

  checkmate::assert_names(
    colnames(df_compare),
    must.include = group_cols
  )
  checkmate::assert_names(
    colnames(iqr_df),
    must.include = group_cols
  )

  iqr_col <- glue::glue("national_iqr_{variable}")

  df_compare <- dplyr::mutate(
    df_compare,
    diff = .data$COMPARE - .data$BASE
  )

  iqr_df <- dplyr::select(
    iqr_df,
    dplyr::all_of(group_cols),
    dplyr::all_of(iqr_col)
  )

  df_compare <- dplyr::left_join(
    df_compare,
    iqr_df,
    by = group_cols
  )

  df_compare <- dplyr::mutate(
    df_compare,
    flag = abs(.data$diff) > threshold * .data[[iqr_col]] & .data[[iqr_col]] > 0
  )

  df_compare <- dplyr::filter(df_compare, .data$flag)

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

  df_compare <- dplyr::select(
    df_compare,
    {{ cols_to_select }}
  )

  rename_lookup <- c("BASE", "COMPARE")
  names(rename_lookup) <- c(
    as.character(touchstone_old),
    as.character(touchstone_new)
  )
  df_compare <- dplyr::rename(
    df_compare,
    dplyr::all_of(rename_lookup)
  )

  df_compare <- dplyr::arrange(df_compare, dplyr::desc(diff))

  df_compare
}

#' Combine and align data from two touchstones
#'
#' @description
#' Generates a full join of two data.frames, selecting for columns of interest.
#'
#' @param prev_dat A data.frame of impact estimates corresponding to an earlier
#' touchstone.
#'
#' @param df_clean A data.frame of impact estimates corresponding to a more
#' recent touchstone.
#'
#' @param interest_cols A character vector of columns of interest. Defaults to
#' [COLNAMES_INTEREST_PRESSURE_TEST].
#'
#' @param key_cols A character vector of columns of interest. Defaults to
#' [COLNAMES_KEY_PRESSURE_TEST].
#'
#' @return A data.frame which is a full join of `prev_dat` and `df_clean`.
#' Columns are disambiguated with the suffixes `"_old"` and `"_new"`.
#'
#' @keywords impact_diagnostics
#'
#' @export
gen_combined_df <- function(
  prev_dat,
  df_clean,
  interest_cols = COLNAMES_INTEREST_PRESSURE_TEST,
  key_cols = COLNAMES_KEY_PRESSURE_TEST
) {
  checkmate::assert_data_frame(
    prev_dat,
    min.cols = 1L,
    min.rows = 1L
  )

  checkmate::assert_subset(
    interest_cols,
    COLNAMES_INTEREST_PRESSURE_TEST
  )
  checkmate::assert_subset(
    key_cols,
    COLNAMES_KEY_PRESSURE_TEST
  )

  cols_to_select <- c(
    "country",
    "country_name",
    "disease",
    "vaccine",
    "activity_type",
    "year",
    "modelling_group",
    "deaths_averted_old",
    "deaths_averted_new",
    "dalys_averted_old",
    "dalys_averted_new"
  )

  checkmate::assert_names(
    colnames(prev_dat),
    must.include = c(interest_cols, key_cols)
  )
  checkmate::assert_names(
    colnames(df_clean),
    must.include = c(interest_cols, key_cols)
  )

  prev_df <- dplyr::select(prev_dat, {{ interest_cols }})
  cur_df <- dplyr::select(df_clean, {{ interest_cols }})

  combined <- dplyr::full_join(
    prev_df,
    cur_df,
    by = key_cols,
    suffix = c("_old", "_new")
  )

  combined <- tibble::as_tibble(combined)

  checkmate::assert_names(
    colnames(combined),
    must.include = cols_to_select
  )

  combined <- dplyr::select(
    combined,
    dplyr::all_of(cols_to_select)
  )

  combined
}

#' Compare sub-regional and national estimates
#'
#' @param df A data.frame with sub-region level data on vaccination impact
#' outcomes.
#'
#' @param outcome A string for the outcome of interest. May be one of
#' `"deaths_averted_rate"` or `"dalys_averted_rate"`.
#'
#' @param activity_filter A string for the type of vaccination activity. May be
#' one of `"campaign"` or `"routine"`.
#'
#' @return A data.frame of sub-regional vaccination impact estimates where the
#' impact is considered to be outside the tolerance limit.
#'
#' @keywords impact_diagnostics
#'
#' @export
compare_natl_subreg <- function(
  df,
  outcome = c("deaths_averted_rate", "dalys_averted_rate"),
  activity_filter = c("campaign", "routine")
) {
  checkmate::assert_data_frame(
    df,
    min.rows = 1L,
    min.cols = length(
      c(outcome, "subregion", COLNAMES_KEY_PRESSURE_TEST)
    )
  )
  outcome <- rlang::arg_match(outcome)
  activity_filter <- rlang::arg_match(activity_filter)

  checkmate::assert_names(
    names(df),
    must.include = c(outcome, "subregion", COLNAMES_KEY_PRESSURE_TEST)
  )

  df <- tibble::as_tibble(df)

  df <- dplyr::filter(df, .data$activity_type == activity_filter)
  df <- dplyr::select(
    df,
    dplyr::all_of(COLNAMES_KEY_PRESSURE_TEST),
    "subregion",
    !!outcome
  )

  # first get national summary
  national_summary <- dplyr::select(
    df,
    dplyr::all_of(COLNAMES_KEY_PRESSURE_TEST),
    "subregion",
    !!outcome
  )
  national_summary <- dplyr::rename(
    national_summary,
    national_value = !!outcome
  )

  # next get sub-regional summary
  subregional_summary <-
    dplyr::group_by(df, .data$subregion, .data$disease, .data$activity_type)

  subregional_summary <- dplyr::summarise(
    subregional_summary,
    subregional_mean = mean(.data[[outcome]], na.rm = TRUE),
    subregional_iqr = stats::IQR(.data[[outcome]], na.rm = TRUE),
    .groups = "drop"
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

  cols_to_select <- c(
    "country_name",
    "vaccine",
    "year",
    "modelling_group",
    "national_value",
    "subregional_mean",
    "subregional_iqr",
    "difference",
    "iqr_score"
  )
  comparison <- dplyr::select(comparison, {{ cols_to_select }})
  comparison <- dplyr::arrange(comparison, dplyr::desc(.data$iqr_score))

  comparison
}

#' Save pressure-testing diagnostics to local file
#'
#' @description
#' Save pressure-testing diagnostics data.frames to local compressed files in
#' the `.Rds` format. Input data.frames are generated by other package functions
#' and are not checked here.
#'
#' @param missing_in_current A data.frame.
#'
#' @param missing_deaths A data.frame that is the output of
#' [filter_invalid_trajectories()] with the outcome `"deaths_averted"`.
#'
#' @param missing_dalys A data.frame that is the output of
#' [filter_invalid_trajectories()] with the outcome `"dalys_averted"`.
#'
#' @param changes_deaths A data.frame that is the output of [flag_large_diffs()]
#' with the outcome `"deaths_averted"`.
#'
#' @param changes_dalys A data.frame that is the output of [flag_large_diffs()]
#' with the outcome `"dalys_averted"`.
#'
#' @param subregional_flags_deaths_camp A data.frame that is the output of
#' [compare_natl_subreg()] with the outcome `"deaths_averted_rate"` for the
#' `"campaign"` activity type.
#'
#' @param subregional_flags_deaths_rout A data.frame that is the output of
#' [compare_natl_subreg()] with the outcome `"deaths_averted_rate"` for the
#' `"routine"` activity type.
#'
#' @param subregional_flags_dalys_camp A data.frame that is the output of
#' [compare_natl_subreg()] with the outcome `"dalys_averted_rate"` for the
#' `"campaign"` activity type.
#'
#' @param subregional_flags_dalys_rout A data.frame that is the output of
#' [compare_natl_subreg()] with the outcome `"dalys_averted_rate"` for the
#' `"campaign"` activity type.
#'
#' @param output_dir A writeable directory. Defaults to "./outputs".
#'
#' @return None. Called for the convenience side-effect of saving data.frames as
#' `.Rds` format.
#'
#' @keywords impact_diagnostics
#'
#' @export
save_outputs <- function(
  missing_in_current,
  missing_deaths,
  missing_dalys,
  changes_deaths,
  changes_dalys,
  subregional_flags_deaths_camp,
  subregional_flags_deaths_rout,
  subregional_flags_dalys_camp,
  subregional_flags_dalys_rout,
  output_dir = here::here("outputs")
) {
  # NOTE: not checking most input args as these are generated from other pkg fns

  output_dir_exists <- dir.exists(output_dir)
  if (!output_dir_exists) {
    cli::cli_abort(
      "Expected output directory {.arg {output_dir}} but it does not exist!"
    )
  }

  # NOTE: consider writing to agnostic format e.g. CSV
  missing_in_current <- dplyr::select(
    missing_in_current,
    {{ colnames_df_missing_cols }}
  )

  filenames <- c(
    "missing_in_current",
    "missing_deaths",
    "missing_dalys",
    "changes_deaths",
    "changes_dalys",
    "subregional_flags_deaths_camp",
    "subregional_flags_deaths_rout",
    "subregional_flags_dalys_camp",
    "subregional_flags_dalys_rout"
  )

  df_list <- list(
    missing_in_current,
    missing_deaths,
    missing_dalys,
    changes_deaths,
    changes_dalys,
    subregional_flags_deaths_camp,
    subregional_flags_deaths_rout,
    subregional_flags_dalys_camp,
    subregional_flags_dalys_rout
  )

  Map(
    df_list,
    filenames,
    f = function(df, name) {
      saveRDS(
        round_numeric(df),
        file.path(output_dir, glue::glue("{name}.Rds"))
      )
    }
  )
}
