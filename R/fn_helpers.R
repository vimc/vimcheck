#' Make data for a no-vaccination scenario
#'
#' @name helpers
#' @rdname helpers
#'
#' @description
#' Helper functions for burden diagnostics.
#'
#' @inheritParams validate_file_dict_template
#'
#' @keywords internal
#'
#' @return
#'
#' - `make_novax_scenario()` returns a tibble with the minimum required column
#' names, and entries corresponding to a 'no-vaccination' scenario for
#' `disease`.
make_novax_scenario <- function(disease) {
  v <- c(
    "novac",
    "No Vaccination",
    glue::glue("{disease}-no-vaccination"),
    "No vaccination",
    "no-vaccination.csv"
  )

  # internal function without input checking
  df_ <- dplyr::tibble(
    variable = file_dict_colnames,
    value = v
  )

  tidyr::pivot_wider(
    df_,
    names_from = "variable"
  )
}

#' Adaptively round numerics
#'
#' @param x A numeric vector.
#'
#' @param large_threshold A single number for the threshold over which numbers
#' are to be considered 'large'.
#'
#' @param small_sigfig A single number for the number of significant digits for
#' 'small' numbers.
#'
#' @param large_digits A single number for the number of places to which 'large'
#' numbers should be rounded.
#'
#' @return `x` rounded to either `large_digits` or to `small_sigfig`.
#'
#' @keywords internal
adaptive_round <- function(
  x,
  large_threshold = 1,
  small_sigfig = 2,
  large_digits = 1
) {
  # basic checks for numeric
  checkmate::assert_numeric(x, finite = TRUE, any.missing = FALSE)
  checkmate::assert_number(large_threshold)
  checkmate::assert_count(small_sigfig, positive = TRUE)
  checkmate::assert_count(large_digits, positive = TRUE)

  ifelse(
    abs(x) >= large_threshold,
    round(x, large_digits),
    signif(x, small_sigfig)
  )
}

#' Round numeric columns of a data.frame
#'
#' @param df A data.frame.
#'
#' @keywords internal
round_numeric <- function(df) {
  checkmate::assert_data_frame(
    df,
    min.rows = 1L,
    min.cols = 1L
  )

  dplyr::mutate(
    df,
    dplyr::across(
      .cols = dplyr::where(is.numeric) &
        !dplyr::matches("year", ignore.case = TRUE),
      .fns = adaptive_round
    )
  )
}

#' Check and return touchstone year-month
#'
#' @param x A string for the touchstone identifier.
#'
#' @return The first 6 characters of `x` converted to a numeric. Also has side
#' effects of erroring if conditions on `x` are not met.
#'
#' @keywords internal
validate_ts_year <- function(x) {
  has_n_chars <- checkmate::test_string(
    x,
    min.chars = N_TS_MIN_CHARS
  )
  if (!has_n_chars) {
    n_chars <- nchar(x) # nolint used in cli
    cli::cli_abort(
      "Touchstone year should be a string with at least {N_TS_MIN_CHARS} \
      characters, but got class {.cls {class(x)}} with {n_chars} characters."
    )
  }

  inferred_year <- as.numeric(substr(x, 1, N_TS_YEAR_CHARS))
  is_good_year <- checkmate::test_number(
    inferred_year,
    lower = MIN_TS_YEAR,
    upper = MAX_TS_YEAR,
    finite = TRUE
  )

  if (!is_good_year) {
    cli::cli_abort(
      "Touchstone year string has an inferred year of \
      {.strong {inferred_year}} but expected an year in the range \
      [{MIN_TS_YEAR}, {MAX_TS_YEAR}]."
    )
  }

  inferred_month <- as.numeric(
    substr(x, N_TS_YEAR_CHARS + 1, N_TS_YEAR_CHARS + 2)
  )
  is_good_month <- checkmate::test_number(
    inferred_month,
    lower = MIN_TS_MONTH,
    upper = MAX_TS_MONTH,
    finite = TRUE
  )

  if (!is_good_month) {
    cli::cli_abort(
      "Touchstone month string has an inferred month of \
      {.strong {inferred_month}} but expected an month in the range \
      [{MIN_TS_MONTH}, {MAX_TS_MONTH}]."
    )
  }

  # return year-month as numeric
  substr(x, 1, N_TS_MIN_CHARS)
}

#' Add campaign id to dataframe
#'
#' @param df A data.frame.
#'
#' @param key_cols A character vector of columns in `df` by which the data are
#' to be grouped.
#'
#' @return `df` with a campaign identifier as a numeric.
#'
#' @keywords internal
add_campaign_id <- function(df, key_cols) {
  checkmate::assert_data_frame(df)
  checkmate::assert_character(key_cols, any.missing = FALSE)

  has_cols <- checkmate::test_names(
    names(df),
    must.include = key_cols
  )
  if (!has_cols) {
    missing_cols <- setdiff(colnames(df), key_cols) # nolint used in cli
    cli::cli_abort(
      "Expected {.code df} to have columns {.str {key_cols}} but columns \
    {.str {missing_cols}} are missing."
    )
  }

  df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(key_cols)))
  df <- dplyr::mutate(df, campaign_id = dplyr::row_number())

  dplyr::ungroup(df)
}
