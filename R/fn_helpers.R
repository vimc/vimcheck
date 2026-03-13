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

adaptive_round <- function(
  x,
  large_threshold = 1,
  small_sigfig = 2,
  large_digits = 1
) {
  ifelse(
    abs(x) >= large_threshold,
    round(x, large_digits),
    signif(x, small_sigfig)
  )
}

round_numeric <- function(df) {
  df %>%
    mutate(across(
      where(is.numeric) & !matches("year", ignore.case = TRUE),
      ~ adaptive_round(.x)
    ))
}

str_as_ts_year <- function(x) {
  as.numeric(substr(x, 1, 6))
}

add_campaign_id <- function(df, key_cols) {
  df <- group_by(df, across(all_of(key_cols)))
  df <- mutate(df, campaign_id = row_number())

  ungroup(df)
}
