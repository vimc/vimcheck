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
