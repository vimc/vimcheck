#' Prepare impact diagnostics for plotting
#'
#' @name plot_prep_impact_diagnostics
#' @rdname plot_prep_impact_diagnostics
#'
#' @description
#' A suite of helper functions that sit between impact diagnostics functions and
#' plotting functions. These functions have some basic checks on the input data,
#' but otherwise assume that users will not modify inputs.
#'
#' @param data A data.frame of impact estimates.
#'
#' @param comparison A data.frame of impact estimates used as a comparator for
#' `comparison`.
#'
#' @param outcome A string for the impact outcome; may be one of
#' "deaths_averted" or "dalys_averted".
#'
#' @return
#'
#' - [prep_plot_mod_grp_varn()] returns a `<tibble>` TODO add
#'
#' @export
prep_plot_mod_grp_varn <- function(df2, df3, outcome = IMPACT_OUTCOMES) {
  # TODO: df2 and df3 need informative names

  offset_manual <- 1e-6
  df_combined <- dplyr::left_join(
    df2,
    df3,
    by = c("modelling_group", "vaccine")
  )

  df_combined <- dplyr::mutate(
    df_combined,
    adj_outc = {{ outcome }} + offset_manual
  )

  df_combined <- dplyr::group_by(
    df_combined,
    .data$vaccine
  )

  df_combined <- dplyr::mutate(
    df_combined,
    mean_outc = stats::weighted.mean(.data$adj_outc, .data$fvps, na.rm = TRUE)
  )

  df_combined
}

#' @name plot_prep_impact_diagnostics
#'
#' @param data description
#'
#' @param outcome
#'
#' @param disease
#'
#' @param touchstone_old
#'
#' @param touchstone_new
#'
#' @return [prep_plot_vax_gavi()] returns a ... TODO add
#'
#' @export
prep_plot_vax_gavi <- function(
  data,
  prev_data,
  outcome = IMPACT_OUTCOMES,
  touchstone_old = DEF_TOUCHSTONE_OLD,
  touchstone_new = DEF_TOUCHSTONE_NEW
) {
  checkmate::assert_tibble(data)
  checkmate::assert_tibble(prev_data)
  outcome <- rlang::arg_match(outcome, IMPACT_OUTCOMES)
  touchstone_old <- validate_ts_year(touchstone_old)
  touchstone_new <- validate_ts_year(touchstone_new)

  df_list <- Map(
    list(data, prev_data),
    list(touchstone_new, touchstone_old),
    f = function(df, ts_id) {
      df <- dplyr::select(
        df,
        dplyr::all_of(COLNAMES_KEY_PRESSURE_TEST),
        {{ outcome }}
      )

      df <- dplyr::filter(
        df,
        dplyr::between(.data$year, 2021, 2024),
        Negate(grepl("COVID", .data$disease, ignore.case = TRUE))
      )

      df <- dplyr::group_by(df, .data$disease, .data$year)

      df <- dplyr::summarise(
        df,
        yearly_outcome = sum({{ outcome }}, na.rm = TRUE),
        .groups = "drop"
      )

      df <- dplyr::mutate(
        df,
        dataset = as.character(ts_id)
      )
    }
  )

  df_combined <- dplyr::bind_rows(df_list)

  df_diff <- Reduce(
    df_list,
    f = function(x, y) {
      dplyr::left_join(
        x,
        y,
        by = c("disease", "year"),
        suffix = c("_curr", "_prev")
      )
    }
  )

  df_diff <- dplyr::mutate(
    df_diff,
    yearly_outcome = .data$yearly_outcome_curr - .data$yearly_outcome_prev,
    dataset = "Difference"
  )
  cols_to_select <- c("disease", "year", "yearly_outcome", "dataset")
  df_diff <- dplyr::select(df_diff, {{ cols_to_select }})

  df_combined <- dplyr::bind_rows(df_combined, df_diff)

  df_combined$dataset <- factor(
    df_combined$dataset,
    levels = c(
      as.character(touchstone_old),
      "Difference",
      as.character(touchstone_new)
    )
  )

  df_combined
}

#' @name plot_prep_impact_diagnostics
#'
#' @param data description
#'
#' @param outcome
#'
#' @param disease
#'
#' @param touchstone_old
#'
#' @param touchstone_new
#'
#' @export
prep_plot_cumul <- function(
  data,
  outcome,
  disease,
  touchstone_old = DEF_TOUCHSTONE_OLD,
  touchstone_new = DEF_TOUCHSTONE_NEW
) {
  outcome_cols <- colnames(data)[stringr::str_detect(
    colnames(data),
    glue::glue("^{outcome}_")
  )]

  cum_col <- glue::glue("cum_{outcome}")
  avg_col <- glue::glue("avg_{outcome}")

  combined2 <- dplyr::select(
    data,
    {{ COLNAMES_KEY_PRESSURE_TEST }},
    {{ outcome_cols }}
  )

  combined2 <- tidyr::pivot_longer(
    combined2,
    cols = dplyr::all_of(outcome_cols),
    names_to = "touchstone",
    values_to = "value"
  )

  combined2 <- dplyr::mutate(
    combined2,
    touchstone = stringr::str_remove(
      .data$touchstone,
      glue::glue("^{outcome}_")
    ),
    touchstone = dplyr::replace_values(
      .data$touchstone,
      c("old", "new"),
      as.character(c(touchstone_old, touchstone_new)),
      .default = .data$touchstone
    ),
    touchstone = factor(
      .data$touchstone,
      levels = as.character(c(touchstone_old, touchstone_new))
    )
  )

  # Cumulative values by modelling group
  df_cum <- dplyr::filter(combined2, .data$disease == disease)
  df_cum <- dplyr::group_by(
    df_cum,
    .data$modelling_group,
    .data$touchstone
  )
  df_cum <- tidyr::complete(
    df_cum,
    year = tidyr::full_seq(.data$year, 1)
  )
  df_cum <- dplyr::arrange(df_cum, .data$year)
  df_cum <- dplyr::mutate(
    df_cum,
    first_valid = min(.data$year[!is.na(data$value)]),
    {{ cum_col }} := dplyr::if_else(
      .data$year < .data$first_valid,
      NA_real_,
      cumsum(tidyr::replace_na(.data$value, 0.0))
    )
  )

  df_cum$first_valid <- NULL
  df_cum <- dplyr::ungroup(df_cum)
  df_cum <- dplyr::mutate(
    df_cum,
    modelling_group = glue::glue("{.data$modelling_group}-{.data$touchstone}")
  )

  # Model average
  df_avg <- dplyr::summarise(
    df_cum,
    {{ avg_col }} := mean({{ cum_col }}, na.rm = TRUE),
    n_models = sum(!is.na({{ cum_col }})),
    .groups = c("year", "touchstone")
  )
  df_avg <- dplyr::filter(
    df_avg,
    .data$n_models >= 1
  )
  df_avg <- dplyr::mutate(
    df_avg,
    modelling_group = glue::glue(
      "Model Average-{.data$touchstone}"
    )
  )

  # Combine for plot
  cols_to_select <- c("year", "modelling_group", "touchstone")
  df_plot <- dplyr::bind_rows(
    dplyr::select(
      df_cum,
      {{ cols_to_select }},
      value = {{ cum_col }}
    ),
    dplyr::select(
      df_avg,
      {{ cols_to_select }},
      value = {{ avg_col }}
    )
  )

  df_plot <- dplyr::group_by(df_plot, .data$modelling_group)
  df_plot <- dplyr::filter(
    df_plot,
    sum(.data$value, na.rm = TRUE) > 0
  )
  df_plot <- dplyr::ungroup(df_plot)
  df_plot <- dplyr::mutate(
    df_plot,
    line_type = dplyr::if_else(
      grepl("Model Average", .data$modelling_group, fixed = TRUE),
      "dashed",
      "solid"
    )
  )

  if (nrow(df_plot) == 0 || all(df_plot$value == 0)) {
    message("No non-zero data to plot for ", disease, ". Skipping plot.")
    return(NULL)
  }

  df_plot
}
