#' Prepare impact diagnostics for plotting
#'
#' @name plot_prep_impact_diagnostics
#' @rdname plot_prep_impact_diagnostics
#'
#' @description
#' A suite of helper functions that sit between impact diagnostics functions and
#' plotting functions. These functions transform and aggregate impact estimates
#' to prepare them for visualisation. Functions have basic checks on input data
#' but otherwise assume users will not modify inputs.
#'
#' @param df2 A `<tibble>` of impact estimates with at least columns
#' `modelling_group`, `vaccine`, outcome variable, and `fvps` (doses
#' delivered). Used as the primary data source for calculations in
#' [prep_plot_mod_grp_varn()].
#'
#' @param df3 A `<tibble>` of modelling group and vaccine combinations,
#' typically with one row per modelling group per vaccine. Joined with `df2`
#' to ensure complete group coverage in [prep_plot_mod_grp_varn()].
#'
#' @param data A `<tibble>` of impact estimates with columns including at least
#' those in [COLNAMES_KEY_PRESSURE_TEST], the outcome variable, and
#' potentially outcome-specific columns (for [prep_plot_cumul()]). Used in
#' [prep_plot_vax_gavi()] and [prep_plot_cumul()].
#'
#' @param prev_data A `<tibble>` of impact estimates from a previous touchstone,
#' used as a comparison baseline in [prep_plot_vax_gavi()]. Should have the
#' same structure as `data`.
#'
#' @param outcome A character string for the impact outcome. Must be one of
#' `"deaths_averted"` or `"dalys_averted"`. For [prep_plot_cumul()],
#' `data` must include columns named `{outcome}_old` and `{outcome}_new`.
#'
#' @param disease A character string specifying a single disease for filtering
#' in [prep_plot_cumul()].
#'
#' @param touchstone_old A six-character touchstone identifier (YYYYMM format)
#' for the previous dataset. Defaults to [DEF_TOUCHSTONE_OLD]. Used in
#' [prep_plot_vax_gavi()] and [prep_plot_cumul()].
#'
#' @param touchstone_new A six-character touchstone identifier (YYYYMM format)
#' for the current dataset. Defaults to [DEF_TOUCHSTONE_NEW]. Used in
#' [prep_plot_vax_gavi()] and [prep_plot_cumul()].
#'
#' @importFrom rlang :=
#'
#' @return
#'
#' - [prep_plot_mod_grp_varn()] returns a grouped `<tibble>` (grouped by
#' `vaccine`) with all columns from `df2` and `df3` plus derived columns:
#' `adj_outc` (adjusted outcome with small offset), `outcome_name` (input
#' outcome), and `mean_outc` (vaccine-level weighted mean outcome).
#'
#' - [prep_plot_vax_gavi()] returns a `<tibble>` with columns `disease`,
#' `year`, `yearly_outcome`, `dataset` (factor with levels for old touchstone,
#' "Difference", and new touchstone), and `outcome_name`. Summarizes outcomes
#' by disease and year across two touchstones.
#'
#' - [prep_plot_cumul()] returns a `<tibble>` with columns `year`,
#' `modelling_group`, `touchstone`, `value` (cumulative or average outcome),
#' `line_type` ("solid" for individual models, "dashed" for model average),
#' and `outcome_name`. Returns `NULL` if the specified disease has no non-zero
#' data to plot.
#'
#' @export
prep_plot_mod_grp_varn <- function(df2, df3, outcome = IMPACT_OUTCOMES) {
  checkmate::assert_tibble(df2, min.rows = 1L, min.cols = 1L)
  checkmate::assert_tibble(df3, min.rows = 1L, min.cols = 1L)

  outcome <- rlang::arg_match(outcome, IMPACT_OUTCOMES)

  offset_manual <- 1e-6
  df_combined <- dplyr::left_join(
    df2,
    df3,
    by = c("modelling_group", "vaccine")
  )

  df_combined <- dplyr::mutate(
    df_combined,
    adj_outc = .data[[outcome]] + offset_manual,
    outcome_name = outcome
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
#' @param data A `<tibble>` of impact estimates with columns including at least
#' those in [COLNAMES_KEY_PRESSURE_TEST], the outcome variable, and
#' potentially other columns for analysis.
#'
#' @param prev_data A `<tibble>` of impact estimates from a previous touchstone,
#' used as a comparison baseline. Should have the same structure as `data`.
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
        dplyr::between(.data$year, 2021, 2024)
      )

      df <- dplyr::filter_out(
        df,
        grepl("COVID", .data$disease, ignore.case = TRUE)
      )

      df <- dplyr::group_by(df, .data$disease, .data$year)

      df <- dplyr::summarise(
        df,
        yearly_outcome = sum(.data[[outcome]], na.rm = TRUE),
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

  df_combined$outcome_name <- outcome

  df_combined
}

#' @name plot_prep_impact_diagnostics
#'
#' @param disease A character string specifying a single disease for filtering
#' and analysis.
#'
#' @export
prep_plot_cumul <- function(
  data,
  outcome,
  disease,
  touchstone_old = DEF_TOUCHSTONE_OLD,
  touchstone_new = DEF_TOUCHSTONE_NEW
) {
  checkmate::assert_tibble(data)
  checkmate::assert_subset(
    outcome,
    IMPACT_OUTCOMES
  )

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
  combined2 <- combined2[combined2$disease == disease, ]

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
      from = c("old", "new"),
      to = as.character(c(touchstone_old, touchstone_new))
    ),
    touchstone = factor(
      .data$touchstone,
      levels = as.character(c(touchstone_old, touchstone_new))
    )
  )

  # Cumulative values by modelling group
  df_cum <- dplyr::group_by(
    combined2,
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
    first_valid = min(.data$year[!is.na(.data$value)]),
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
    .by = c("year", "touchstone")
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

  # add outcome name
  df_plot$outcome_name <- outcome

  if (nrow(df_plot) == 0 || all(df_plot$value == 0)) {
    message("No non-zero data to plot for ", disease, ". Skipping plot.")
    return(NULL)
  }

  df_plot
}
