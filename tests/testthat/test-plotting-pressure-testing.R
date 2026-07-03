# preparatory code
prev_df <- suppressWarnings(flag_duplicates(eg_impact))
prev_df <- dplyr::filter(prev_df, n_key == 1)
prev_df <- tidyr::pivot_wider(
  prev_df,
  id_cols = {{ COLNAMES_KEY_PRESSURE_TEST }},
  names_from = "burden_outcome",
  values_from = "impact"
)
prev_df$support_type <- "other" # unsure what values this can take
prev_df$coverage <- 0.5
prev_df$fvps <- 1e6
prev_df$target_population <- 2e6

prev_df$deaths_averted <- withr::with_seed(
  1,
  rnorm(nrow(prev_df), 100, 0.1)
)
prev_df$dalys_averted <- prev_df$deaths_averted * 100
prev_df$touchstone <- "202010"

# assign dummy values
curr_df <- prev_df
curr_df$deaths_averted <- withr::with_seed(
  1,
  rnorm(nrow(prev_df), 300, 0.1)
)
curr_df$dalys_averted <- curr_df$deaths_averted * 100
curr_df$touchstone <- "202110"

interest_cols <- c("deaths_averted", "dalys_averted")
changes <- suppressWarnings(generate_diffs(
  prev_df,
  curr_df,
  interest_cols
))

# national IQR - inset dummy values for tests
national_iqr <- gen_national_iqr(prev_df)
national_iqr$national_iqr_deaths_averted <- seq_len(nrow(national_iqr))

test_that("plot_sig_diff() works", {
  df_plot <- flag_large_diffs(changes, national_iqr)

  p <- plot_sig_diff(df_plot)

  expect_class(p, "ggplot")
  vdiffr::expect_doppelganger("plot_sig_diff", p)
})

test_that("plot_diff() works", {
  df_plot <- gen_combined_df(prev_df, curr_df)

  p <- plot_diff(df_plot)

  expect_class(p, "ggplot")
  vdiffr::expect_doppelganger("plot_diff", p)
})

test_that("plot_modelling_group_variation() works", {
  prev_df <- dplyr::select(curr_df, vaccine, modelling_group) %>%
    dplyr::distinct() %>%
    dplyr::group_by(vaccine) %>%
    dplyr::mutate(mod_num = dplyr::row_number())

  df_plot <- prep_plot_mod_grp_varn(curr_df, prev_df)

  p <- plot_modelling_group_variation(df_plot)

  expect_class(p, "ggplot")
  vdiffr::expect_doppelganger("plot_modelling_group_variation", p)
})

test_that("plot_vaccine_gavi() works", {
  df_plot <- prep_plot_vax_gavi(curr_df, prev_df, "deaths_averted")

  p <- plot_vaccine_gavi(df_plot)

  expect_class(p, "ggplot")
  vdiffr::expect_doppelganger("plot_vaccine_gavi", p)
})

test_that("plot_cumul() works", {
  combined_df <- gen_combined_df(prev_df, curr_df)

  # NOTE: warnings probably generated due to use of dummy data
  df_plot <- suppressWarnings(
    prep_plot_cumul(combined_df, "deaths_averted", "Measles")
  )

  p <- plot_cumul(df_plot)

  expect_class(p, "ggplot")
  vdiffr::expect_doppelganger("plot_cumul", p)
})
