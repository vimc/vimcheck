# TODO: unsure how this should be tested with data provided
test_that("`compare_natl_subreg()`: Comparing national-subregional works", {
  df <- suppressWarnings(flag_duplicates(eg_impact))
  df <- dplyr::filter(df, n_key == 1)
  df <- tidyr::pivot_wider(
    df,
    id_cols = {{ COLNAMES_KEY_PRESSURE_TEST }},
    names_from = "burden_outcome",
    values_from = "impact"
  )
  # prev_df$support_type <- "other" # unsure what values this can take
  # prev_df$coverage <- 0.5

  # assign dummy values
  df$deaths_averted <- 1e3
  df$dalys_averted <- 1e6

  df <- dplyr::left_join(
    df,
    who_subregions,
    by = c("country", "country_name")
  )

  expect_data_frame(
    compare_natl_subreg(df)
  )
  expect_names(
    names(compare_natl_subreg(df)),
    must.include = c(
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
  )

  # errors and warnings
  expect_error(
    compare_natl_subreg("df"),
    "Must be of type 'data.frame'"
  )
  expect_error(
    compare_natl_subreg(data.frame(year = 2026)),
    "Must have at least 10 cols"
  )
  expect_error(
    compare_natl_subreg(
      as.data.frame(as.list(1:10))
    ),
    "Names must include the elements"
  )
  expect_error(
    compare_natl_subreg(df, "dummy_outcome"),
    "`outcome` must be one of"
  )
  expect_error(
    compare_natl_subreg(df, activity_filter = "dummy_activity"),
    "`activity_filter` must be one of"
  )
})
