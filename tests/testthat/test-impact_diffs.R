test_that("`gen_national_iqr()`: Generating impact IQR works", {
  df <- suppressWarnings(flag_duplicates(eg_impact))
  df <- dplyr::filter(df, n_key == 1)
  df <- tidyr::pivot_wider(
    df,
    id_cols = {{ COLNAMES_KEY_PRESSURE_TEST }},
    names_from = "burden_outcome",
    values_from = "impact"
  )

  expect_data_frame(
    gen_national_iqr(df)
  )
  expect_data_frame(
    gen_national_iqr(df, value_cols = "deaths_averted")
  )
  expect_data_frame(
    gen_national_iqr(df, value_cols = "deaths_averted")
  )

  group_cols <- c("country", "vaccine", "activity_type")
  value_cols <- c("deaths_averted", "dalys_averted")
  prefix <- "xyz_prefix"
  df_iqr <- gen_national_iqr(
    df,
    group_cols,
    value_cols,
    prefix
  )

  expect_names(
    names(df_iqr),
    must.include = c(group_cols, sprintf("%s_%s", prefix, value_cols))
  )

  # check for errors
  expect_error(
    gen_national_iqr("df"),
    "Must be of type 'data.frame'"
  )
  expect_error(
    gen_national_iqr(data.frame(year = 2026)),
    "Must have at least 5 cols"
  )
  expect_error(
    gen_national_iqr(
      as.data.frame(as.list(1:10))
    ),
    "Names must include"
  )
  expect_error(
    gen_national_iqr(
      df,
      1:10
    ),
    "(group_cols)*(Must be of type 'character')"
  )
  expect_error(
    gen_national_iqr(
      df,
      value_cols = "dummy_value_col"
    ),
    "(value_cols)*(has additional elements)*(dummy_value_col)"
  )
  expect_error(
    gen_national_iqr(df, prefix = 1L),
    "Must be of type 'string'"
  )
})

test_that("`generate_diffs()`: Generating differences works", {
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

  # assign dummy values
  curr_df <- prev_df
  curr_df$deaths_averted <- 1e3
  curr_df$dalys_averted <- 1e6

  interest_cols <- c("deaths_averted", "dalys_averted")
  difflist <- suppressWarnings(
    generate_diffs(
      prev_df,
      curr_df,
      interest_cols
    )
  )
  expect_list(
    difflist,
    names = "unique"
  )
  expect_names(
    names(difflist),
    permutation.of = interest_cols
  )

  # errors and warnings
  expect_error(
    generate_diffs("df"),
    "Must be of type 'data.frame'"
  )
  expect_error(
    generate_diffs(data.frame(year = 2026)),
    "Must have at least 14 cols"
  )
  expect_error(
    generate_diffs(prev_df, "df"),
    "Must be of type 'data.frame'"
  )
  expect_error(
    generate_diffs(prev_df, data.frame(year = 2026)),
    "Must have at least 14 cols"
  )
  expect_error(
    generate_diffs(
      prev_df,
      curr_df,
      1:10
    ),
    "(interest_cols)*(Must be of type 'character')"
  )
  expect_error(
    generate_diffs(
      prev_df,
      curr_df,
      key_cols = 1:10
    ),
    "(key_cols)*(Must be of type 'character')"
  )
  expect_error(
    generate_diffs(
      as.data.frame(as.list(1:14)),
      curr_df
    ),
    "(colnames\\(prev_df\\))*(Names must include)"
  )
  expect_error(
    generate_diffs(
      prev_df,
      as.data.frame(as.list(1:14))
    ),
    "(colnames\\(curr_df\\))*(Names must include)"
  )
  expect_error(
    generate_diffs(
      prev_df,
      curr_df,
      touchstone = "999999"
    )
  )
})


test_that("`flag_large_diffs()`: Flagging large diffs works", {
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

  # assign dummy values
  curr_df <- prev_df
  curr_df$deaths_averted <- 1e6
  curr_df$dalys_averted <- 1e9

  interest_cols <- c("deaths_averted", "dalys_averted")
  changes <- suppressWarnings(generate_diffs(
    prev_df,
    curr_df,
    interest_cols
  ))

  # national IQR - inset dummy values for tests
  national_iqr <- gen_national_iqr(prev_df)
  national_iqr$national_iqr_deaths_averted <- seq_len(nrow(national_iqr))

  expect_data_frame(
    flag_large_diffs(
      changes,
      national_iqr,
      "deaths_averted"
    )
  )
  expect_data_frame(
    flag_large_diffs(
      changes,
      national_iqr,
      "dalys_averted"
    )
  )

  # check touchstones added
  tstone_old <- "208801"
  tstone_new <- "209901"
  diffs <- flag_large_diffs(
    changes,
    national_iqr,
    "deaths_averted",
    touchstone_old = tstone_old,
    touchstone_new = tstone_new
  )
  expect_names(
    names(diffs),
    must.include = c(tstone_old, tstone_new)
  )
})

test_that("`gen_combined_df()`: Generating combined data works", {
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
  prev_df$touchstone <- "202010"

  # assign dummy values
  curr_df <- prev_df
  curr_df$deaths_averted <- 1e6
  curr_df$dalys_averted <- 1e9
  curr_df$touchstone <- "202310"

  expect_data_frame(
    gen_combined_df(
      prev_df,
      curr_df
    )
  )
  expect_names(
    names(
      gen_combined_df(
        prev_df,
        curr_df
      )
    ),
    must.include = sprintf(
      "%s_%s",
      c("deaths_averted", "dalys_averted"),
      c("old", "new")
    )
  )

  # check error on touchstone
  prev_df$touchstone <- NULL
  curr_df$touchstone <- NULL
  expect_error(
    gen_combined_df(prev_df, curr_df),
    "(Names)*(is missing elements)*(touchstone)"
  )
})
