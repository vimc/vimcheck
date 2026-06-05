# each test uses a scoped copy of impact data
test_that("`filter_recent_ts()`: Filtering by touchstone works", {
  df <- eg_impact
  df$touchstone <- DEF_TOUCHSTONE_NEW
  test_scenario_types <- rep(
    c("default", "dummy"),
    each = nrow(df) / 2
  )
  df$scenario_type <- test_scenario_types

  # touchstone is most recent one - expect filtering
  expect_data_frame(
    filter_recent_ts(df)
  )
  # half rows are excluded due to scenario name
  expect_equal(
    nrow(filter_recent_ts(df)),
    nrow(df) / 2L
  )

  # touchstone is older - no filtering
  df$touchstone <- DEF_TOUCHSTONE_OLD

  expect_data_frame(
    filter_recent_ts(df)
  )
  expect_equal(
    nrow(filter_recent_ts(df)),
    nrow(df)
  )

  # warnings and errors
  # bad df input
  expect_error(
    filter_recent_ts("df"),
    "Must be of type 'data.frame'"
  )
  expect_error(
    filter_recent_ts(data.frame())
  )
  expect_error(
    filter_recent_ts(data.frame(year = 2026))
  )
  expect_error(
    filter_recent_ts(data.frame(year = 2026, disease = "YF")),
    "Names must include the elements \\{'touchstone'\\}"
  )
  expect_error(
    filter_recent_ts(data.frame(touchstone = 2026, year = 2026)),
    "(Touchstone year should be a string with at least 6)*(characters)"
  )

  # bad touchstone
  expect_error(
    filter_recent_ts(df, as.numeric(DEF_TOUCHSTONE_NEW)),
    "(Touchstone year should be a string with at least 6)*(characters)"
  )
})

# each test uses a scoped copy of impact data
test_that("`filter_excluded_diseases_ts()`: Filtering by disease works", {
  df <- eg_impact
  df$touchstone <- DEF_TOUCHSTONE_NEW

  # replace known rows with an exlcuded disease
  n_replaced <- 100L
  df[1:n_replaced, "disease"] <- EXCLUDED_DISEASES[1L]

  # touchstone at/above threshold - expect no filtering
  expect_data_frame(
    filter_excluded_diseases_ts(df)
  )
  expect_equal(
    nrow(filter_excluded_diseases_ts(df)),
    nrow(df)
  )

  # touchstone is older than threshold - expect filtering
  df$touchstone <- DEF_TOUCHSTONE_OLD
  expect_equal(
    nrow(filter_excluded_diseases_ts(df, DEF_TOUCHSTONE_NEW)),
    nrow(df) - n_replaced
  )

  # warnings and errors
  # bad df input
  expect_error(
    filter_excluded_diseases_ts("df"),
    "Must be of type 'data.frame'"
  )
  expect_error(
    filter_excluded_diseases_ts(data.frame())
  )
  expect_error(
    filter_excluded_diseases_ts(data.frame(year = 2026)),
    "Names must include the elements \\{'touchstone'\\}"
  )
  expect_error(
    filter_excluded_diseases_ts(data.frame(touchstone = 2026)),
    "(Touchstone year should be a string with at least 6)*(characters)"
  )

  # bad touchstone
  expect_error(
    filter_excluded_diseases_ts(df, as.numeric(DEF_TOUCHSTONE_NEW)),
    "(Touchstone year should be a string with at least 6)*(characters)"
  )
})

test_that("`flag_duplicates()`: Flagging duplicates works", {
  df <- eg_impact
  expect_warning(
    flag_duplicates(df),
    "duplicates found in data"
  )
  expect_data_frame(
    suppressWarnings(flag_duplicates(df))
  )

  # TODO: please check that duplicates are true duplicates
  expect_equal(
    nrow(flag_duplicates(df)),
    nrow(df)
  )

  expect_true(
    "n_key" %in% colnames(flag_duplicates(df))
  )

  # errors
  expect_error(
    flag_duplicates("df"),
    "Must be of type 'data.frame'"
  )
  expect_error(
    flag_duplicates(df, 1:100),
    "Must have at least 100 cols"
  )
  expect_error(
    flag_duplicates(df, 1:6),
    "'key_cols' failed: Must be of type 'character'"
  )
  expect_error(
    flag_duplicates(
      data.frame(year = 2026),
      "vaccine"
    ),
    "(Expected `df` to have columns)*(but columns)*(were missing)"
  )
})

# TODO: how should this be tested?
test_that("`filter_invalid_trajectories()`: Filtering impact trends works", {
  prev_df <- flag_duplicates(eg_impact)
  prev_df <- dplyr::filter(prev_df, n_key == 1)
  prev_df <- tidyr::pivot_wider(
    prev_df,
    id_cols = {{ COLNAMES_KEY_PRESSURE_TEST }},
    names_from = "burden_outcome",
    values_from = "impact"
  )
  prev_df$support_type <- "other" # unsure what values this can take
  prev_df$coverage <- 0.5
  prev_df$deaths_averted <- 1e3
  prev_df$dalys_averted <- 1e6

  # assign dummy values
  curr_df <- prev_df
  curr_df$deaths_averted <- NA_real_
  curr_df$dalys_averted <- NA_real_

  expect_data_frame(
    filter_invalid_trajectories(
      curr_df,
      prev_df
    )
  )
  expect_equal(
    nrow(
      filter_invalid_trajectories(
        curr_df,
        prev_df
      )
    ),
    nrow(curr_df)
  )
})
