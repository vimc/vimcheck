# Basic checks on internal functions. These are mostly checked via exported
# functions for a smaller testing surface
test_that("`make_novax_scenario()` works", {
  disease <- "dummy"
  expect_no_condition(
    make_novax_scenario(disease)
  )
  df <- make_novax_scenario(disease)
  expect_named(
    df,
    file_dict_colnames
  )
})

test_that("Adaptive rounding works", {
  expect_number(
    adaptive_round(runif(1))
  )
  expect_numeric(
    adaptive_round(runif(10))
  )

  large_num <- 1.346
  expected_num <- 1.3
  expect_identical(
    adaptive_round(large_num),
    expected_num
  )

  small_num <- 0.345
  expected_num <- 0.34
  expect_identical(
    adaptive_round(small_num),
    expected_num
  )

  # round numeric
  df <- data.frame(
    year_but_complex = 2010:2015 + 0.35,
    num = 1.346
  )
  expect_data_frame(round_numeric(df))
  df_new <- round_numeric(df)

  # expect year not rounded
  expect_identical(
    df_new$year_but_complex,
    df$year_but_complex
  )

  expect_identical(
    df_new$num,
    adaptive_round(df_new$num)
  )
})

test_that("`validate_ts_year()`: Validating touchstone works", {
  ts <- "202010"
  expect_number(
    validate_ts_year(ts),
    lower = 200001,
    upper = 210012
  )

  expect_error(
    validate_ts_year(202010),
    "should be a string"
  )

  ts <- "2020"
  expect_error(
    validate_ts_year(ts),
    "at least 6 characters"
  )

  ts <- "199910"
  expect_error(
    validate_ts_year(ts),
    "expected an year in the range \\[2000, 2100\\]"
  )

  ts <- "220010"
  expect_error(
    validate_ts_year(ts),
    "expected an year in the range \\[2000, 2100\\]"
  )

  ts <- "202019"
  expect_error(
    validate_ts_year(ts),
    "expected a month in the range \\[1, 12\\]"
  )
})

test_that("`add_campaign_id()`: Adding campaign identifier works", {
  df <- data.frame(
    key1 = c("a", "b", "c", "a", "b", "c"),
    key2 = letters[1:6]
  )

  df_new <- add_campaign_id(df, c("key1", "key2"))
  expect_data_frame(df_new)
  expect_names(names(df_new), must.include = "campaign_id")
  expect_identical(
    unique(df_new$campaign_id),
    1L
  )

  df_new <- add_campaign_id(df, c("key1"))
  expect_identical(
    unique(df_new$campaign_id),
    c(1L, 2L)
  )

  expect_error(
    add_campaign_id("df", "key"),
    "Must be of type 'data.frame'"
  )
  expect_error(
    add_campaign_id(df, letters),
    "Must have at least 26 cols"
  )
  expect_error(
    add_campaign_id(df, c("a", "b")),
    "(columns)*(are missing)"
  )

  expect_error(
    add_campaign_id(df, 1:2),
    "(key_cols)*(Must be of type 'character')"
  )
})
