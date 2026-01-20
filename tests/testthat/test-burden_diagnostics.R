test_that("`validate_file_dict_template()` works", {
  # set up local tempdir to test missing bad file error msg
  wd <- withr::local_tempdir()

  tmp_path_burden <- file.path(wd, "incoming_burden_estimates")
  dir.create(tmp_path_burden)
  file_dict <- file.path(tmp_path_burden, "file_dictionary.csv")
  dummy_data <- data.frame()
  readr::write_csv(dummy_data, file_dict)

  disease <- "dummy_disease"

  success_msg <- glue::glue(
    "(File dictionary found at)*({file_dict})*(no action needed)"
  )
  fail_msg <- "(File dictionary)*(is not well formed)"

  expect_error(
    validate_file_dict_template(
      disease,
      tmp_path_burden
    ),
    fail_msg
  )

  # check success segment
  eg_path_burden <- test_path("incoming_burden_estimates")
  expect_message(
    validate_file_dict_template(
      disease,
      eg_path_burden
    ),
    success_msg
  )

  # check ladder where file dict is missing
  file.remove(file_dict)
  expect_error(
    validate_file_dict_template(
      "dummy_disease",
      tmp_path_burden
    ),
    "(model_inputs)*(does not exist)"
  )

  # check no scenario.csv file found
  dir.create(file.path(tmp_path_burden, "model_inputs"))
  expect_error(
    validate_file_dict_template(
      "dummy_disease",
      tmp_path_burden
    ),
    "(File does not exist)*(scenario\\.csv)"
  )

  # check success when scenario.csv file found
  tmp_scenarios_file <- file.path(
    tmp_path_burden,
    "model_inputs",
    "scenario.csv"
  )
  file.copy(
    file.path(eg_path_burden, "model_inputs", "scenario.csv"),
    tmp_scenarios_file
  )

  expect_message(
    validate_file_dict_template(
      "dummy_disease",
      tmp_path_burden
    ),
    "(No file dictionary found)*(created a file dictionary)"
  )
})

test_that("`validate_complete_incoming_files()`", {
  # set up local tempdir with bad file dictionary to check errors
  wd <- withr::local_tempdir()

  path_burden <- file.path(wd, "incoming_burden_estimates")
  dir.create(path_burden)

  fail_msg <- "(Expected a file dictionary)*(but it was not found)"
  expect_error(
    validate_complete_incoming_files(path_burden),
    fail_msg
  )

  file_dict <- file.path(path_burden, "file_dictionary.csv")
  dummy_data <- make_novax_scenario("dummy_disease")
  dummy_data <- rbind(dummy_data, dummy_data)
  n_duplicates <- anyDuplicated(dummy_data)
  readr::write_csv(dummy_data, file_dict)

  fail_msg <- glue::glue(
    "(Expected)*(non-duplicate entries)*(found {n_duplicates} duplicated rows)"
  )
  expect_error(
    validate_complete_incoming_files(path_burden),
    fail_msg
  )

  # check error on duplicated filenames
  dummy_data <- readr::read_csv(
    test_path("incoming_burden_estimates", "file_dictionary.csv"),
    show_col_types = FALSE
  )
  dummy_data$file <- "dummy_file.csv"
  readr::write_csv(dummy_data, file_dict)

  fail_msg <- "(Expected)*(non-duplicate scenario filenames)"
  expect_error(
    validate_complete_incoming_files(path_burden),
    fail_msg
  )

  # check error on missing scenario files
  dummy_data <- readr::read_csv(
    test_path("incoming_burden_estimates", "file_dictionary.csv"),
    show_col_types = FALSE
  )
  readr::write_csv(dummy_data, file_dict)
  fail_msg <- "Expected as many scenario data files as scenarios"
  expect_error(
    validate_complete_incoming_files(path_burden),
    fail_msg
  )

  # TODO: test success case
})

# test for checking template alignment
test_that("`check_template_alignment()` works", {
  template <- eg_burden_template
  burden_set <- template

  expect_no_condition(
    validate_template_alignment(
      burden_set,
      template
    )
  )

  diff_tibble <- tibble::tibble(
    disease = character(0),
    country = character(0),
    year = double(0),
    age = double(0)
  )

  perfect_output <- list(
    missing_cols_in_burden = character(0),
    extra_cols_in_burden = character(0),
    burden_cols_matches_template = TRUE,
    missing_grid_in_burden = diff_tibble,
    extra_grid_in_burden = diff_tibble,
    burden_grid_matches_template = TRUE
  )

  expect_identical(
    validate_template_alignment(
      burden_set,
      template
    ),
    perfect_output,
    ignore_attr = TRUE
  )

  # check when burden is missing cols
  burden_set_ <- burden_set
  missing_col <- "cohort_size"
  extra_col <- "extra_col"
  burden_set_[, missing_col] <- NULL
  burden_set_[, extra_col] <- NA_character_

  extra_rows <- head(burden_set_)
  extra_rows$disease <- "dummy_disease_2"
  burden_set_ <- rbind(burden_set_, extra_rows)

  output <- validate_template_alignment(
    burden_set_,
    template
  )
  expect_identical(
    output$missing_cols_in_burden,
    missing_col
  )
  expect_identical(
    output$extra_cols_in_burden,
    extra_col
  )
  expect_false(
    output$burden_cols_matches_template
  )
  expect_false(
    output$burden_grid_matches_template
  )

  # check errors on inputs
  expect_error(
    validate_template_alignment(
      "burden_set",
      template
    ),
    "Must be of type 'data.frame'"
  )
  expect_error(
    validate_template_alignment(
      burden_set,
      "template"
    ),
    "Must be of type 'data.frame'"
  )
})

test_that("`check_demography_alignment()` works", {
  # assume burden data are same as template
  burden <- eg_burden_template

  # assign dummy cohort size value from random draw
  # 10e6 is the value in `eg_wpp`, allow negative differences
  burden$cohort_size <- withr::with_seed(
    1,
    rnorm(nrow(burden), 10e6, 1e4)
  )

  expect_no_condition(
    check_demography_alignment(
      burden,
      eg_wpp
    )
  )

  output <- check_demography_alignment(
    burden,
    eg_wpp
  )
  expected_names <- c(
    "country",
    "year",
    "age",
    "cohort_size",
    "provided",
    "expected",
    "difference",
    "abs_diff",
    "prop_diff"
  )

  checkmate::expect_tibble(
    output,
    types = c("character", "numeric"), # allowed types, no order
    all.missing = FALSE,
    col.names = "unique"
  )
  checkmate::expect_names(
    colnames(output),
    permutation.of = expected_names
  )
})
