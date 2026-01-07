test_that("`validate_file_dict_template()` works", {
  # set up local tempdir to test missing bad file error msg
  wd <- withr::local_tempdir()

  path_burden <- file.path(wd, "incoming_burden_estimates")
  dir.create(path_burden)
  file_dict <- file.path(path_burden, "file_dictionary.csv")
  dummy_data <- data.frame()
  readr::write_csv(dummy_data, file_dict)

  success_msg <- glue::glue(
    "(File dictionary found at)*({file_dict})*(no action needed)"
  )
  fail_msg <- "(File dictionary found)*(but it is not well formed)"

  expect_error(
    validate_file_dict_template(
      disease,
      path_burden
    ),
    fail_msg
  )

  # check success segment
  path_burden <- test_path("incoming_burden_estimates")
  expect_message(
    validate_file_dict_template(
      disease,
      path_burden
    ),
    success_msg
  )

  validate_file_dict_template(
    "dummy_disease",
    test_path("incoming_burden_estimates")
  )

  # TODO: check other branch of if-else ladder
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
    test_path("incoming_burden_estimates", "file_dictionary.csv")
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
    test_path("incoming_burden_estimates", "file_dictionary.csv")
  )
  readr::write_csv(dummy_data, file_dict)
  fail_msg <- "Expected as many scenario data files as scenarios"
  expect_error(
    validate_complete_incoming_files(path_burden),
    fail_msg
  )
})
