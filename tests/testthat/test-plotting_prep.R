test_that("prep_plot_demography() works ", {
  burden <- eg_burden_template

  x <- check_demography_alignment(burden, eg_wpp)
  y <- prep_plot_demography(x)

  checkmate::expect_tibble(y)
  checkmate::expect_names(
    colnames(y),
    must.include = colnames_plot_demog_compare
  )
})

test_that("prep_plot_age() works ", {
  burden <- eg_burden_template
  expected_colnames <- c("scenario", "burden_outcome", "age", "value_millions")
  x <- prep_plot_age(burden)

  checkmate::expect_tibble(x)
  checkmate::expect_names(
    colnames(x),
    permutation.of = expected_colnames
  )
})

test_that("prep_plot_burden_decades() works ", {
  burden <- eg_burden_template
  year_max <- 2100
  x <- prep_plot_burden_decades(burden, year_max)

  expected_colnames <- c(
    "scenario",
    "burden_outcome",
    "decade_label",
    "value_millions"
  )
  checkmate::expect_tibble(x)
  checkmate::expect_names(
    colnames(x),
    permutation.of = expected_colnames
  )
})


test_that("prep_plot_global_burden() works ", {
  burden <- eg_burden_template
  x <- prep_plot_global_burden(burden)

  expected_colnames <- c("burden_outcome", "burden_data")

  checkmate::expect_tibble(x, types = c("character", "list"))
  checkmate::expect_names(
    colnames(x),
    permutation.of = expected_colnames
  )
})

test_that("prep_plot_coverage_set() works ", {
  skip("TODO: Determine where coverage set data is generated")
})

test_that("prep_plot_fvp() works ", {
  skip("TODO: Fix how FVP data is generated")
})
