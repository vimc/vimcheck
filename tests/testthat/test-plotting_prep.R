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

skip("Skipped while tests are written")
test_that("prep_plot_age() works ", {
  checkmate::expect_tibble(
    prep_plot_age(x)
  )
})

test_that("prep_plot_burden_decades() works ", {
  checkmate::expect_tibble(
    prep_plot_burden_decades(x)
  )
})

test_that("prep_plot_global_burden() works ", {
  checkmate::expect_tibble(
    prep_plot_global_burden(x)
  )
})

test_that("prep_plot_coverage_set() works ", {
  checkmate::expect_tibble(
    prep_plot_coverage_set(x)
  )
})

test_that("prep_plot_fvp() works ", {
  checkmate::expect_tibble(
    prep_plot_fvp(x)
  )
})
