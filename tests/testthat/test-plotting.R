test_that("plot_compare_demography() works", {
  burden <- eg_burden_template

  burden <- check_demography_alignment(burden, eg_wpp)
  burden <- prep_plot_demography(burden)

  p <- plot_compare_demography(burden, 1)

  checkmate::expect_class(p, "ggplot")

  vdiffr::expect_doppelganger("plot_demography_alignment", p)
})

test_that("plot_age_patterns() works", {
  burden <- eg_burden_template
  burden <- prep_plot_age(burden)

  # manually set values as template default is NA, prevents ggplot warnings
  burden$value_millions <- 1.0

  p <- plot_age_patterns(burden, 1)

  checkmate::expect_class(p, "ggplot")

  vdiffr::expect_doppelganger("plot_age_patterns", p)
})

test_that("plot_global_burden_decades() works", {
  burden <- eg_burden_template
  year_max <- 2100
  burden <- prep_plot_burden_decades(burden, year_max)

  # manually set values as template default is NA, prevents ggplot warnings
  burden$value_millions <- 1.0

  p <- plot_global_burden_decades(burden, 1)

  checkmate::expect_class(p, "ggplot")

  vdiffr::expect_doppelganger("plot_global_burden_decades", p)
})

test_that("plot_global_burden_decades() works", {
  burden <- eg_burden_template
  burden <- prep_plot_global_burden(burden)

  # NOTE: expected use case is to loop over nested column DFs
  # set values to a dummy placeholder
  burden$burden_data[[1]]$value_millions <- 1

  p <- plot_global_burden(
    burden$burden_data[[1]],
    burden$burden_outcome[[1]],
    1
  )

  checkmate::expect_class(p, "ggplot")

  vdiffr::expect_doppelganger("plot_global_burden", p)
})
