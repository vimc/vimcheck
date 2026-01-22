test_that("plot_compare_demography() works", {
  burden <- eg_burden_template

  burden <- check_demography_alignment(burden, eg_wpp)
  burden <- prep_plot_demography(burden)

  p <- plot_compare_demography(burden, 1)

  checkmate::expect_class(p, "ggplot")

  vdiffr::expect_doppelganger("A blank plot", p)
})
