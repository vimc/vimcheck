test_that("Plotting impact works", {
  p <- plot_impact(
    eg_impact_2,
    "A",
    "cases",
    "year",
    "Dummy title"
  )
  expect_class(p, "ggplot")

  p <- plot_impact(
    eg_impact_2,
    "A",
    "deaths",
    "year",
    "Dummy title"
  )
  expect_class(p, "ggplot")

  p <- plot_impact(
    eg_impact_2,
    "A",
    "cases",
    "cohort",
    "Dummy title"
  )
  expect_class(p, "ggplot")

  # errors
  expect_error(
    plot_impact("dummy_data"),
    "Must be of type 'data.frame'"
  )
  expect_error(
    plot_impact("dummy_data"),
    "Must be of type 'data.frame'"
  )

  expect_error(
    plot_impact(eg_impact_2[-1]),
    "Names must include"
  )

  expect_error(
    plot_impact("dummy_data"),
    "Must be of type 'data.frame'"
  )

  expect_error(
    plot_impact(eg_impact_2),
    "expected to have country"
  )

  expect_error(
    plot_impact(eg_impact_2, "A", "dummy_impact"),
    "`burden_type` must be one of"
  )

  expect_error(
    plot_impact(eg_impact_2, "A", view = "dummy"),
    "`view` must be one of"
  )

  expect_error(
    plot_impact(eg_impact_2, "A", "dummy_impact"),
    "`burden_type` must be one of"
  )

  ei2 <- eg_impact_2
  ei2$impact <- 0

  expect_error(
    plot_impact(ei2, "A"),
    "No estimates remaining in the data"
  )
})

test_that("Plotting coverage and FVPs works", {
  p <- plot_coverage_fvps(eg_fvps_2, "AGO")
  expect_list(p, "ggplot")

  # errors
  expect_error(
    plot_coverage_fvps("dummy_data"),
    "Must be of type 'data.frame'"
  )
  expect_error(
    plot_coverage_fvps(eg_fvps_2[-1]),
    "Must have at least 7 cols"
  )

  expect_error(
    plot_coverage_fvps(eg_fvps_2, "ETH"),
    "expected to have country"
  )

  expect_warning(
    plot_coverage_fvps(
      eg_fvps_2[eg_fvps_2$activity_type == "campaign", ],
      "AGO"
    ),
    "There is no routine coverage"
  )
})
