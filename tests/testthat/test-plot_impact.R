test_that("Plotting impact works", {
  p <- plot_impact(
    eg_impact_2,
    "A",
    "cases",
    "year",
    "Dummy title"
  )
  expect_class(p, "ggplot")

  vdiffr::expect_doppelganger("plot_impact", p)

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

test_that("Impact trajectories retain zero estimates in both views", {
  impact_data <- data.frame(
    country = "A",
    year = 2020:2023,
    birth_cohort = 2000:2003,
    burden_outcome = "cases",
    impact = c(10, 0, 10, NA),
    short_name = "routine"
  )

  for (values in list(c(10, 0, 10, NA), c(-10, 0, 10, NA))) {
    impact_data$impact <- values

    for (view in c("year", "cohort")) {
      p <- plot_impact(impact_data, "A", "cases", view)
      expected_x <- if (view == "year") 2020:2022 else 2000:2002
      expected_y <- values[1:3]

      expect_equal(p$data$impact, expected_y)

      layers <- ggplot2::ggplot_build(p)$data
      for (layer in layers[2:3]) {
        expect_equal(layer$x, expected_x)
        expect_equal(layer$y, expected_y)
      }
    }
  }
})

test_that("Impact plots still reject outcomes without nonzero estimates", {
  impact_data <- data.frame(
    country = "A",
    year = 2020:2022,
    birth_cohort = 2000:2002,
    burden_outcome = c("cases", "cases", "deaths"),
    impact = c(0, NA, 10),
    short_name = "routine"
  )

  for (view in c("year", "cohort")) {
    expect_error(
      plot_impact(impact_data, "A", "cases", view),
      "No estimates remaining in the data"
    )
    expect_error(
      plot_impact(impact_data, "A", "dalys", view),
      "No estimates remaining in the data"
    )
  }
})

test_that("Plotting coverage and FVPs works", {
  p <- plot_coverage_fvps(eg_fvps_2, "AGO")
  expect_list(p, "ggplot")

  vdiffr::expect_doppelganger("plot_coverage", p[[1]])
  vdiffr::expect_doppelganger("plot_fvps", p[[2]])

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
