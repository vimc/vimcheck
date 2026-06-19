# Package design decisions

This page documents *vimcheck* package design decisions as a guide to
users, and to potential contributors seeking to extend this package or
revise some of those decisions.

Some design decisions that live in the background include:

- *vimcheck* is set up as a modern R package built with *devtools*,
  *usethis*, and *testthat* (testing);
- *vimcheck* is documented using *roxygen2* and *pkgdown* for the
  package website;
- *vimcheck* is developed on GitHub and uses continuous integration
  workflows provided by GitHub Actions;
- Any other decisions not covered here most likely follow advice in
  [Wickham and Bryan’s *R Packages*](https://r-pkgs.org/).

## Big picture

*vimcheck* is intended to collect, house, and be the single source for
functionality used by VIMC to check submitted modelling outputs for
discrepancies. This solves the immediate problem that this functionality
is currently spread and repeated over multiple reports, increasing the
potential for discrepancies and functionality drift in the tools. The
overall goal is to improve the quality of VIMC’s work by improving the
reliability of VIMC outputs. The main users are currently intended to be
members of the VIMC Secretariat, but may include VIMC consortium members
in future.

## Function organisation

*vimcheck* is currently developed in bursts, with each burst so far
adding a set of data wrangling and plotting functions taken from a
specific VIMC report.

The package has two axes of organisation for its functionality: the
theme or goal of the report from which the function comes, and what the
function does.

The current reports from which functions have been taken relate to:

- Diagnostics on burden estimates provided by VIMC modelling groups, and
- Pressure testing diagnostics intended to check for outliers in vaccine
  impact estimates.

Functions are split into four categories, and the general idea is to
have functionality be modular. As an example, *vimcheck* favours
functions that produce intermediate products that can be reused by
multiple downstream functions (following the [DRY
principle](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself)).

- Functions that work on data: This is the main functionality of the
  package and includes functions that work with raw or semi-processed
  data, such as burden or impact estimates;
- Functions that prepare data for plotting: These are functions that sit
  between the data wrangling functions and plotting functions;
- Plotting functions that are typically related to one upstream
  data-wrangling or plotting-preparation function;
- Helper functions that provide useful but miscellaneous functionality.

The general idea is to be able to set up small R pipelines within
reports of the following form.

``` r

# some data read in from a local source
data |>
  fn_wrangle_data() |>
  fn_prep_data_for_plotting() |>
  fn_plot_data()
```

The [function reference in the
documentation](https://vimc.github.io/vimcheck/reference/index.html) is
organised similarly.

The R source code files in `./R/` are also organised in this way; for
example `R/fn_burden_diagnostics.R` holds data-wrangling functions
related to burden estimates, `R/fn_plotting_prep_bur_diag.R` holds
functions to prepare wrangled data for plotting, and
`R/fn_plotting_burden_diagnostics.R` holds plotting functions for the
prepared data.

## Package data

*vimcheck* includes some package data which is used to demonstrate and
test its functionality. Some data is purely dummy data that follows the
structure of data seen in VIMC reports. However, some data such as
\[eg_impact\] is real VIMC data that has been released publicly as part
of other packages.

There is a number of package constants, which are single values or small
vectors that are provided with and exported from the package.

## Package dependencies

We only list notable dependencies here.

- [Tidyverse packages](https://tidyverse.org/) over base R or
  *data.table*; this is to keep functions within the dependency
  framework used in the reports from which they come — we assume the
  report writers are also the package user-base and *vimcheck* aims to
  be used, and friendly to use, for these people.

- *cli* and *glue* for string interpolation and printing error messages
  to screen.

- *diffdf* to provide differences between data.frames.

- *ggplot2* for plotting; functions are not explicitly namespaced in
  many cases, but imported from the package to reduce code clutter in
  plotting function files.

- *checkmate* for input checking and to extend *testthat*.

- [*vdiffr*](https://cran.r-project.org/package=vdiffr) for snapshot
  tests of plotting functions.

### Data frames and tibbles

Data-wrangling functions are agnostic to the type of tabular input, but
always return a [tibble](https://tibble.tidyverse.org/) rather than a
plain data.frame (if they return tabular data). This is because internal
manipulation using Tidyverse functions often results in tibbles being
produced (e.g. using `tidyr::pivot_*()`, or
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
followed by ungrouping), but inexplicably some Tidyverse functions
preserve data.frames.  
We think it is preferable for users and developers to have a uniform
function output type rather than have to guess whether it will be a
tibble or a data.frame. A second reason is that tibbles are much easier
to read when printed to screen.

**Note that** all downstream functions — plotting preparation and
plotting functions — that expect tabular data expect a tibble, and
**will error** if they are not passed a tibble! This is partially to
create some friction so that users check what they are passing: data
processed with *vimcheck* will always return a tibble, downstream
functions only work on processed data, and errors might indicate that
the wrong data are being passed.

## Testing

*vimcheck* function are tested using package data (see above). As a
result, tests focus on input checking and the form of outputs. There are
comparatively few tests on correctness (e.g. are output numbers within a
range), and this is a clear avenue for further development.
