# Plot burden and impact diagnostics

Plotting functions for burden and impact diagnostics. All functions
operate on data prepared for plotting by a corresponding
[plotting-preparation
function](https://vimc.github.io/vimcheck/reference/plotting_prep.md).

## Usage

``` r
plot_compare_demography(data, fig_number)

plot_age_patterns(burden_age, fig_number)

plot_global_burden_decades(burden_decades, fig_number)

plot_global_burden(burden_data, outcome_name, fig_number)

plot_coverage_set(coverage_set, fig_number)

plot_fvp(fvp_data, fig_number)
```

## Arguments

- data:

  A `<data.frame>` that gives the comparison between VIMC-provided and
  modeller-used demography values, in long-format. This is expected to
  be the output of
  [`check_demography_alignment()`](https://vimc.github.io/vimcheck/reference/check_demography_alignment.md)
  processed by
  [`prep_plot_demography()`](https://vimc.github.io/vimcheck/reference/plotting_prep.md).

- fig_number:

  The figure number displayed in the plot title.

- burden_age:

  A `<tibble>` with the minimum column names "age", "value_millions",
  "burden_outcome", and "scenario"; expected to be the output of
  [`prep_plot_age()`](https://vimc.github.io/vimcheck/reference/plotting_prep.md).

- burden_decades:

  A `<tibble>` giving the burden by decade, up to `year_max`; expected
  to be the output of
  [`prep_plot_burden_decades()`](https://vimc.github.io/vimcheck/reference/plotting_prep.md).

- burden_data:

  This is expected to be a `<tibble>` from a nested-`<tibble>`
  constructed using
  [`prep_plot_global_burden()`](https://vimc.github.io/vimcheck/reference/plotting_prep.md).

- outcome_name:

  A string for an outcome name. Allowed outcome names are given in the
  package constant
  [constants](https://vimc.github.io/vimcheck/reference/constants.md).

- coverage_set:

  A `<tibble>` that is the output of
  [`prep_plot_coverage_set()`](https://vimc.github.io/vimcheck/reference/plotting_prep.md).

- fvp_data:

  A `<tibble>` of estimates of fully-vaccinated persons (FVPs) per
  scenario, with scenarios as factors in order of the number of
  adjusted-FVPs. Expected to be the output of
  [`prep_plot_fvp()`](https://vimc.github.io/vimcheck/reference/plotting_prep.md).

## Value

A `<ggplot>` object that can be printed to screen in the plot frame or
saved to an output device (i.e., saved as an image file).
