# Prepare data for plotting

Transform burden estimate data from modelling groups to make them
suitable for plotting using an appropriate [plotting
function](https://vimc.github.io/vimcheck/reference/plotting.md). Each
preparation function corresponds to a plotting function.

## Usage

``` r
prep_plot_demography(burden)

prep_plot_age(burden)

prep_plot_burden_decades(burden, year_max)

prep_plot_global_burden(burden)

prep_plot_coverage_set(coverage)

prep_plot_fvp(fvp, year_min, year_max)
```

## Arguments

- burden:

  For `prep_plot_demography()`, a `<tibble>` output from
  [`check_demography_alignment()`](https://vimc.github.io/vimcheck/reference/check_demography_alignment.md).
  For other functions, a burden dataset similar to
  [eg_burden_template](https://vimc.github.io/vimcheck/reference/eg_burden_template.md).

- year_max:

  The maximum year to be represented in a subsequent figure. For
  `prep_plot_burden_decades()`, must be a decade, i.e., multiple of 10.

- coverage:

  WIP. Coverage data.

- fvp:

  WIP. Data on counts of fully vaccinated persons.

- year_min:

  Minimum year.

## Value

- For `prep_plot_demography()`: a `<tibble>` in long-format, with the
  identifier-columns, "scenario", "age", and "year", with the added
  column "value_millions".

- For `prep_plot_age()`: a `<tibble>` with the columns "scenario",
  "burden_outcome", "age", "value_millions".

- For `prep_plot_burden_decades()`: a `<tibble>` with the columns
  "scenario", "burden_outcome", "decade_label", and "value_millions".

- For `prep_plot_global_burden()`: a nested `<tibble>` with the string
  column "burden_outcome", and a list column of tibbles "burden_data".

- For `prep_plot_coverage_set()`: WIP

- For `prep_plot_fvp()`: WIP.
