# Create impact diagnostics plots

Functions that create impact diagnostics plots (or plotting objects).
All functions are associated with one other upstream data processing
function, and can be used in a pipe with that function. Where
appropriate, outcome selection and label preparation is automated to
reduce function arguments.

Plotting functions for impact diagnostics. See [plotting-preparation
functions](https://vimc.github.io/vimcheck/reference/plot_prep_impact_diagnostics.md)
for a set of helper functions that prepare impact diagnostics for
plotting. See the details of the `data` argument for functions that help
to prepare the data.

## Usage

``` r
plot_sig_diff(data, outcome = IMPACT_OUTCOMES)

plot_diff(
  data,
  outcome = IMPACT_OUTCOMES,
  group_vars = IMPACT_GROUP_VARS,
  touchstone_old = DEF_TOUCHSTONE_OLD,
  touchstone_new = DEF_TOUCHSTONE_NEW
)

plot_modelling_group_variation(data)

plot_vaccine_gavi(data)

plot_cumul(data)
```

## Arguments

- data:

  A data.frame suitable for plotting.

  - `plot_sig_diff()`: Output of
    [`flag_large_diff()`](https://vimc.github.io/vimcheck/reference/flag_large_diffs.md).

  - `plot_diff()`: Output of
    [`gen_combined_df()`](https://vimc.github.io/vimcheck/reference/gen_combined_df.md).

  - `plot_modelling_group_variation()`: Output of
    [`plot_prep_mod_grp_varn()`](https://vimc.github.io/vimcheck/reference/plot_prep_impact_diagnostics.md).

  - `plot_vaccine_gavi()`: Output of
    [`plot_prep_vax_gavi()`](https://vimc.github.io/vimcheck/reference/plot_prep_impact_diagnostics.md)

  - `plot_cumul()`: Output of
    [`plot_prep_cumul()`](https://vimc.github.io/vimcheck/reference/plot_prep_impact_diagnostics.md)

- outcome:

  A string for the impact outcome. One of
  [IMPACT_OUTCOMES](https://vimc.github.io/vimcheck/reference/constants.md).

- group_vars:

  A single string for the grouping variables. May be any of
  [IMPACT_OUTCOMES](https://vimc.github.io/vimcheck/reference/constants.md),
  which are `"activity_type"` and `"vaccine"`.

- touchstone_old:

  A string for the previous touchstone in format `"YYYYMM"`. Defaults to
  [DEF_TOUCHSTONE_OLD](https://vimc.github.io/vimcheck/reference/constants.md).

- touchstone_new:

  A string for the current or new touchstone in format `"YYYYMM"`.
  Defaults to
  [DEF_TOUCHSTONE_NEW](https://vimc.github.io/vimcheck/reference/constants.md).

## Value

A `<ggplot2>` object that can be viewed or saved.
