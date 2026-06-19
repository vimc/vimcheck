# Prepare impact diagnostics for plotting

A suite of helper functions that sit between impact diagnostics
functions and plotting functions. These functions transform and
aggregate impact estimates to prepare them for visualisation. Functions
have basic checks on input data but otherwise assume users will not
modify inputs.

## Usage

``` r
prep_plot_mod_grp_varn(df2, df3, outcome = IMPACT_OUTCOMES)

prep_plot_vax_gavi(
  data,
  prev_data,
  outcome = IMPACT_OUTCOMES,
  touchstone_old = DEF_TOUCHSTONE_OLD,
  touchstone_new = DEF_TOUCHSTONE_NEW
)

prep_plot_cumul(
  data,
  outcome,
  disease,
  touchstone_old = DEF_TOUCHSTONE_OLD,
  touchstone_new = DEF_TOUCHSTONE_NEW
)
```

## Arguments

- df2:

  A `<tibble>` of impact estimates with at least columns
  `modelling_group`, `vaccine`, outcome variable, and `fvps` (doses
  delivered). Used as the primary data source for calculations in
  `prep_plot_mod_grp_varn()`.

- df3:

  A `<tibble>` of modelling group and vaccine combinations, typically
  with one row per modelling group per vaccine. Joined with `df2` to
  ensure complete group coverage in `prep_plot_mod_grp_varn()`.

- outcome:

  A character string for the impact outcome. Must be one of
  `"deaths_averted"` or `"dalys_averted"`. For `prep_plot_cumul()`,
  `data` must include columns named `{outcome}_old` and `{outcome}_new`.

- data:

  A `<tibble>` of impact estimates with columns including at least those
  in
  [COLNAMES_KEY_PRESSURE_TEST](https://vimc.github.io/vimcheck/reference/constants.md),
  the outcome variable, and potentially other columns for analysis.

- prev_data:

  A `<tibble>` of impact estimates from a previous touchstone, used as a
  comparison baseline. Should have the same structure as `data`.

- touchstone_old:

  A six-character touchstone identifier (YYYYMM format) for the previous
  dataset. Defaults to
  [DEF_TOUCHSTONE_OLD](https://vimc.github.io/vimcheck/reference/constants.md).
  Used in `prep_plot_vax_gavi()` and `prep_plot_cumul()`.

- touchstone_new:

  A six-character touchstone identifier (YYYYMM format) for the current
  dataset. Defaults to
  [DEF_TOUCHSTONE_NEW](https://vimc.github.io/vimcheck/reference/constants.md).
  Used in `prep_plot_vax_gavi()` and `prep_plot_cumul()`.

- disease:

  A character string specifying a single disease for filtering and
  analysis.

## Value

- `prep_plot_mod_grp_varn()` returns a grouped `<tibble>` (grouped by
  `vaccine`) with all columns from `df2` and `df3` plus derived columns:
  `adj_outc` (adjusted outcome with small offset), `outcome_name` (input
  outcome), and `mean_outc` (vaccine-level weighted mean outcome).

- `prep_plot_vax_gavi()` returns a `<tibble>` with columns `disease`,
  `year`, `yearly_outcome`, `dataset` (factor with levels for old
  touchstone, "Difference", and new touchstone), and `outcome_name`.
  Summarizes outcomes by disease and year across two touchstones.

- `prep_plot_cumul()` returns a `<tibble>` with columns `year`,
  `modelling_group`, `touchstone`, `value` (cumulative or average
  outcome), `line_type` ("solid" for individual models, "dashed" for
  model average), and `outcome_name`. Returns `NULL` if the specified
  disease has no non-zero data to plot.
