# Filter data for touchstones or diseases

A pair of helper functions allowing filtering out of recent touchstone
values and excluded diseases.

## Usage

``` r
filter_recent_ts(df, threshold = DEF_TOUCHSTONE_NEW)

filter_excluded_diseases_ts(df, threshold = DEF_TOUCHSTONE_OLD_OLD)

flag_duplicates(df, key_cols = COLNAMES_KEY_PRESSURE_TEST)

filter_invalid_trajectories(
  df,
  prev_data,
  outcome = c("deaths_averted", "dalys_averted")
)
```

## Arguments

- df:

  A `<data.frame>` holding impact data. This data.frame is not checked
  for contents

- threshold:

  A six-digit number that is checked as a valid touchstone identifier
  (YYYYMM format) using
  [`validate_ts_year()`](https://vimc.github.io/vimcheck/reference/validate_ts_year.md).
  Defaults to
  [DEF_TOUCHSTONE_NEW](https://vimc.github.io/vimcheck/reference/constants.md)
  (`"202310"`).

- key_cols:

  Key columns in `df` to check for duplicates.

- prev_data:

  A `<data.frame>` holding data from a previous touchstone for the same
  scenarios as `df`.

- outcome:

  A string giving the outcome of interest; may be one of
  `"deaths_averted"` or `"dalys_averted"`.

## Value

A filtered `<data.frame>`.

- `filter_recent_ts()` returns `df` with rows where the touchstone
  condition is not met excluded.

- `filter_excluded_diseases_ts()` returns `df` with rows where rows
  relating to the
  [EXCLUDED_DISEASES](https://vimc.github.io/vimcheck/reference/constants.md),
  when the touchstone year in `df` is less than the `threshold`,
  excluded.

- `flag_duplicates()` returns `df` with duplicated combinations of
  `key_cols` flagged using the column `n_key` (or a user-defined name).

- `filter_invalid_trajectories()` returns `df` with bad outcome
  trajectories (`NA` to non-`NA`) removed.
