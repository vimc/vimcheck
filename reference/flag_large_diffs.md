# Flag significant changes in impact estimates

Calculates and flags whether the difference in impact estimates between
touchstones is greater than expected. A row is flagged if the difference
is greater than `threshold` \\\times\\ the inter-quartile range for
cases where the IQR is greater than zero.

## Usage

``` r
flag_large_diffs(
  changes_list,
  iqr_df,
  variable = c("deaths_averted", "dalys_averted"),
  group_cols = c("country", "vaccine", "activity_type"),
  threshold = 100,
  touchstone_old = DEF_TOUCHSTONE_OLD_OLD,
  touchstone_new = DEF_TOUCHSTONE_NEW
)
```

## Arguments

- changes_list:

  A list of data.frames with one element per variable of interest (see
  `variable`). Usually generated using
  [`generate_diffs()`](https://vimc.github.io/vimcheck/reference/generate_diffs.md).

- iqr_df:

  A data.frame of inter-quartile differences generated using
  [`gen_national_iqr()`](https://vimc.github.io/vimcheck/reference/gen_national_iqr.md).

- variable:

  A string specifying the variable of interest. Must be one of
  "deaths_averted" or "dalys_averted", and must be present as a name and
  element of `changes_list`.

- group_cols:

  A character vector of grouping columns. Defaults to "country",
  "vaccine", "activity_type".

- threshold:

  A single numeric value for the IQR multiplier. Defaults to 100.

- touchstone_old:

  The previous touchstone identifier. Defaults to
  [DEF_TOUCHSTONE_OLD_OLD](https://vimc.github.io/vimcheck/reference/constants.md).

- touchstone_new:

  The new touchstone identifier. Defaults to
  [DEF_TOUCHSTONE_NEW](https://vimc.github.io/vimcheck/reference/constants.md).

## Value

A filtered data.frame of differences in impact estimates flagged as too
large. Rows with differences within tolerance are removed.
