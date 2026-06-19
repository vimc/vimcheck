# Explore significant changes in deaths and DALYs

Explore significant changes in deaths and DALYs

## Usage

``` r
generate_diffs(
  prev_df,
  curr_df,
  interest_cols = COLNAMES_INTEREST_PRESSURE_TEST,
  key_cols = COLNAMES_KEY_PRESSURE_TEST,
  touchstone = DEF_TOUCHSTONE_OLD
)
```

## Arguments

- prev_df:

  A `<data.frame>` of impact estimates from the previous touchstone.

- curr_df:

  A `<data.frame>` of impact estimates for the current touchstone.

- interest_cols:

  A character vector of columns to check for differences. Defaults to
  [COLNAMES_INTEREST_PRESSURE_TEST](https://vimc.github.io/vimcheck/reference/constants.md).

- key_cols:

  A character vector of columns to use when assigning campaign
  identifiers. Passed to
  [`add_campaign_id()`](https://vimc.github.io/vimcheck/reference/add_campaign_id.md),
  defaults to
  [COLNAMES_KEY_PRESSURE_TEST](https://vimc.github.io/vimcheck/reference/constants.md).

- touchstone:

  A six character string that can be converted to a six digit numeric
  giving a touchstone identifier in `YYYYMM` format.

## Value

A list of tibbles of differences between `prev_df` and `curr_df`, with
one list element per element of `interest_cols`.
