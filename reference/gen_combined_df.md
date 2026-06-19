# Combine and align data from two touchstones

Generates a full join of two data.frames, selecting for columns of
interest.

## Usage

``` r
gen_combined_df(
  prev_dat,
  df_clean,
  interest_cols = COLNAMES_INTEREST_PRESSURE_TEST,
  key_cols = COLNAMES_KEY_PRESSURE_TEST
)
```

## Arguments

- prev_dat:

  A data.frame of impact estimates corresponding to an earlier
  touchstone.

- df_clean:

  A data.frame of impact estimates corresponding to a more recent
  touchstone.

- interest_cols:

  A character vector of columns of interest. Defaults to
  [COLNAMES_INTEREST_PRESSURE_TEST](https://vimc.github.io/vimcheck/reference/constants.md).

- key_cols:

  A character vector of columns of interest. Defaults to
  [COLNAMES_KEY_PRESSURE_TEST](https://vimc.github.io/vimcheck/reference/constants.md).

## Value

A data.frame which is a full join of `prev_dat` and `df_clean`. Columns
are disambiguated with the suffixes `"_old"` and `"_new"`.
