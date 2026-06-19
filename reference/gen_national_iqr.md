# Generate IQR for key outcomes

Generate IQR for key outcomes

## Usage

``` r
gen_national_iqr(
  df,
  group_cols = c("country", "vaccine", "activity_type"),
  value_cols = c("deaths_averted", "dalys_averted"),
  prefix = "national_iqr"
)
```

## Arguments

- df:

  A data.frame of impact estimates.

- group_cols:

  A character vector of grouping columns. Defaults to "country",
  "vaccine", "activity_type".

- value_cols:

  A character vector of value columns. Defaults to "deaths_averted" and
  "dalys_averted".

- prefix:

  A string for the prefix applied to every IQR summary column. Defaults
  to "national_iqr".

## Value

A `<data.frame>` with the inter-quartile range of the columns in
`value_cols`, with the column name constructed as `{prefix}_{value_col}`
using string interpolation.
