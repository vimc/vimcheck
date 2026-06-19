# Compare sub-regional and national estimates

Compare sub-regional and national estimates

## Usage

``` r
compare_natl_subreg(
  df,
  outcome = c("deaths_averted_rate", "dalys_averted_rate"),
  activity_filter = c("campaign", "routine")
)
```

## Arguments

- df:

  A data.frame with sub-region level data on vaccination impact
  outcomes.

- outcome:

  A string for the outcome of interest. May be one of
  `"deaths_averted_rate"` or `"dalys_averted_rate"`.

- activity_filter:

  A string for the type of vaccination activity. May be one of
  `"campaign"` or `"routine"`.

## Value

A data.frame of sub-regional vaccination impact estimates where the
impact is considered to be outside the tolerance limit.
