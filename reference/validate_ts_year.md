# Check and return touchstone year-month

Check and return touchstone year-month

## Usage

``` r
validate_ts_year(x)
```

## Arguments

- x:

  A string for the touchstone identifier.

## Value

The first 6 characters of `x` converted to a numeric. Also has side
effects of erroring if conditions on `x` are not met.
