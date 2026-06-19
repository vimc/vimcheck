# Adaptively round numerics

Adaptively round numerics

## Usage

``` r
adaptive_round(x, large_threshold = 1, small_sigfig = 2, large_digits = 1)
```

## Arguments

- x:

  A numeric vector.

- large_threshold:

  A single number for the threshold over which numbers are to be
  considered 'large'.

- small_sigfig:

  A single number for the number of significant digits for 'small'
  numbers.

- large_digits:

  A single number for the number of places to which 'large' numbers
  should be rounded.

## Value

`x` rounded to either `large_digits` or to `small_sigfig`.
