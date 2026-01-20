# Check incoming burden cohort size against interpolated population

Check the modelled disease burden data has similar population sizes as
the provided population data.

## Usage

``` r
check_demography_alignment(
  burden_set,
  wpp,
  gender = c("Both", "Male", "Female")
)
```

## Arguments

- burden_set:

  A `<data.frame>` of modeller-provided burden-set data.

- wpp:

  Population estimates for the country in `burden_set`, provided by
  VIMC.

- gender:

  The assigned sex for which demography is to be checked. Options are
  `"Both"` (default), `"Male"`, or `"Female"`.

## Value

A `<tibble>` giving the alignment, i.e., percentage difference of
modelled population size from the WPP-derived population estimates.
