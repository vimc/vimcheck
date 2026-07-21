# Plot coverage and fully vaccinated persons (FVPs)

Generates plots of routine vaccine coverage and fully vaccinated persons
(FVPs) over time for selected countries.

## Usage

``` r
plot_coverage_fvps(fvps, country = PINE)
```

## Arguments

- fvps:

  A data.frame (or class extending it) showing the number of FVPs (fully
  vaccinated persons) by country, year and scenario/activity type.

- country:

  A character vector of country identifiers, with all identifiers
  expected to be found in `fvps`. Defaults to PINE countries.

## Value

A named list with two ggplot objects:

- coverage:

  A plot of routine vaccine coverage over time.

- fvps:

  A plot of fully vaccinated persons over time.

If there is no data on routine vaccination in the dataset, the
`coverage` element of the return will be an empty `<ggplot>` object, and
a warning is thrown.

## Examples

``` r
fvps <- eg_fvps_2

plots <- plot_coverage_fvps(fvps, "AGO")
plots$coverage

plots$fvps

```
