# Plotting theme for vimcheck

A simple plotting theme building on
[`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Usage

``` r
theme_vimc(x_text_angle = 45, y_text_angle = 0, ...)

theme_vimc_noxaxis()
```

## Arguments

- x_text_angle:

  The angle for X-axis labels. Defaults to 45 degrees.

- y_text_angle:

  The angle for Y-axis labels. Defaults to 0 degrees.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Other arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).
  These will be applied in addition to, or in place of, pre-existing
  elements defined by this theme. See the examples for this theme's
  appearance.

## Value

A `ggplot2` theme that can be added to `ggplot2` plots or objects.

## Examples

``` r
# using an inbuilt dataset
data(mtcars)

# standard theme
ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
  ggplot2::geom_point() +
  theme_vimc()


# with X-axis suppression
ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
  ggplot2::geom_point() +
  theme_vimc_noxaxis()

```
