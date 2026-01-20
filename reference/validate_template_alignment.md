# Check incoming burden set against template

Check incoming burden set against template

## Usage

``` r
validate_template_alignment(burden_set, template)
```

## Arguments

- burden_set:

  A `<data.frame>` of modeller-provided burden-set data.

- template:

  A `<data.frame>` of the burden template as provided to modelling
  groups by VIMC.

## Value

A named list of checks carried out on `burden_set` to compare it against
`template`, with information on missing and extra data.
