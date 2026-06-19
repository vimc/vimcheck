# Example of impact data

Example of vaccine impact data taken from data used to test vimpact.
This data is primarily used for testing here too.

## Usage

``` r
eg_impact
```

## Format

### `eg_impact`

A data frame with 5396 rows and 9 columns:

- disease:

  Disease name.

- vaccine:

  Vaccine identifier.

- modelling_group:

  Modelling group name.

- country:

  Country ISO 3-character code.

- country_name:

  Country name.

- year:

  Year for which impacts are modelled.

- activity_type:

  Activity type: either "routine" or "campaign."

- burden_outcome:

  Name of the burden outcome; one of "deaths_averted" or
  "dalys_averted".

- impact:

  Value of the impact

## Source

Prepared by the VIMC secretariat.
