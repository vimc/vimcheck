# Example of FVP estimate data

Example data of fully-vaccinated persons (FVPs).

## Usage

``` r
eg_fvps
```

## Format

### `eg_fvps`

A data frame with 11 rows and 24 columns:

- scenario_type:

  Scenario type name.

- scenario_type_description:

  Scenario type description string.

- scenario:

  Scenario name string.

- scenario_description:

  Scenario description string.

- coverage_set:

  Coverage set string.

- gavi_support_level:

  String for whether GAVI supported the scenario.

- source_from:

  String identifier for the source.

- disease:

  Infection identifier.

- vaccine:

  Vaccine identifier.

- activity_type:

  Vaccination activity identifier.

- year:

  Year

- country:

  Country name in short format; this is a placeholder name.

- age_from:

  Age limit lower limit.

- age_to:

  Age limit upper limit.

- age_range_verbatim:

  Description of age range.

- target:

  Target for vaccination.

- coverage:

  Proportional coverage.

- gender:

  Sex to which data applies, may be "Male", "Female", or "Both".

- proportion_risk:

  Proportional risk value.

- job:

  Job code as a numeric.

- fvps:

  Count of FVPs.

- fvps_adjusted:

  Count of adjusted FVPs.

- target_adjusted:

  Adjusted vaccination target.

- coverage_adjusted:

  Ratio of adjusted FVPs to adjusted target.

## Source

Prepared by the VIMC secretariat.
