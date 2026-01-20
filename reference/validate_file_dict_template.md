# Validate file dictionary template

Function to create a `file_dictionary` template. It maps to touchstone
disease scenarios and you will see expected number of scenarios i.e. the
number of files that we expect from a model. Users should populate the
file column to match the scenario-file. This function will run if a
`file_dictionary.csv` file does not exist

## Usage

``` r
validate_file_dict_template(disease, path_burden = "incoming_burden_estimates")
```

## Arguments

- disease:

  A disease identifier.

- path_burden:

  A directory with burden estimate data.

## Value

Nothing; called primarily for its side-effects. If the file
`path_burden/file_dictionary.csv` does not exist, a file dictionary CSV
file is written to the same location. Prints a message to screen
informing the user whether any action has been taken.
