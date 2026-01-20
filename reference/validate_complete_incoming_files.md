# Validate files in a burden estimate

Check that incoming data files in a burden estimate are complete, and
that no extra files have been included. This function expects that
incoming burden files are in the directory given by `path_burden`, which
holds a file dictionary which maps each data file to a specific
scenario.

## Usage

``` r
validate_complete_incoming_files(path_burden = "incoming_burden_estimates")
```

## Arguments

- path_burden:

  A directory with burden estimate data.

## Value

A `<tibble>` of the scenario file dictionary in `path_burden` if all
checks pass. Otherwise, exits with informative errors on failed checks.
