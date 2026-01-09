#' Validate file dictionary template
#'
#' @description
#' Function to create a `file_dictionary` template.
#' It maps to touchstone disease scenarios and you will see expected number of
#' scenarios i.e. the number of files that we expect from a model.
#' Users should populate the file column to match the scenario-file.
#' This function will run if a `file_dictionary.csv` file does not exist
#'
#' @param disease A disease identifier.
#'
#' @param path_burden A directory with burden estimate data.
#'
#' @return Nothing; called primarily for its side-effects.
#' If the file `path_burden/file_dictionary.csv` does not exist, a file
#' dictionary CSV file is written to the same location.
#' Prints a message to screen informing the user whether any action has been
#' taken.
#'
#' @keywords diagnostics
#'
#' @export
validate_file_dict_template <- function(
  disease,
  path_burden = "incoming_burden_estimates"
) {
  # NOTE: maybe need to check allowed options for disease?

  checkmate::assert_string(disease)
  checkmate::assert_directory_exists(path_burden)
  template <- file.path(path_burden, "file_dictionary.csv")

  if (file.exists(template)) {
    # TODO: check that file_dictionary entries are acceptable?
    data_dict <- readr::read_csv(
      template,
      show_col_types = FALSE
    )
    is_good_df <- checkmate::test_data_frame(
      data_dict,
      any.missing = FALSE,
      min.cols = length(file_dict_colnames),
      min.rows = 2L # needs a no-vax and a vax scenario?
    ) &&
      checkmate::test_names(
        colnames(data_dict),
        must.include = file_dict_colnames
      )

    if (is_good_df) {
      cli::cli_inform(
        "File dictionary found at {.file {template}}, no action needed."
      )
    } else {
      cli::cli_abort(
        c(
          "File dictionary found at {.file {template}}, \\
          but it is not well formed. Please check info below.",
          i = "Column names: {.str {colnames(data_dict)}}, \\
          expected min. column names: {.str {file_dict_colnames}};
          No. rows: {nrow(data_dict)}, expected min. no. rows: 2."
        )
      )
    }
  } else {
    # NOTE: see expected dir structure in `tests/testthat/testdata/`
    scenario_dir <- file.path(path_burden, "model_inputs")
    checkmate::assert_directory_exists(scenario_dir)

    scenario_data <- file.path(scenario_dir, "scenario.csv")
    checkmate::assert_file_exists(scenario_data)

    sce <- readr::read_csv(scenario_data, show_col_types = FALSE)

    # NOTE: consider wrapping into check function
    checkmate::assert_data_frame(
      sce,
      any.missing = TRUE, # allowing missing as contained in examples
      min.cols = length(scenario_data_colnames),
    )
    checkmate::assert_names(
      colnames(sce),
      must.include = scenario_data_colnames
    )

    # get distinct scenario entries and add no-vax if needed
    sce <- dplyr::select(sce, {{ scenario_data_colnames }})
    sce <- dplyr::distinct(sce)

    novax_scenario <- "novac"

    if (!checkmate::test_subset(novax_scenario, sce$scenario_type)) {
      sce <- dplyr::bind_rows(
        sce,
        make_novax_scenario(disease)
      )
    }

    sce$file <- NA_character_ # NOTE: remove file
    readr::write_csv(sce, template)

    cli::cli_inform(
      "No file dictionary found at {.file {template}}; created a file \\
      dictionary and wrote it to {.file {template}}."
    )
  }

  # no return
}

#' Validate files in a burden estimate
#'
#' @description
#' Check that incoming data files in a burden estimate are complete, and that
#' no extra files have been included.
#' This function expects that incoming burden files are in the
#' directory given by `path_burden`, which holds a file dictionary which maps
#' each data file to a specific scenario.
#'
#' @inheritParams validate_file_dict_template
#'
#' @return A `<tibble>` of the scenario file dictionary in `path_burden` if all
#' checks pass. Otherwise, exits with informative errors on failed checks.
#'
#' @keywords diagnostics
#'
#' @export
validate_complete_incoming_files <- function(
  path_burden = "incoming_burden_estimates"
) {
  checkmate::assert_directory_exists(path_burden)

  files <- list.files(path_burden, full.names = TRUE)
  file_dict <- file.path(path_burden, "file_dictionary.csv")

  # checks on the file dictionary
  if (file.exists(file_dict)) {
    df_dict <- readr::read_csv(
      file_dict,
      show_col_types = FALSE
    )

    col_filenames <- "file"
    scenario_filenames <- df_dict[[col_filenames]]
    df_dict <- dplyr::select(df_dict, -{{ col_filenames }})

    duplicate_rows <- anyDuplicated(df_dict)
    duplicate_filenames <- anyDuplicated(scenario_filenames)

    if (duplicate_rows) {
      cli::cli_abort(
        "Expected file dictionary {.file {file_dict}} to have non-duplicate \\
        entries, but found {duplicate_rows} duplicated rows!"
      )
    }
    if (duplicate_filenames) {
      cli::cli_abort(
        "Expected file dictionary {.file {file_dict}} to have non-duplicate \\
        scenario filenames, but found {duplicate_filenames} duplicated \\
        filenames!"
      )
    }

    # expect scenario files in path_burden are the same as scenario_filenames
    sce_files <- files[files != file_dict]
    scenario_filenames <- file.path(path_burden, scenario_filenames)
    are_good_scefiles <- checkmate::test_permutation(
      scenario_filenames,
      sce_files
    )

    if (!are_good_scefiles) {
      extra_files <- setdiff(sce_files, scenario_filenames)
      n_extra_files <- length(extra_files)
      missing_files <- setdiff(scenario_filenames, sce_files)
      n_missing_files <- length(missing_files)

      cli::cli_abort(
        c(
          "Expected as many scenario data files as scenarios \\
          ({length(scenario_filenames)}), but found \\
          {.emph {length(sce_files)}} instead.",
          i = "Found {cli::no(n_extra_files)} extra files{? /}\\
          {.file {basename(extra_files)}}, {cli::qty(n_missing_files)}
          {?but could not find/and} \\
          {cli::no(n_missing_files)} missing files{? /} \\
          {.file {basename(missing_files)}}",
          i = "Directory searched: {.file {path_burden}}"
        ),
      )
    }
  } else {
    cli::cli_abort(
      "Expected a file dictionary {.file {file_dict}}, but it was not found!"
    )
  }

  df_dict
}

#' Check incoming burden set against template
#'
#' @description
#'
#' @param burden_set A `<data.frame>` of modeller-provided burden-set data.
#'
#' @param template A `<data.frame>` of the burden template as provided to
#' modelling groups by VIMC.
#'
#' @return A named list of checks carried out on `burden_set` to compare it
#' against `template`, with information on missing and extra data.
#'
#' @keywords diagnostics
#'
#' @export
validate_template_alignment <- function(burden_set, template) {
  checkmate::assert_data_frame(burden_set)
  checkmate::assert_data_frame(template)

  expected <- colnames(template)
  provided <- colnames(burden_set)

  missing_cols_in_burden <- setdiff(expected, provided)
  extra_cols_in_burden <- setdiff(provided, expected)

  # explicitly check each length
  burden_cols_matches_template <- length(missing_cols_in_burden) +
    length(extra_cols_in_burden) ==
    0L

  key_cols <- c("disease", "country", "year", "age")
  template_grid <- dplyr::distinct(
    template,
    dplyr::across({
      key_cols
    })
  )
  burden_grid <- dplyr::distinct(
    burden_set,
    dplyr::across({
      key_cols
    })
  )

  missing_grid_in_burden <- dplyr::setdiff(template_grid, burden_grid)
  extra_grid_in_burden <- dplyr::setdiff(burden_grid, template_grid)
  burden_grid_matches_template <- all(
    c(
      nrow(missing_grid_in_burden),
      nrow(extra_grid_in_burden)
    ) ==
      0L
  )

  list(
    missing_cols_in_burden = missing_cols_in_burden,
    extra_cols_in_burden = extra_cols_in_burden,
    burden_cols_matches_template = burden_cols_matches_template,
    missing_grid_in_burden = missing_grid_in_burden,
    extra_grid_in_burden = extra_grid_in_burden,
    burden_grid_matches_template = burden_grid_matches_template
  )
}

#' Check incoming burden cohort size against interpolated population
#'
#' @description Check the modelled disease burden data has similar population
#' sizes as the provided population data.
#'
#' @inheritParams validate_template_alignment
#'
#' @param wpp Population estimates for the country in `burden_set`, provided by
#' VIMC.
#'
#' @param gender The assigned sex for which demography is to be checked. Options
#' are `"Both"` (default), `"Male"`, or `"Female"`.
#'
#' @return A `<tibble>` giving the alignment, i.e., percentage difference of
#' modelled population size from the WPP-derived population estimates.
#'
#' @keywords diagnostics
#'
#' @export
check_demography_alignment <- function(
  burden_set,
  wpp,
  gender = c("Both", "Male", "Female")
) {
  # TODO: input checks
  checkmate::assert_data_frame(burden_set)
  checkmate::assert_data_frame(wpp)

  gender <- rlang::arg_match(gender)

  cols_to_select <- c("country", "year", "age", "cohort_size")
  provided <- dplyr::select(
    burden_set,
    {{ cols_to_select }}
  )
  provided <- dplyr::mutate(
    provided,
    provided = cohort_size
  )

  # TODO: explain what expected is
  # TODO: replace with a right-join?
  expected <- dplyr::filter(
    wpp,
    gender == {{ gender }}
  )

  # in case there are many extra cols
  cols_to_select <- c("country", "year", "age", "value")
  expected <- dplyr::select(
    expected,
    {{ cols_to_select }}
  )
  expected <- dplyr::rename(
    expected,
    expected = value
  )

  # return left join
  alignment <- dplyr::left_join(
    provided,
    expected,
    by = c("country", "year", "age")
  )
  alignment <- dplyr::mutate(
    alignment,
    difference = provided - expected,
    abs_diff = abs(difference),
    prop_diff = difference / expected
  )

  alignment
}

#' Sanity checks on burden estimates
#'
#' @description Helper function for sanity checks on burden estimate values.
#' Checks whether any burden estimates are non-numeric, missing, or negative.
#'
#' @param burden A `<data.frame>` of disease burden estimates. Must have
#' at least a single column named `"value"` of numeric burden estimates.
#'
#' @return A character vector of messages generated by checks on burden
#' estimates, with the length of the vector depending on how many checks fail.
#'
#' @keywords diagnostics
#'
#' @export
basic_burden_sanity <- function(burden) {
  # TODO: expectations on burden
  mes <- "Basic sanity check for burden estimates:"

  value_col <- "value"
  value <- burden[[value_col]]

  if (is.numeric(burden$value)) {
    if (anyNA(burden$value)) {
      mes_any_missing <- glue::glue(
        "Warning: Burden estimates should not have missing values, but some \\
        values are missing. Fix missing values by converting to zeros!"
      )

      mes <- c(mes, mes_any_missing)
    }

    if (any(burden$value < 0, na.rm = TRUE)) {
      mes_any_negative <- glue::glue(
        "Warning: Burden estimates should all be positive or zero, but found \\
        some negative estimates!"
      )

      mes <- c(mes, mes_any_negative)
    }
  } else {
    mes_not_numeric <- glue::glue(
      "Warning: Burden estimates should be of type `numeric`, but are not. \\
      Convert burden estimates to `numeric`."
    )

    mes <- c(mes, mes_not_numeric)
  }

  if (length(mes) == 1L) {
    mes <- c(mes, "PASS.")
  }

  mes
}

#' @title
#'
#' @description
#' A short description...
#'
#' @param coverage
#'
#' @param wpp
#'
#' @return
#'
#' @examples
#' # example code
#'
#' @importFrom dplyr .data
#'
#' @keywords diagnostics
#'
#' @export
transfrom_coverage_fvps <- function(coverage, wpp) {
  # TODO: checks on coverage
  # TODO: checks on wpp

  cols_to_select <- c("age_from", "age_to", "gender")
  todo_list <- dplyr::select(
    coverage,
    cols_to_select
  )
  todo_list <- dplyr::distinct(todo_list)
  todo_list <- dplyr::mutate(
    todo_list,
    job = seq_along(.data$gender)
  )

  # TODO: THIS NEEDS TO BE CLEANED UP
  # TODO: clarify structure of `coverage` and mapping of gender to age
  pop_all <- list()
  for (i in seq_along(todo_list$age_from)) {
    pop_all[[i]] <- wpp %>%
      x <- dplyr::filter(
      wpp,
      .data$age >= todo_list$age_from[i],
      .data$age <= todo_list$age_to[i],
      .data$gender == todo_list$gender[i]
    )
    x <- dplyr::group_by(x, .data$country, .data$year)
    x <- dplyr::summarise(
      x,
      target_wpp = sum(.data$value),
      .groups = "drop"
    )
    x <- dplyr::mutate(
      x,
      job = todo_list$job[i]
    )
  }
  pop_all <- dplyr::bind_rows(pop_all)

  # TODO: add comments or explain in fn docs
  d <- dplyr::left_join(
    coverage,
    pop_all,
    by = c("country", "year")
  )
  d <- dplyr::mutate(
    d,
    target = dplyr::coalesce(
      .data$target,
      .data$target_wpp # replace NAs in target with target_wpp
    ),
    fvps = .data$target * .data$coverage,
    fvps_adjusted = pmin(
      .data$target_wpp,
      .data$fvps
    ),
    target_adjusted = pmin(
      .data$target_wpp,
      .data$target
    ),
    coverage_adjusted = .data$fvps_adjusted / .data$target_adjusted
  )
  d[["target_wpp"]] <- NULL

  d
}

# TODO: fill out fn docs
#' @title
#'
#' @description
#'
#' @param burden
#'
#' @param scenario_order
#'
#' @return
#'
#' @importFrom dplyr .data
#'
#' @examples
#'
#' @keywords diagnostics
#'
#' @export
impact_check <- function(burden, scenario_order) {
  # TODO: input checks
  scenario_cols <- c("scenario", "scenario_order")
  scenario_order <- dplyr::select(scenario_order, {{ scenario_cols }})

  d <- dplyr::summarise(
    burden,
    millions = sum(.data$value) / 1e6,
    .by = c("scenario", "burden_outcome"),
    .groups = "drop" # probably unnecessary as grouping is temporary
  )

  d <- dplyr::left_join(
    d,
    scenario_order,
    by = "scenario"
  )

  d <- dplyr::mutate(
    d,
    scenario_order = glue::glue("{.data$scenario_order}:{.data$scenario}")
  )

  d$scenario <- NULL

  d <- tidyr::pivot_wider(
    d,
    names_from = "scenario_order",
    values_from = "million"
  )

  # TODO: CLEAN THIS UP
  for (i in 2:nrow(scenario_order)) {
    for (j in 1:(i - 1)) {
      if (any(d[i + 1] > d[j + 1])) {
        cat(sprintf(
          "&nbsp;&nbsp;&nbsp;&nbsp;**Warning**: provided less disease burden in lower coverage scenario (%s) compared to higher coverage scenario (%s).</span>",
          names(d)[j + 1],
          names(d)[i + 1]
        ))
        cat("<br>")
      } else {
        cat(sprintf(
          "&nbsp;&nbsp;&nbsp;&nbsp;**PASS**: Provided higher disease burden in scenarios with fewer FVPs.</span>"
        ))
        cat("<br>")
      }
    }
  }

  d
}
