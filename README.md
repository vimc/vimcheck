
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vimc.rpkg.template: TAGLINE

<!-- badges: start -->

[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build
status](https://github.com/vimc/vimc.rpkg.template/workflows/R-CMD-check/badge.svg)](https://github.com/vimc/vimc.rpkg.template/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/vimc/vimc.rpkg.template/branch/main/graph/badge.svg)](https://app.codecov.io/gh/vimc/vimc.rpkg.template?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/vimc.rpkg.template)](https://CRAN.R-project.org/package=vimc.rpkg.template)
<!-- badges: end -->

*vimc.rpkg.template* is a template package and repository on which
future VIMC R packages are based.

To use this template, select *vimc.rpkg.template* from the drop-down
menu under **Repository template** when creating a new repository in the
VIMC GitHub organisation. Replace all instances of
`"vimc.rpkg.template"` with your package name. Make sure to also:

1.  Edit the `DESCRIPTION` as appropriate with the correct package
    information, including authors;

2.  Edit the files in `R/`, `tests/`, and `vignettes/` to suit your
    package;

3.  Add spell check by running
    `usethis::use_spell_check(lang = "en-GB")`;

4.  Remove these instructions from `README.Rmd`, and re-render the `.md`
    file using `devtools::render_readme()`.

## Installation

**NOTE:** Remove or comment out installation sources as appropriate.

**Remember** to add the package to the [VIMC
R-universe](https://github.com/vimc/vimc.r-universe.dev).

You can install the development version of *vimc.rpkg.template* from the
VIMC R-universe with:

``` r
installation from R-universe
install.packages(
  "vimc.rpkg.template", 
  repos = c(
    "https://vimc.r-universe.dev", "https://cloud.r-project.org"
  )
)
```

or from GitHub [GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("vimc/vimc.rpkg.template")
```

## Quick start

Add a simple example of using the package’s main feature(s) here, with a
minimum amount of code. If preparatory or plotting steps are needed,
prefer to hide them to keep focus on the package functionality.

## Related projects

Add information and links to related projects, such as research papers
or packages, here.

## References

Space for references: REMOVE this text.
