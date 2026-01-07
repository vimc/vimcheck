# Basic checks on internal functions. These are mostly checked via exported
# functions for a smaller testing surface
test_that("`make_novax_scenario()` works", {
  disease <- "dummy"
  expect_no_condition(
    make_novax_scenario(disease)
  )
  df <- make_novax_scenario(disease)
  expect_named(
    df,
    file_dict_colnames
  )
})
