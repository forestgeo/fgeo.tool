context("create_habitat.R")

test_that("outputs a dataframe", {
  hab <- create_habitat(fgeo.data::luquillo_elevation)
  expect_true(has_class_df(hab))
  expect_silent(check_crucial_names(hab, c("x", "y", "habitats")))
  expect_false(dplyr::is_grouped_df(hab))
})
