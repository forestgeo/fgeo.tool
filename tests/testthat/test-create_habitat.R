context("create_habitat.R")

test_that("outputs a dataframe with expected structure", {
  skip_if_not_installed("fgeo.data")
  skip_on_travis()
  hab <- create_habitat(fgeo.data::luquillo_elevation, 20, 4)
  expect_true(has_class_df(hab))
  expect_silent(check_crucial_names(hab, c("x", "y", "habitats")))
  expect_false(dplyr::is_grouped_df(hab))
  expect_equal(hab, fgeo.data::luquillo_habitat)
})

test_that("errs with informative messages", {
  expect_error(
    create_habitat(),
    "is not TRUE"
  )
  expect_error(
    create_habitat(fgeo.data::luquillo_elevation),
    "is not TRUE"
  )
  expect_error(
    create_habitat(fgeo.data::luquillo_elevation, 20),
    "is not TRUE"
  )
})
