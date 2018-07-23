context("fgeo_habitat.R")

test_that("it works with data from bci", {
  skip_if_not_installed("bciex")
  elev <- bciex::bci_elevation
  expect_error(
    fgeo_habitat(elev, gridsize = 20, 2),
    "`xdim` and `ydim` can't be missing if `elevation is a data.frame"
  )
  expect_silent(fgeo_habitat(elev, gridsize = 20, 2, xdim = 1000, ydim = 500))
  expect_silent(out <- fgeo_habitat(list(col = elev), gridsize = 20, 2))
})


test_that("outputs a dataframe with expected structure", {
  skip_if_not_installed("fgeo.data")
  skip_on_travis()
  
  # A list
  hab <- fgeo_habitat(fgeo.data::luquillo_elevation, 20, 4)
  expect_is(hab, "data.frame")
  expect_is(hab, "fgeo_habitat")
  
  expect_silent(check_crucial_names(hab, c("gx", "gy", "habitats")))
  expect_false(dplyr::is_grouped_df(hab))
  expect_equal(hab, fgeo.data::luquillo_habitat)
  
  # A dataframe
  hab <- fgeo_habitat(
    fgeo.data::luquillo_elevation$col, gridsize = 20, n = 4, xdim = 320, 
    ydim = 500
  )
  expect_equal(hab, fgeo.data::luquillo_habitat)
  
  # An object of class fgeo_elevation
  elev <- fgeo_elevation(fgeo.data::luquillo_elevation)
  hab <- fgeo_habitat(elev, gridsize = 20, n = 4, xdim = 320, ydim = 500)
  expect_equal(hab, fgeo.data::luquillo_habitat)
})

test_that("errs with informative messages", {
  expect_error(fgeo_habitat(1), "Can't deal with data of class")
  expect_error(fgeo_habitat(fgeo.data::luquillo_elevation), "is missing")
  expect_error(fgeo_habitat(fgeo.data::luquillo_elevation$col), "is missing")
  expect_error(fgeo_habitat(fgeo.data::luquillo_elevation, 20), "is missing")
})
