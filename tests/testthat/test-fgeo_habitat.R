context("fgeo_habitat.R")

test_that("outputs object with number of rows equal to number of quadrats", {
  elev_luq <- fgeo.data::luquillo_elevation
  hab <- fgeo_habitat(elev_luq, gridsize = 20, n = 4)
  
  plotdim <- c(320, 500)
  gridsize <- 20
  rows <- plotdim[[1]] / gridsize
  cols <- plotdim[[2]] / gridsize
  expect_equal(nrow(hab), rows * cols)
  
  # Reference: This habitat dataset was created by the authors of tt_test()
  habitat_pasoh <- pasoh::pasoh_hab_index20
  pd <- c(1000, 500)
  gs <- 20
  rw <- pd[[1]] / gs
  cl <- pd[[2]] / gs
  expect_equal(nrow(habitat_pasoh), rw * cl)
})

test_that("it rounds any gx and gy with accuracy given by gridsize", {
  elev <- tibble::tribble(
    ~x, ~y, ~elev,
     0,  0,    89,
    11,  0,    99
  )
  
  gsz <- 20
  out <- fgeo_habitat(elev, gridsize = gsz, n = 1, xdim = 100, ydim = 100)
  expect_equal(out$gx, c(0, 20))
  expect_equal(out$gx, c(0, 20))
})

test_that("it works with data from bci", {
  skip_if_not_installed("bciex")
  elev <- bciex::bci_elevation
  expect_error(
    fgeo_habitat(elev, gridsize = 20, 2),
    "`xdim` and `ydim` can't be missing if `elevation` is a data.frame"
  )
  expect_silent(fgeo_habitat(elev, gridsize = 20, 2, xdim = 1000, ydim = 500))
  bci_elev_ls <- list(col = elev, xdim = 1000, ydim = 500)
  expect_silent(fgeo_habitat(bci_elev_ls, gridsize = 20, n = 4))
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
  expect_error(
    fgeo_habitat(fgeo.data::luquillo_elevation$col), 
    "can't be missing"
  )
  
  elev_missing_xydims <- list(col = fgeo.data::luquillo_elevation$col)
  expect_error(fgeo_habitat(elev_missing_xydims), "Ensure your data set")
  expect_error(fgeo_habitat(fgeo.data::luquillo_elevation, 20), "is missing")
})
