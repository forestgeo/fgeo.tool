context("create_habitat.R")

test_that("outputs a dataframe with expected structure", {
  skip_if_not_installed("fgeo.data")
  skip_on_travis()
  hab <- create_habitat(fgeo.data::luquillo_elevation, 20, 4)
  expect_is(hab, "data.frame")
  expect_silent(fgeo.base::check_crucial_names(hab, c("x", "y", "habitats")))
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

test_that("outputs correct habitat data from bciex", {
  bci_elev <- list(col = bciex::bci_elevation, xdim = 1000, ydim = 500)
  bci_hab <- fgeo.tool::create_habitat(bci_elev, 20, 4)
  last_row <- unname(unlist(bci_hab[nrow(bci_hab), c("x", "y")]))
  expect_equal(last_row, c(980, 480))
})


