context("fgeo_elev")

elev_df <- bciex::bci_elevation
.elev <- fgeo_elev(elev_df)

test_that("has class elev", {
  expect_is(.elev, "fgeo_elev")
})

test_that("does nothing if structure is OK", {
  expect_identical(fgeo_elev(.elev), .elev)
})

test_that("deals only with data.frame and list", {
  expect_silent(fgeo_elev(elev_df))
  expect_silent(fgeo_elev(list(col = elev_df)))
  expect_error(fgeo_elev(1), "numeric")
  expect_error(fgeo_elev("a"), "character")
})

test_that("errs with wrong list", {
  elev_list <- list(bad = elev_df, superbad = 1)
  expect_error(
    fgeo_elev(elev_list)
  )
})

test_that("fixes names", {
  expect_equal(names(.elev), c("gx", "gy", "elev"))
})
