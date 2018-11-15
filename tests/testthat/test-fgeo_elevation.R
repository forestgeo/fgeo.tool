context("fgeo_elevation")

elev_df <- fgeo.x::elevation$col
.elev <- fgeo_elevation(elev_df)

test_that("has class elev", {
  expect_is(.elev, "fgeo_elevation")
})

test_that("does nothing if structure is OK", {
  expect_identical(fgeo_elevation(.elev), .elev)
})

test_that("deals only with data.frame and list", {
  expect_silent(fgeo_elevation(elev_df))
  expect_silent(fgeo_elevation(list(col = elev_df)))
  expect_error(fgeo_elevation(1), "numeric")
  expect_error(fgeo_elevation("a"), "character")
})

test_that("errs with wrong list", {
  elev_list <- list(bad = elev_df, superbad = 1)
  expect_error(
    fgeo_elevation(elev_list)
  )
})

test_that("fixes names", {
  expect_equal(names(.elev), c("gx", "gy", "elev"))
})
