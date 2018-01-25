context("restructure_elev")

elev_df <- bciex::bci_elevation
elev_clean <- restructure_elev(elev_df)

test_that("fixes names", {
  expect_equal(names(elev_clean), c("gx", "gy", "elev"))
})

test_that("does nothing if structure is OK", {
  expect_identical(restructure_elev(elev_clean), elev_clean)
})

test_that("works with a list", {
  elev_list <- list(col = elev_df)
  expect_identical(restructure_elev(elev_list), elev_clean)
})

test_that("errs with wrong list", {
  elev_list <- list(bad = elev_df, superbad = 1)
  expect_error(
    restructure_elev(elev_list)
  )
})

