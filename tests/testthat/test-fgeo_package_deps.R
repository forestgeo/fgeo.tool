context("fgeo_package_deps.R")

test_that("returns the expected string", {
  skip_on_travis()
  root_from_testthat <- "../../../"
  expect_true(
    any(
      grepl("fgeo", fgeo_package_deps("fgeo.utils", root = root_from_testthat))
    )
  )
})
