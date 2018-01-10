context("sysdata.R")

test_that("sysdata contains objects", {
  expect_error(missing_object)
  expect_silent(fgeo.tool:::functions_priority)
})
