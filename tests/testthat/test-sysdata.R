context("test-sysdata.R")

test_that("sysdata contains objects", {
  expect_error(missing_object) 
  expect_silent(forestr::functions_priority)
})
