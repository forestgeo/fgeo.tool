context("warn_na.R")

test_that("outputs warnings as expected", {
  expect_warning(warn_na(c(x = 1, y = NA)))
  expect_error(warn_na(c(x = 1, 1)))
})
