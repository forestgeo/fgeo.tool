library(dplyr)

context("status_stem")

test_that("checks inputs as expected", {
  expect_error(
    status_stem(x = "wrong-type")
  )

  wrong_type <- 1
  expect_error(
    status_stem(x = tibble(a = 1), .status = wrong_type)
  )
  expect_error(
    status_stem(x = tibble(missing_name = 1), .status = "correct type")
  )
  expect_error(
    status_stem(x = tibble(status = "A"), .status = "wrong status")
  )
})
