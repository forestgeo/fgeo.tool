context("operators")

test_that("outputs as expected", {
  expect_identical(1 %||% 2, 1)
  expect_identical(NULL %||% 2, 2)
  expect_identical(NULL %||% NA, NA)
  expect_identical(NA %||% 2, NA)
})
