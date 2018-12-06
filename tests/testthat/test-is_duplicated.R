context("is_duplicated")

test_that("returns true if any value of a variable is duplicated", {
  expect_true(is_duplicated(c(1, 1)))
  expect_false(is_duplicated(c(1, NA)))
  expect_false(is_duplicated(c(1, 2)))
})



context("is_multiple")

test_that("detects multiple different values of a variable", {
  expect_true(is_multiple(c(1, 2)))
  expect_false(is_multiple(c(1, NA)))
  expect_false(is_multiple(c(1, 1)))
})
