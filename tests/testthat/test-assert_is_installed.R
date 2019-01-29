context("assert_is_installed")

test_that("if pkg is not installed assert_is_installed throws an error", {
  expect_error(assert_is_installed("bad"), "Please install")
})

test_that("if pkg is installed assert_is_installed returns invisible pkg", {
  expect_silent(out <- assert_is_installed("base"))
  expect_equal(out, "base")
})
