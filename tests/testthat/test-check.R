# Check inputs ------------------------------------------------------------

context("check_unique_vector")

single <- rep(1, 3)
num <- c(1:3)
chr <- c(letters[1:3])

test_that("behaves as expected", {
  expect_silent(check_unique_vector(single, "warning"))
  expect_warning(check_unique_vector(num, "warning"))
  expect_error(check_unique_vector(chr, "stop", "Do something"), "Do something")
})



context("check_unique")

.df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)

test_that("returns the expected messages and output x", {
  expect_warning(check_unique(.df, "a"))
  expect_error(check_unique(.df, "a", "stop", "do this"), "do this")
  expect_silent(out <- check_unique(.df, "b", "stop", "do this"))
  expect_identical(.df, out)
})
