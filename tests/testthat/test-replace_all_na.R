context("replace_all_na")

df <- tibble::tibble(x = c(NA, 1), y = c("a", NA))

test_that("errs with wrong input", {
  expect_error(replace_all_na(1))
})

test_that("returns expected value", {
  out <- replace_all_na(df, "z")
  expect_equal(out$x[[1]], "z")
  expect_equal(out$y[[2]], "z")
})
