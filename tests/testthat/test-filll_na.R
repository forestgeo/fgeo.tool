context("fill_na")

df <- tibble::tibble(x = c(NA, 1), y = c("a", NA))

test_that("returns expected value", {
  out <- fill_na(df, "z")
  expect_equal(out$x[[1]], "z")
  expect_equal(out$y[[2]], "z")
})
