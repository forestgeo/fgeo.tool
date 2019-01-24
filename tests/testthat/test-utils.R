context("utils")

test_that("utils pad_dbl() outputs equal to stringr::str_pad()", {
  skip_if_not_installed("stringr")
  string <- c(04, 14, 24, 34, 03, 13, 23, 33, 02, 12, 22, 32, 01, 11, 21, 31)
  width <- 5
  pad <- 0
  expect_equal(
    pad_dbl(string, width, pad),
    stringr::str_pad(string, width = width, pad = pad)
  )
})
