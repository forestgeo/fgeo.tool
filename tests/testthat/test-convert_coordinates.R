library(dplyr)
x <- tibble(
  gx = 100:102,
  gy = 100:102
)

context("add_coord")

test_that("converts as expected", {
  actual <- select(add_coord(x = x, from = "gxgy", to = "lxly"), lx, ly)
  expected <- as_tibble(ctfs::gxgy.to.lxly(x$gx, x$gy))
  expect_equal(actual, expected)
})
