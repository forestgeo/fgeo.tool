library(dplyr)

context("add_var")

test_that("converts as the equivalent function from the CFTSR Package", {
  x <- tibble(gx = 990:992, gy = 490:492)

  w_lxly <- suppressMessages(add_var(x, var = "lxly"))
  actual <- select(w_lxly, lx, ly)
  expected <- as_tibble(gxgy_to_lxly(x$gx, x$gy, 20, c(1000, 500)))
  expect_equal(actual, expected)

  w_qxqy <- suppressMessages(add_var(x, var = "qxqy"))
  actual <- select(w_qxqy, QX, QY)
  expected <- as_tibble(gxgy_to_qxqy(x$gx, x$gy, 20, c(1000, 500)))
  expect_equal(actual, expected)
  
  w_colrow <- suppressMessages(
    add_var(x, var = "colrow")
  )
  actual <- select(w_colrow, row, col) %>% purrr::map_df(as.numeric)
  expected <- as_tibble(gxgy_to_rowcol(x$gx, x$gy, 20, c(1000, 500)))
  expect_equal(actual, expected)

  w_idx <- suppressMessages(add_var(x, var = "index"))
  actual <- select(w_idx, index)
  expected <- tibble(index = gxgy_to_index(x$gx, x$gy, 20, c(1000, 500)))
  expect_equal(actual, expected)

  w_hi <- suppressMessages(add_var(x, var = "hectindex"))
  actual <- select(w_hi, hectindex)
  expected <- tibble(hectindex = gxgy_to_hectindex(x$gx, x$gy, c(1000, 500)))
  expect_equal(actual, expected)
})

test_that("informs what's going on", {
  x <- tibble(gx = -1, gy = 1)
  expect_error(
    add_var(x, var = "lxly")
  )
  x <- tibble(gx = NA, gy = 1)
  expect_error(
    add_var(x, var = "lxly")
  )
})

context("add_quad")

test_that("outputs equal to ctfs::gxgy.to.quad()", {
  x <- tibble(gx = c(5, 25), gy = c(5, 5))

  skip_if_not_installed("ctfs")
  skip_on_travis()

  now <- add_quad(x, start = 1, width = 2)$quad
  ctfs <- ctfs::gxgy.to.quad(x$gx, x$gy, start = "one", digits = 2)
  expect_equal(now, ctfs)

  now <- add_quad(x, start = 0, width = 2)$quad
  ctfs <- ctfs::gxgy.to.quad(x$gx, x$gy, start = "zero", digits = 2)
  expect_equal(now, ctfs)

  x <- bciex::bci12t7mini
  now <- add_quad(x, start = 0, width = 2)$quad
  ctfs <- ctfs::gxgy.to.quad(x$gx, x$gy, start = "zero", digits = 2)
  expect_equal(now, ctfs)
})



context("add_col_row2")

x <- tibble::tribble(
  ~QuadratName,
  "0001",
  "0011",
  "0101",
  "1001"
)
x

test_that("adds expected names", {
  expect_true(
    all(
      c("col", "row")  %in%  names(add_col_row2(x))
    )
  )
})

test_that("with wrong inputs aborts", {
  expect_error(
    add_col_row2(1)
  )
  expect_error(
    add_col_row2(dplyr::rename(x, odd_nm = QuadratName))
  )
})
