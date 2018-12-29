context("add_var")

x <- tribble(
    ~gx,    ~gy,
      0,      0,
     50,     25,
  999.9, 499.95,
   1000,    500
)

gridsize <- 20
plotdim <- c(1000, 500)



context("add_lxly")

test_that("outputs equivalent to ctfs analog", {
  expect_equivalent(
    add_lxly(x, gridsize, plotdim)[c("lx", "ly")],
    gxgy_to_lxly(x$gx, x$gy, gridsize, plotdim)
  )
})



context("add_qxqy")

test_that("outputs equivalent to ctfs analog", {
  expect_equivalent(
    add_qxqy(x, gridsize, plotdim)[c("QX", "QY")],
    gxgy_to_qxqy(x$gx, x$gy, gridsize, plotdim)
  )
})



context("add_index")

test_that("returns equal to ctfs analog", {
  skip_if_not_installed("ctfs")

  expect_equal(
    suppressWarnings(add_index(x, plotdim = plotdim))[["index"]],
    ctfs::gxgy.to.index(x$gx, x$gy, gridsize, plotdim)
  )
})



context("add_hectindex")

test_that("returns equal to ctfs analog", {
  skip_if_not_installed("ctfs")

  expect_equal(
    suppressWarnings(add_hectindex(x))[["hectindex"]],
    ctfs::gxgy.to.hectindex(x$gx, x$gy, plotdim)
  )
})

context("add_quad")

test_that("returns equal to ctfs analog", {
  skip_if_not_installed("ctfs")
  
  expect_equal(
    add_quad(x, gridsize, plotdim, start = 0)[["quad"]],
    ctfs::gxgy.to.quad(x$gx, x$gy, gridsize, plotdim, start = "zero")
  )
  
  expect_warning(add_quad(x), NA)
})



context("add_var")

test_that("with ViewFullTable, it outputs the original names plus lx/ly", {
  x <- fgeo.x::vft_4quad

  expect_named(add_var(x, "lxly"), c(names(x), c("lx", "ly")))
  # Other top level functions
  expect_named(add_lxly(x), c(names(x), c("lx", "ly")))
  expect_named(add_col_row(x), c(names(x), c("col", "row")))
  expect_named(add_index(x), c(names(x), "index"))
})

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

  x <- fgeo.x::tree5
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

test_that("adds expected names", {
  expect_named(add_col_row2(x), c("QuadratName", "col", "row"))
})

test_that("with wrong inputs aborts", {
  expect_error(
    add_col_row2(1)
  )
  expect_error(
    add_col_row2(dplyr::rename(x, odd_nm = QuadratName))
  )
})
