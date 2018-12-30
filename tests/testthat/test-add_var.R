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
    c("0000", "0201", "4924", NA) 
  )
  expect_equal(
    ctfs::gxgy.to.quad(x$gx, x$gy, gridsize, plotdim, start = "zero"),
    c("0000", "0201", "4924", "NANA") 
  )
})

test_that("no longer warns that missing values are introduces by coersion", {
  expect_warning(add_quad(x), NA)
})

test_that("is sensitive to `start`", {
  expect_equal(add_quad(x, gridsize, plotdim, start = 0)$quad[[1]], "0000")
  expect_equal(add_quad(x, gridsize, plotdim, start = NULL)$quad[[1]], "0101")
  expect_equal(add_quad(x, gridsize, plotdim)$quad[[1]], "0101")
})

test_that("edge quadrats are `NA` not 'NA'", {
  expect_true(is.na(dplyr::last(add_quad(x)$quad)))
})



context("add_col_row")

test_that("returns equal to ctfs analog", {
  skip_if_not_installed("ctfs")
  
  expect_equivalent(
    purrr::modify(add_col_row(x)[c("row", "col")], as.numeric),
    ctfs::gxgy.to.rowcol(x$gx, x$gy)
  )
})



context("add_col_row2")

test_that("outputs the same as add_col_row", {
   ctfs <- ctfs::gxgy.to.rowcol(x$gx, x$gy)
   col_row <- tibble(
     QuadratName = paste_colrow(
       pad_dbl(ctfs$col, width = 2, pad = 0),
       pad_dbl(ctfs$row, width = 2, pad = 0)
     )
   )
   expect_equivalent(
     purrr::modify(add_col_row2(col_row)[c("row", "col")], as.numeric),
     ctfs
   )
   
})



context("add_col_row2")

test_that("outputs the extected data structure", {
  x <- tibble::tribble(
    ~QuadratName,
          "0001",
          "0011",
          "0101",
          "1001"
  )
 expect_named(add_col_row2(x), c("QuadratName", "col", "row"))
 expect_is(add_col_row2(x), "tbl")
  
})

test_that("aborts with wrong inputs", {
  
  expect_error(
    add_col_row2(1), 
    "must be a data.frame"
  )
  
  expect_error(
    add_col_row2(tibble(bad_name = "0001")),
    "Ensure.*quadratname"
  )
})
