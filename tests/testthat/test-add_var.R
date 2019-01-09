# Input quadratname or quadrat --------------------------------------------

context("add_gxgy")

set.seed(1)

test_that("add_gxgy w/ ViewFullTable fails gracefully if missing quadratname", {
  vft <- fgeo.x::vft_4quad
  vft$QuadratName <- NULL
  expect_error(add_gxgy(vft), "Ensure.*quadrat")
})

test_that("add_gxgy w/ census table fails gracefully if missing quadrat", {
  tree <- fgeo.x::tree5
  tree$quadrat <- NULL
  expect_error(add_gxgy(tree), "Ensure.*quadrat")
})

test_that("add_gxgy handles 0-row input", {
  tree <- fgeo.x::tree5[0, ]
  expect_equal(nrow(add_gxgy(tree)), 0)
  expect_equal(ncol(add_gxgy(tree)), ncol(tree) + 2)
})

test_that("add_gxgy handles factors", {
  tree1 <- fgeo.x::tree5[1, ]
  tree1$quadrat <- as.factor(100)

  tree2 <- fgeo.x::tree5[1, ]
  tree2$quadrat <- 100L

  expect_equal(
    add_gxgy(tree1)$gx1,
    add_gxgy(tree2)$gx1
  )
})

test_that("add_gxgy handles NA", {
  tree <- fgeo.x::tree5[1, ]
  tree$quadrat <- NA
  expect_true(is.na(add_gxgy(tree)$gx1))
})

test_that("add_gxgy with a viewfulltable outputs a data.frame", {
  vft <- dplyr::sample_n(fgeo.x::vft_4quad, 10)
  expect_is(add_gxgy(vft), "data.frame")
})

test_that("add_gxgy with a viewfulltable outputs the expected names", {
  vft <- dplyr::sample_n(fgeo.x::vft_4quad, 10)
  out <- add_gxgy(vft)

  expect_true(
    all(c("QuadratName", "gx", "gy") %in% names(out))
  )
})

test_that("add_gxgy handles potentially duplicated names and avoids them", {
  skip_if_not_installed("ctfs")

  tree <- dplyr::sample_n(fgeo.x::tree5, 10)
  out <- add_gxgy(tree)
  expect_true(all(c("quadrat", "gx1", "gy1") %in% names(out)))

  expect_equivalent(
    add_gxgy(tree)[["gx1"]], ctfs::quad.to.gxgy(tree$quadrat)$gx
  )
  expect_equivalent(
    add_gxgy(tree)[["gy1"]], ctfs::quad.to.gxgy(tree$quadrat)$gy
  )
})

test_that("add_gxgy with viewfulltable outputs equal to ctfs::quad.to.gxgy()", {
  skip_if_not_installed("ctfs")

  vft <- dplyr::sample_n(fgeo.x::vft_4quad, 10)
  expect_equal(
    add_gxgy(vft)[c("gx", "gy")], ctfs::quad.to.gxgy(vft$QuadratName)
  )
})

# Input gxgy --------------------------------------------------------------

x <- tribble(
  ~gx, ~gy,
  0, 0,
  50, 25,
  999.9, 499.95,
  1000, 500
)

gridsize <- 20
plotdim <- c(1000, 500)



context("add_lxly")

test_that("works with `px`, `py`", {
  expect_equal(
    add_lxly(tibble(px = 1, py = 1), gridsize, plotdim)[c("lx", "ly")],
    add_lxly(tibble(gx = 1, gy = 1), gridsize, plotdim)[c("lx", "ly")]
  )
})

test_that("informs plotdim if not explicitely given", {
  expect_silent(add_lxly(tibble(gx = 1, gy = 1), gridsize, plotdim))
  expect_message(
    add_lxly(tibble(gx = 1, gy = 1), gridsize),
    "Guessing: plotdim"
  )
  expect_message(
    add_lxly(tibble(gx = 1, gy = 1), gridsize),
    "If.*wrong.*provide.*plotdim"
  )
})



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

test_that("aborts bad start", {
  expect_error(
    add_quad(x, gridsize, plotdim, start = "bad"),
    "must be `NULL` or `0000`"
  )
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
