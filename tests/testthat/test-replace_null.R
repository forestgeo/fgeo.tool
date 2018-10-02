context("replace_null.R")

test_that("leaves dates unchanged", {
  dfm <- tibble::tibble(x = lubridate::as_date("1993-08-12"))
  expect_is(fgeo.tool::replace_null(dfm)$x, "date")
})

test_that("works well with logical, integer, double, character", {
  before <- c("a", "NULL")
  expect_equal(replace_null(before),  c("a", NA))
  expect_equal(replace_null(before, "after"), c("a", "after"))
  
  expect_equal(replace_null(c(TRUE, FALSE)), c(TRUE, FALSE))
  expect_equal(replace_null(c(1L, 2L)), c(1L, 2L))
  expect_equal(replace_null(c(1.1, 2.1)), c(1.1, 2.1))
})

test_that("works well with dataframe", {
  before <- c("a", "NULL")
  after <- c("a", NA)
  
  dfm <- tibble::tibble(x = before, y = 1:2)
  expect_equal(replace_null(dfm)$x, after)
  dfm <- data.frame(x = before, y = 1:2, stringsAsFactors = FALSE)
  expect_equal(replace_null(dfm)$x, after)
  
  # Data with factors
  dfm <- data.frame(x = before, y = 1:2)
  expect_warning(
    expect_equal(replace_null(dfm)$x, after)
  )
  expect_warning(
    expect_equal(
      replace_null(dfm, "after")$x, 
      c("a", "after")
    )
  )
})

test_that("works well with matrix", {
  before <- c("a", "NULL")
  after <- c("a", NA)
  expect_equal(replace_null(matrix(before)), after)
})

test_that("works with factors", {
  expect_warning(replace_null(factor(c("a", "NULL"))))
})

test_that("errs with undefined classes", {
  expect_error(replace_null(list(1)))
})
