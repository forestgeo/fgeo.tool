context("replace_null_by_na.R")

test_that("works well with logical, integer, double, character", {
  expect_equal(replace_null_by_na(c('a', 'NULL')), c('a', NA))
  expect_equal(replace_null_by_na(c(1, 'NULL')), c('1', NA))
  
  expect_equal(replace_null_by_na(c(TRUE, FALSE)), c(TRUE, FALSE))
  expect_equal(replace_null_by_na(c(1L, 2L)), c(1L, 2L))
  expect_equal(replace_null_by_na(c(1.1, 2.1)), c(1.1, 2.1))
})

test_that("works well with dataframe and matrix", {
  dfm <- tibble(x = c("a", "NULL"), y = 1:2)
  expect_equal(replace_null_by_na(dfm)$x, c('a', NA))
  
  expect_equal(replace_null_by_na(matrix(c(1, "NULL"))), c("1", NA))
})

test_that("errs with undefined classes", {
  expect_error(replace_null_by_na(list(1)))
})
