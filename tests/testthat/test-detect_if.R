context("detect_if")

test_that("works with any case", {
  dfm <- data.frame(CensusID = c(1, 2, NA))
  expect_true(detect_if(dfm, "censusid", is_multiple))
  expect_false(detect_if(dfm, "censusid", is_duplicated))

  dfm <- data.frame(CensusID = c(1, 1))
  expect_true(detect_if(dfm, "censusid", is_duplicated))
  expect_false(detect_if(dfm, "censusid", is_multiple))

  dfm <- data.frame(CensusID = c(1, 1, 2))
  expect_true(detect_if(dfm, "censusid", is_duplicated))
  expect_true(detect_if(dfm, "censusid", is_multiple))
})

test_that("rejects invalid var", {
  dfm <- data.frame(CensusID = c(1, 2, NA))
  expect_error(detect_if(dfm, "bad", is_multiple), "invalid name")
  expect_error(detect_if(dfm, "bad", is_duplicated), "invalid name")
})

dfm <- function(x) data.frame(Name = x, stringsAsFactors = TRUE)
test_that("creates a function that detects duplicates on a specific variable", {
  expect_true(detect_if(dfm(c(1, 1)), "Name", is_duplicated))
  expect_false(detect_if(dfm(c(1, NA)), "Name", is_duplicated))
  expect_false(detect_if(dfm(c(1, 2)), "Name", is_duplicated))
})

test_that("works with upper or lowercase name", {
  expect_true(detect_if(dfm(c(1, 1)), "Name", is_duplicated))
  expect_true(detect_if(dfm(c(1, 1)), "name", is_duplicated))

  expect_false(detect_if(dfm(c(1, 2)), "Name", is_duplicated))
  expect_false(detect_if(dfm(c(1, 2)), "name", is_duplicated))
})

test_that("ignores groups but groups can be handled via map(nest()$data)", {
  skip_if_not_installed("tidyr")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("purrr")
  library(tidyr)
  library(dplyr)
  library(purrr)

  dfm <- data.frame(x = c(1, 1), g = c(1, 2), stringsAsFactors = TRUE)
  expect_true(detect_if(group_by(dfm, g), "x", is_duplicated))
  grouped <- group_by(dfm, g)
  expect_false(any(
    map_lgl(nest(grouped)$data, ~ detect_if(.x, "x", is_duplicated))
  ))
})
