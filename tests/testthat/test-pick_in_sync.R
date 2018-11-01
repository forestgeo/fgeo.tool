context("map_mindbh")

library(purrr)
library(tidyr)
library(tibble)
library(dplyr)



lst <- list(
  c1 = tibble::tibble(dbh = 1:2),
  c2 = tibble::tibble(dbh = 8:9)
)

test_that("with simplest inputs returns the expected data structure", {
  expect_error(out <- pick(lst, dbh == 1), NA)
  expect_is(out, "list")
  expect_named(out[[1]], "dbh")
})

test_that("picking a row in key census picks the same row in other censuses", {
  
  out <- pick(lst, dbh == 1, key = 1)
  expect_equal(out[[1]]$dbh, 1)
  expect_equal(out[[2]]$dbh, 8)
  
  out <- pick(lst, dbh == 2, key = 1)
  expect_equal(out[[1]]$dbh, 2)
  expect_equal(out[[2]]$dbh, 9)

  out <- pick(lst, dbh == 9, key = 2)
  expect_equal(out[[1]]$dbh, 2)
  expect_equal(out[[2]]$dbh, 9)
  
  out <- pick(lst, dbh == 9 | dbh < 9, key = 2)
  expect_equal(out[[1]]$dbh, 1:2)
  expect_equal(out[[2]]$dbh, 8:9)
  
  out <- pick(lst, dbh == 0, key = 1)
  expect_equal(out[[1]]$dbh, integer(0))
  expect_equal(out[[2]]$dbh, integer(0))
})

test_that("works with nested dataframe and numeric nesting-group", {
  lst <- list(
    c1 = tibble::tibble(dbh = 1:2, census = 1),
    c2 = tibble::tibble(dbh = 8:9, census = 2)
  )
  dfm <- reduce(lst, bind_rows)
  ndf <- dfm %>% dplyr::group_by(census) %>% nest()
  
  out <- pick(ndf, dbh == 1, key = 1)
  expect_equal(out$data[[1]]$dbh, 1)
  expect_equal(out$data[[2]]$dbh, 8)
  
  out <- pick(ndf, dbh == 2, key = 1)
  expect_equal(out$data[[1]]$dbh, 2)
  expect_equal(out$data[[2]]$dbh, 9)
  
  out <- pick(ndf, dbh == 9, key = 2)
  expect_equal(out$data[[1]]$dbh, 2)
  expect_equal(out$data[[2]]$dbh, 9)
})

test_that("works with nested data and character nesting-group", {
  censuses <- rdata_df(tool_example("rdata"), .id = "census") %>% 
    group_by(census) %>% 
    nest()
  
  expect_error(out <- pick(censuses, dbh > 30), NA)
  expect_is(out, "tbl")
  
  expect_true(nrow(censuses$data[[1]]) > nrow(out$data[[1]]))
  expect_error(pick(censuses, dbh > 30, key = "tree6"), NA)
})
