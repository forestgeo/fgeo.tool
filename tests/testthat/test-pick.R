context("map_mindbh")

library(purrr)
library(tidyr)
library(tibble)
library(dplyr)

cns_lst <- as_censuses(list(
  c1 = tibble::tibble(dbh = 1:2),
  c2 = tibble::tibble(dbh = 8:9)
))

lst <- list(
  c1 = tibble::tibble(dbh = 1:2, census = 1),
  c2 = tibble::tibble(dbh = 8:9, census = 2)
)
dfm <- reduce(lst, bind_rows)
ndf <- dfm %>% 
  dplyr::group_by(census) %>% 
  nest() %>% 
  as_censuses()


test_that("errs with informative message", {
  expect_error(pick(1), "Can't deal with data")
  expect_error(pick(data.frame(x = 1)), "Can't deal with data")
  expect_error(pick(tibble::tibble(x = 1)), "Can't deal with data")
  expect_error(pick(list(x = 1)), "Can't deal with data")
  
  expect_error(pick(cns_lst, novar > 1), "'novar' not found")
  expect_error(pick(cns_lst, dbh == 1, key = "bad"), "'bad' not found")
  expect_error(pick(ndf, dbh == 1, key = "bad"), "'bad' not found")
  
  # Pick as is. Passes
  expect_error(pick(cns_lst), NA)
})

test_that("with simplest inputs returns the expected data structure", {
  expect_error(out <- pick(cns_lst, dbh == 1), NA)
  expect_is(out, "censuses_lst")
  expect_is(out, "list")
  expect_named(out[[1]], "dbh")
})

test_that("picking a row in key census picks the same row in other censuses", {
  out <- pick(cns_lst, dbh == 1, key = "c1")
  expect_equal(out[[1]]$dbh, 1)
  expect_equal(out[[2]]$dbh, 8)
  
  out <- pick(cns_lst, dbh == 2, key = "c1")
  expect_equal(out[[1]]$dbh, 2)
  expect_equal(out[[2]]$dbh, 9)

  out <- pick(cns_lst, dbh == 9, key = "c2")
  expect_equal(out[[1]]$dbh, 2)
  expect_equal(out[[2]]$dbh, 9)
  
  out <- pick(cns_lst, dbh == 9 | dbh < 9, key = "c2")
  expect_equal(out[[1]]$dbh, 1:2)
  expect_equal(out[[2]]$dbh, 8:9)
  
  out <- pick(cns_lst, dbh == 0, key = "c1")
  expect_equal(out[[1]]$dbh, integer(0))
  expect_equal(out[[2]]$dbh, integer(0))
})

test_that("with the simplest call returns the expected data structure", {
  out <- pick(ndf, dbh == 1)
  expect_is(out, "censuses_df")
})

test_that("works with nested dataframe and numeric nesting-group", {
  
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
    nest() %>% 
    as_censuses()
  
  # Pick rows over the mean
  mean_dbh <- mean(censuses$data[[1]]$dbh, na.rm = TRUE)
    expect_error(out <- pick(censuses, dbh > mean_dbh), NA)
  expect_is(out, "censuses_df")
  
  expect_true(nrow(censuses$data[[1]]) > nrow(out$data[[1]]))
  expect_error(pick(censuses, dbh > mean_dbh, key = "tree6"), NA)
})
