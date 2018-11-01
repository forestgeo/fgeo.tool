context("map_mindbh")

library(purrr)
library(tidyr)
library(tibble)
library(dplyr)



lst <- list(
  c1 = tibble::tibble(dbh = 1:2),
  c2 = tibble::tibble(dbh = 8:9)
)

test_that("picking a row in key census picks the same row in other censuses", {
  out <- pick_in_sync(lst, dbh == 1, key = 1)
  expect_equal(out[[1]]$dbh, 1)
  expect_equal(out[[2]]$dbh, 8)
  
  out <- pick_in_sync(lst, dbh == 2, key = 1)
  expect_equal(out[[1]]$dbh, 2)
  expect_equal(out[[2]]$dbh, 9)

  out <- pick_in_sync(lst, dbh == 9, key = 2)
  expect_equal(out[[1]]$dbh, 2)
  expect_equal(out[[2]]$dbh, 9)
  
  out <- pick_in_sync(lst, dbh == 9 | dbh < 9, key = 2)
  expect_equal(out[[1]]$dbh, 1:2)
  expect_equal(out[[2]]$dbh, 8:9)
  
  out <- pick_in_sync(lst, dbh == 0, key = 1)
  expect_equal(out[[1]]$dbh, integer(0))
  expect_equal(out[[2]]$dbh, integer(0))
  
  
  
  lst <- list(
    c1 = tibble::tibble(dbh = 1:2, census = 1),
    c2 = tibble::tibble(dbh = 8:9, census = 2)
  )
  
  dfm <- reduce(lst, bind_rows)
  ndf <- dfm %>% dplyr::group_by(census) %>% nest()
  
  out <- pick_in_sync(ndf, dbh == 1, key = 1)
  expect_equal(out$data[[1]]$dbh, 1)
  expect_equal(out$data[[2]]$dbh, 8)
  
})













