library(dplyr)

fgeo <- tribble(
  ~CensusID, ~Tag, ~Status,
         1,    1, "alive",
         1,    1,  "dead",
         1,    2,  "dead",
         1,    2,  "dead",
         2,    1, "alive",
         2,    1, "alive",
         2,    2, "alive",
         2,    2,  "dead"
)



context("row_filter_status.R")

test_that("correctly handles wrong inputs", {
  not_chr <- 1

  expect_error(row_filter_status("wrong"))
  expect_error(
    row_filter_status(fgeo, "stem", not_chr)
  )
  expect_warning(
    row_filter_status(fgeo, "stem", "wrong")
  )
  expect_error(
    row_filter_status(fgeo, not_chr, "alive")
  )
  expect_error(
    row_filter_status(fgeo, "wrong", "alive")
  )
  expect_error(
    row_filter_status(fgeo, "stem", "alive", "not logical")
  )
})

test_that("returns data of correct class", {
  out <- row_filter_status(fgeo, "stem", "alive")
  expect_true(any(grepl("data.frame", class(out))))
})

test_that("returns expected values", {
  out <- row_filter_status(fgeo, "stem", "alive")
  expect_equal(
    unique(out$Status), "alive"
  )

  out <- row_filter_status(fgeo, "stem", "dead")
  expect_equal(
    unique(out$Status), "dead"
  )

  w_status_tree <- fgeo %>% add_status_tree("alive", "dead")
  out <- row_filter_status(w_status_tree, "tree", "dead")
  expect_equal(unique(out$status_tree), "dead")
  expect_equal(unique(out$Status), "dead")
  expect_equal(unique(out$CensusID), 1)
  expect_equal(unique(out$Tag), 2)
})

test_that("Warns when ignoring NA(s)", {
  w_na <- tribble(
    ~CensusID, ~Tag, ~Status,
            1,    1, "alive",
            1,    1,  "dead",
            1,    2,  "dead",
            1,    2,  "dead",
            2,    1, "alive",
            2,    1, "alive",
            2,    2, "alive",
            2,    2,  "dead",
            2,    2,  NA
  )
  expect_warning(row_filter_status(w_na, "stem", "dead"))
})




context("row_keep_alive_stem")

test_that("shortcut returns the same as row_filter_status", {
  expect_equal(
    row_filter_status(fgeo, "stem", "dead", exclude = TRUE),
    row_keep_alive_stem(fgeo, "dead")
  )
})



context("row_keep_alive_tree")

test_that("shortcut returns the same as row_filter_status", {
  w_status_tree <- add_status_tree(fgeo, "alive", "dead")
  long <- row_filter_status(w_status_tree, "tree", "dead", exclude = TRUE)
  short <- row_keep_alive_tree(w_status_tree, "dead")
  expect_equal(long, short)
})

