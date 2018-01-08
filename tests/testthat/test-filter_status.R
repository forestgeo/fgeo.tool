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



context("filter_status.R")

test_that("correctly handles wrong inputs", {
  not_chr <- 1

  expect_error(filter_status("wrong"))
  expect_error(
    filter_status(fgeo, "stem", not_chr)
  )
  expect_warning(
    filter_status(fgeo, "stem", "wrong")
  )
  expect_error(
    filter_status(fgeo, not_chr, "alive")
  )
  expect_error(
    filter_status(fgeo, "wrong", "alive")
  )
  expect_error(
    filter_status(fgeo, "stem", "alive", "not logical")
  )
})

test_that("returns data of correct class", {
  out <- filter_status(fgeo, "stem", "alive")
  expect_true(any(grepl("data.frame", class(out))))
})

test_that("returns expected values", {
  out <- filter_status(fgeo, "stem", "alive")
  expect_equal(
    unique(out$Status), "alive"
  )

  out <- filter_status(fgeo, "stem", "dead")
  expect_equal(
    unique(out$Status), "dead"
  )

  w_status_tree <- fgeo %>% add_status_tree("alive", "dead")
  out <- filter_status(w_status_tree, "tree", "dead")
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
  expect_warning(filter_status(w_na, "stem", "dead"))
})




context("stem_not_dead")

test_that("shortcut returns the same as filter_status", {
  expect_equal(
    filter_status(fgeo, "stem", "dead", exclude = TRUE),
    stem_not_dead(fgeo, "dead")
  )
})



context("tree_not_dead")

test_that("shortcut returns the same as filter_status", {
  w_status_tree <- add_status_tree(fgeo, "alive", "dead")
  long <- filter_status(w_status_tree, "tree", "dead", exclude = TRUE)
  short <- tree_not_dead(w_status_tree, "dead")
  expect_equal(long, short)
})

