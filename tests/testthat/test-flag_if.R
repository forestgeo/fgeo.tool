context("flag_if")

test_that("flag_if flaggs if predicate is true", {
  dupl <- c(1, 1)
  expect_warning(flag_if(dupl, is_duplicated))
  expect_silent(flag_if(dupl, is_multiple))

  mult <- c(1, 2)
  expect_message(flag_if(mult, is_multiple, message, "Custom"), "Custom")
  expect_silent(flag_if(mult, is_duplicated))

  expect_silent(flag_if(c(1, NA), is_multiple))
  expect_silent(flag_if(c(1, NA), is_duplicated))
})


.df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)

test_that("flags a variable with multiple values", {
  expect_silent(flag_if(.df, "b", is_multiple))
  expect_warning(flag_if(.df, "a", is_multiple))
  expect_error(flag_if(.df, "a", is_multiple, stop, "Custom"), "Custom")
})

test_that("is insensitive to name-case", {
  expect_error(flag_if(.df, "A", is_multiple, stop, "Custom"), "Custom")
  expect_silent(flag_if(.df, "B", is_multiple, stop, "Custom"))
})

test_that("returns `cond`", {
  msg <- "Flagged values were detected"
  dfm <- function(x) data.frame(Name = x, stringsAsFactors = TRUE)
  .data <- dfm(c(1, 1))
  expect_warning(flag_if(.data, "Name", is_duplicated, warning, msg))

  expect_error(flag_if(.data, "Name", is_duplicated, stop, msg))
  .data <- dfm(c(1, 2))
  expect_silent(flag_if(.data, "Name", is_duplicated, stop))
})

test_that("includes in the message the name of the variable being tested", {
  tree <- data.frame(treeID = c(1, 1), stringsAsFactors = FALSE)
  expect_warning(flag_if(tree, "treeID", is_duplicated), "treeid")
})

test_that("doesn't deal directly with grouped data to work within groups", {
  skip_if_not_installed("dplyr")
  library(dplyr)

  # Single within groups but multiple accross entire dataset
  .df <- tibble(a = c(1, 1, 2, 2), b = c(1, 1, 2, 2))

  by_a <- group_by(.df, a)
  warn_if_b_is_multiple <- function(.data) flag_if(.data, "b", is_multiple)
  expect_warning(warn_if_b_is_multiple(by_a), "Flagged values")
})
