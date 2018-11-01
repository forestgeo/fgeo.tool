context("rdata_df")

library(glue)
library(fs)

#' dir(path_dir)
#' 
#' rdata_df(path_dir)
#' 
#' rdata_df(path_dir, match = "tree6")
#' 
#' dfm <- rdata_df(path_dir, match = "tree5|6", .id = "source")
#' dfm
#' tail(dfm)
NULL

path_dir <- tool_example("rdata")

test_that("read_rdata() can read a single .rdata file", {
  input <- read_rdata(fs::dir_ls(path_dir)[[1]])
  expect_is(input, "data.frame")
  # expect_error(
  #   read_rdata(empty_dir), 
  #   glue("Can't find any .rdata to read in {empty_dir}")
  # )
  
})

test_that("rdata_df() can handle cero, and two .rdata files", {
  zero <- tool_example("csv")
  expect_warning(rdata_df(zero), "Can't find in.*any file")
  
  empty <- tool_example("empty")
  expect_warning(rdata_df(empty), "Can't find in.*any file")
  
  one <- tool_example("rdata_one")
  expect_silent(rdata_df(one))
  
  two <- rdata_df(path_dir)
  expect_is(two, "data.frame")
})

