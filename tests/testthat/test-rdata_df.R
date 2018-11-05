context("rdata_df")

test_that("errs with informative message if directory has no .rdata file", {
  zero <- tool_example("csv")
  expect_error(rdata_df(zero), "Can't find.*rdata")
})

test_that("can handle and two .rdata files", {
  one <- tool_example("rdata_one")
  expect_silent(rdata_df(one))
  
  two <- rdata_df(tool_example("rdata"))
  expect_is(two, "data.frame")
})

test_that("with non-null `.id` adds new column", {
  dfm <- rdata_df(tool_example("rdata"), .id = "id")
  expect_equal(names(dfm)[[1]], "id")
})

test_that("is sensitive to `.match`", {
  ## FIXME: These tests pass test() but not check() and I don't know why.
  # dfm <- rdata_df(tool_example("rdata"), .match = "5", .id = "src")
  # expect_equal(unique(dfm$src), "tree5")
  # 
  # dfm <- rdata_df(tool_example("rdata"), .match = "6", .id = "src")
  # expect_equal(unique(dfm$src), "tree6")

  dfm <- rdata_df(tool_example("rdata"), .match = "5|6", .id = "src")
  expect_equal(unique(dfm$src), c("tree5", "tree6"))
})
