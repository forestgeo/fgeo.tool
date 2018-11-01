context("rdata_df")

test_that("warns if directory has no .rdata file", {
  zero <- tool_example("csv")
  expect_warning(rdata_df(zero), "Can't find.*rdata")
  
  empty <- tool_example("empty")
  expect_warning(rdata_df(empty), "Can't find.*rdata")
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

test_that("is sensitive to `match`", {
  dfm <- rdata_df(tool_example("rdata"), match = 6, .id = "id")
  expect_equal(unique(dfm$id), "tree6")
  dfm <- rdata_df(tool_example("rdata"), match = "5|6", .id = "id")
  expect_equal(unique(dfm$id), c("tree5", "tree6"))
})
