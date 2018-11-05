context("censuses")

test_that("outputs an object of class 'censuses_df'", {
  expect_is(read_censuses(tool_example("rdata")), "censuses_df")
})
