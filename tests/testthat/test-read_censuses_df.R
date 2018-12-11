context("test-read_censuses_df")

test_that("outputs an object of class 'censuses_df'", {
  expect_is(read_censuses_df(tool_example("rdata")), "censuses_df")
})
