context("tool_example")

test_that("outputs expected path", {
  expect_is(tool_example("csv"), "character")
})
