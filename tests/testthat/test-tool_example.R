context("tool_example")

test_that("outputs expected path", {
  expect_is(tool_example("two_files/new_stem_1.xlsx"), "character")
})
