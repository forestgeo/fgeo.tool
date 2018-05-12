context("example_path")

test_that("outputs expected path", {
  expect_is(example_path("two_files/new_stem_1.xlsx"), "character")
})
