context("example_path_dir")

test_that("outputs expected path", {
  expect_is(example_path_dir("two_files/new_stem_1.xlsx"), "character")
})
