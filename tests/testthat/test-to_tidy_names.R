context("to_tidy_names")

test_that("works as unnamed character string", {
  skip_if_not_installed("fgeo.tool")

  string <- "Hi mE"
  expect_equal(fgeo.tool::nms_tidy(string), to_tidy_names(string))
  expect_equal(to_tidy_names(string), "hi_me")
})



context("anchor")

test_that("outputs the expected string", {
  expect_equal(anchor(letters[1:3]), c("^a$", "^b$", "^c$"))
})
