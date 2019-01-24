context("test-guess_plotdim.R")

test_that("outputs expected plotdim and message", {
  skip_if_not_installed("fgeo.data")

  x <- data.frame(gx = c(0, 300, 981), gy = c(0, 300, 499))
  expect_equal(guess_plotdim(x), c(1000, 500))

  x <- data.frame(gx = c(0, 981), gy = c(0, 479))
  expect_equal(
    expect_message(guess_plotdim(x), "Guessing"),
    c(1000, 480)
  )

  expect_equal(guess_plotdim(fgeo.data::luquillo_stem6_random), c(320, 500))
})



context("test-nms_pull_matches")

test_that("pulls the expected names", {
  skip_if_not_installed("fgeo.data")
  vft <- fgeo.data::luquillo_stem_random_tiny
  expected <- c("gx", "gy")
  expect_equal(nms_pull_matches(vft, c("x", "PX", "gx", "gy")), expected)
})

test_that("fails with unnamed input", {
  expect_error(nms_pull_matches("unnnamed", "x"), "is not TRUE")
})



context("commas")

test_that("output the expected string", {
  expect_equal(commas(1:3), "1, 2, 3")
  expect_equal(pipes(1:3), "1|2|3")
})
