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

describe("nms_pull_matches", {
  it("pulls the expected names", {
    vft <- luquillo_stem_random_tiny
    expected <-  c("gx", "gy")
    expect_equal(nms_pull_matches(vft, c("x", "PX", "gx", "gy")), expected)
  })

  it("fails with unnamed input", {
    expect_error(nms_pull_matches("unnnamed", "x"), "is not TRUE")
  })
})



context("glue_comma")

test_that("output the expected string", {
  expect_equal(glue_comma(1:3), "1, 2, 3")
  expect_equal(glue_pipe(1:3), "1|2|3")
})
