context("add_subquad")

vft <- tibble::tribble(
   ~QX,  ~QY,
  17.9,    0,
   4.1,   15,
   6.1, 17.3,
   3.8,  5.9,
   4.5, 12.4,
   4.9,  9.3,
   9.8,  3.2,
  18.6,  1.1,
  17.3,  4.1,
   1.5, 16.3
)

test_that("outputs a dataframe with new expected variable", {
  with_sq <- vft %>% add_subquad(x_q = 20, x_sq = 5)
  expect_named(with_sq, c(names(vft), "subquadrat"))
  expect_is(with_sq, "data.frame")
})

test_that("throws error with wrong inputs to add_subquad", {

  # check that works
  expect_silent(add_subquad(vft, 20, 20, 5, 5))
  expect_silent(add_subquad(vft, 40, 50, 5, 5))

  # Fails
  expect_error(add_subquad(1, 20, 20, 5, 5))
  expect_error(add_subquad(vft, "a", 20, 5, 5))
  expect_error(add_subquad(vft, 20, c(20, 20), 5, 5))
  expect_error(add_subquad(vft, 20, 20, "a", 5))
  expect_error(add_subquad(vft, 20, 20, 5, c(5, 5)))
  expect_error(add_subquad(vft, -1, 20, 5, 5))
  expect_error(add_subquad(vft, 20, Inf, 5, 5))
  expect_error(add_subquad(vft, 20, 20, 5, 5, start_with0 = "wrong input"))
})
