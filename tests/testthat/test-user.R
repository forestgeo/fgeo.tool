context("top")

df <- data.frame(x = 1:9, y = letters[1:3], stringsAsFactors = FALSE)

test_that("var can be reffered bared, quoted and by position", {
  # `var` can be bare or quoted
  (result <- top(df, "y"))
  expect_equal(top(df, y), result)

  # matching `var` by position starting from the left
  expect_equal(top(df, var = y), top(df, var = 2))
  # matching `var` by position starting from the right
  expect_equal(top(df, var = y), top(df, var = -1))
  expect_equal(
    unique(top(df, y, n = 2)$y),
    c("a", "b")
  )
  # Negative values select from the tail
  expect_equal(
    unique(top(df, y, n = -2)$y),
    c("b", "c")
  )
})



context("rm_dead_twice")

dfm <- tibble::tribble(
  ~PlotCensusNumber, ~Tag,  ~Status,
                  1,    1,   "alive",
                  1,    1,    "dead",
                  1,    2,    "dead",
                  1,    2,    "dead",

                  2,    1,   "alive",
                  2,    1,   "alive",
                  2,    2,   "alive",
                  2,    2,    "dead",

                  3,    1,   "alive",
                  3,    1,   "alive",
                  3,    2,    "dead",
                  3,    2,    "dead"
)

test_that("returns equal to a known object", {
  expect_known_output(rm_dead_twice(dfm), "ref_rm_dead_twice.csv")
})

test_that("adding a third census removes a first census", {
  out <- rm_dead_twice(dfm)
  expect_false(any(grepl(1, out$PlotCensusNumber)))
})



context("add_status_tree")

test_that("the tree status is dead only if one stem is dead", {
  one_dead <- tibble::tibble(
    tag = c(
      1, 1,
      2, 2,
      3, 3
    ),
    status = c(
      "alive", "dead",
      "dead", "dead",
      "broken below", "missing"
    ),
    plotcensusnumber = 1,
    plotid = 1
  )
  expected <- c(rep("alive", 2), rep("dead", 2), rep("alive", 2))
  out <- add_status_tree(one_dead)
  expect_equal(out$status_tree, expected)
  expect_is(out, "data.frame")
})

.df <- tibble::tribble(
  ~PlotCensusNumber, ~Tag,  ~Status,
  1,    1,   "alive",
  1,    1,    "dead",
  1,    2,    "dead",
  1,    2,    "dead",

  2,    1,   "alive",
  2,    1,   "alive",
  2,    2,   "alive",
  2,    2,    "dead"
)

test_that("works even if data already contains the variable `status_tree`", {
  expect_silent(add_status_tree(add_status_tree(.df)))
})

test_that("outputs the correct variable status_tree", {
  exp <- c("alive", "alive", "dead", "dead", "alive", "alive", "alive", "alive")
  expect_identical(add_status_tree(.df)$status_tree, exp)
})
