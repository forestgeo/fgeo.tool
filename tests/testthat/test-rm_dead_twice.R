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
