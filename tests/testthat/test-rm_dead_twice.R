context("rm_dead_twice")

# rm_dead_twice() makes no sense for census tables because each has only 1 cns.
cns <- tribble(
  ~CensusID, ~Tag,  ~Status,
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
  expect_known_output(rm_dead_twice(cns), "ref_rm_dead_twice.csv")
})

test_that("adding a third census removes a first census", {
  out <- rm_dead_twice(cns)
  expect_false(any(grepl(1, out$CensusID)))
})
