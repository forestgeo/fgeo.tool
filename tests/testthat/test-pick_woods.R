context("pick_woods")

library(dplyr)

cns <- tibble::tribble(
  ~dbh,   ~sp, ~treeID, ~stemID,
    10, "sp1",     "1",   "1.1",
   100, "sp1",     "1",   "1.2",
    22, "sp2",     "2",   "2.1",
    99, "sp2",     "2",   "2.2",
    NA, "sp2",     "2",   "2.3"
)

describe("pick_woods", {
  it("with no filtering expressions it outputs equal to pick_dbh_largest()", {
    expect_equal(pick_woods(cns), pick_dbh_largest(cns))
  })
})

describe("pick_trees", {
  it("picks only stems of dbh 100 mm and over", {
    expect_equal(pick_trees(cns)$dbh, 100)
  })
})

describe("pick_saplings", {
  it("picks only stems between dbh of 10 mm inclusive and 100 dbh exclusive", {
    expect_equal(pick_saplings(cns)$dbh, 99)
  })
})
