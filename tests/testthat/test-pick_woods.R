context("pick_woods")

library(dplyr)

cns <- tibble::tribble(
  ~dbh,   ~sp, ~treeID, ~stemID, ~hom,
    10, "sp1",     "1",   "1.1",   10,
   888, "sp1",     "1",   "1.2",   10,
   100, "sp1",     "1",   "1.2",   88,
    22, "sp2",     "2",   "2.1",   10,
    99, "sp2",     "2",   "2.2",   10,
    88, "sp2",     "2",   "2.2",   22,
    NA, "sp2",     "2",   "2.3",   10
)

describe("pick_woods", {
  it("with no filtering expressions it outputs equal to pick_largest_hom_dbh()", {
    expect_equal(pick_woods(cns), pick_largest_hom_dbh(cns))
  })
  
  it("returns the expected data structure", {
    expect_is(pick_woods(cns), "data.frame")
    expect_named(pick_woods(cns), c("dbh", "sp", "treeID", "stemID", "hom"))
  })
  
  it(glue::glue("picks only stems between dbh of 10 mm inclusive and 100 dbh\\
    exclusive and it is picked first by largest hom, then my largest dbh"), {
    expect_equal(pick_woods(cns, dbh >= 100)$hom, 88)
    expect_equal(pick_woods(cns, dbh >= 100)$dbh, 100)
  })
})

describe("pick_trees", {
  it("picks only stems of dbh 100 mm and over", {
    expect_equal(pick_trees(cns)$dbh, 100)
  })
})

describe("pick_saplings", {
  it(glue::glue("
      picks only stems between dbh of 10 mm inclusive and 100 dbh exclusive\\
      and it is picked first by largest hom, then my largest dbh
    "), {
    expect_equal(pick_saplings(cns)$hom, 22)
    expect_equal(pick_saplings(cns)$dbh, 88)
  })
})
