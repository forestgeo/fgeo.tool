context("pick_woods")

library(dplyr)

describe("pick_woods with real-ish ViewFullTables and census tables", {
  it("outputs a tibble", {
    stem_one_census <- fgeo.data::luquillo_stem6_1ha
    expect_is(pick_woods(stem_one_census, dbh >= 10), "tbl_df")
    
    tree_one_census <- fgeo.data::luquillo_tree6_random
    expect_is(pick_woods(tree_one_census, dbh >= 10), "tbl_df")

    vft <- fgeo.data::luquillo_vft_4quad
    expect_is(pick_woods(vft, dbh >= 10), "tbl_df")
  })
})

describe("pick_woods with buttressess and smaller diameter higher up the tree", {
  it("picks main stem of smaller diameter but higher up the tree", {
    census <- tibble::tribble(
        ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~CensusID,
      "sp1",     "1",   "1.1",   140,   40,         2,  # main stem
      "sp1",     "1",   "1.1",   130,   60,         2,  # buttresses
    )
    expect_equal(pick_woods(census, dbh >= 10)$dbh, 40)

    census <- tibble::tribble(
        ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~CensusID,
      "sp1",     "1",   "1.1",   140,   40,         1,  # main stem
      "sp1",     "1",   "1.1",   130,   60,         1,  # buttresses
      
      "sp1",     "1",   "1.1",   140,   60,         2,  # main stem
      "sp1",     "1",   "1.1",   130,   40,         2,  # buttresses
    )
    expect_equal(pick_woods(census, dbh >= 10)$dbh, c(40, 60))
  })
  
  it("outputs the same for a census in a single or multi-census dataset", {
    cns_n <- 6
    multi <- fgeo.data::luquillo_vft_4quad
    single <- filter(multi, CensusID == cns_n)
    
    out_m <- filter(pick_woods(multi, dbh > 0), CensusID == cns_n)
    out_s <- pick_woods(single, dbh > 0)
    expect_equal(out_m, out_s)
  })
})




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

describe("pick_woods with single census", {
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
