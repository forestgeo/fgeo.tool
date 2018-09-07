context("pick_main_stem")

library(dplyr)
library(tibble)

describe("pick_main_stem with multiple stems including buttress", {
  it("doesn't mess the original order of the data", {
    census <- tribble(
        ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~rowindex,
      "sp1",     "1",   "1.1",     2,    1,         1,
      "sp1",     "1",   "1.1",     1,    1,         2,
      "sp1",     "2",   "2.1",     1,    1,         3,
      "sp1",     "3",   "3.1",     1,    1,         4,
    )
    out <- pick_main_stem(census)
    expect_identical(out$rowindex, sort(out$rowindex))
    
    census <- tribble(
        ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~rowindex,
      "sp1",     "3",   "3.1",     1,    1,         1,
      "sp1",     "2",   "2.1",     1,    1,         2,
      "sp1",     "1",   "1.1",     1,    1,         3,
      "sp1",     "1",   "1.1",     2,    1,         4,
    )
    out <- pick_main_stem(census)
    expect_identical(out$rowindex, sort(out$rowindex))

    census <- tribble(
        ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~rowindex,
      "sp1",     "3",   "3.1",     1,    1,         1,
      "sp1",     "1",   "1.1",     2,    1,         2,
      "sp1",     "2",   "2.1",     1,    1,         3,
      "sp1",     "1",   "1.1",     1,    1,         4,
    )
    out <- pick_main_stem(census)
    expect_identical(out$rowindex, sort(out$rowindex))
  })
  
  it("chooses the stem of largest hom regardless of dbh", {
    census <- tribble(
        ~sp, ~treeID, ~stemID,  ~hom, ~dbh,
      "sp1",     "1",   "1.1",     2,    1,
      "sp1",     "1",   "1.1",     1,    1,
    )
    expect_equal(pick_main_stem(census)$hom, 2)
    
    census <- tribble(
        ~sp, ~treeID, ~stemID,  ~hom, ~dbh,
      "sp1",     "1",   "1.1",     2,    2,
      "sp1",     "1",   "1.1",     1,    1,
    )
    expect_equal(pick_main_stem(census)$hom, 2)
    
    census <- tribble(
        ~sp, ~treeID, ~stemID,  ~hom, ~dbh,
      "sp1",     "1",   "1.1",     2,    1,
      "sp1",     "1",   "1.1",     1,    2,
    )
    expect_equal(pick_main_stem(census)$hom, 2)
    
    census <- tibble::tribble(
        ~sp, ~treeID, ~stemID,  ~hom, ~dbh,
      "sp1",     "1",   "1.1",     2,    1,
      "sp1",     "1",   "1.2",     1,    2,
    )
    expect_equal(pick_main_stem(census)$hom, 2)
    
    census <- tibble::tribble(
        ~sp, ~treeID, ~stemID,  ~hom, ~dbh,
      "sp1",     "1",   "1.1",     2,    2,
      "sp1",     "1",   "1.2",     1,    2,
    )
    expect_equal(pick_main_stem(census)$hom, 2)
    
    census <- tibble::tribble(
        ~sp, ~treeID, ~stemID,  ~hom, ~dbh,
      "sp1",     "1",   "1.1",     2,    2,
      "sp1",     "1",   "1.2",     1,    1,
    )
    expect_equal(pick_main_stem(census)$hom, 2)
  })
})

cns <- tibble::tribble(
  ~hom, ~dbh,   ~sp, ~treeID, ~stemID,
    10,   10, "sp1",     "1",   "1.1",
    20,  100, "sp1",     "1",   "1.2",  # main stem
    10,  111, "sp1",     "1",   "1.2",
  
    10,   22, "sp2",     "2",   "2.1",
    22,   88, "sp2",     "2",   "2.2",
    22,   99, "sp2",     "2",   "2.2",  # main stem
    10,   NA, "sp2",     "2",   "2.3"
)

describe("pick_main_stem()", {
  it("outputs the expected data structure", {
    out <- pick_main_stem(cns)
    expect_named(out, c("hom", "dbh", "sp", "treeID", "stemID"))
  })
  
  it("picks first by hom then by dbh", {
    collapsed <- pick_main_stem(cns)
    expect_equal(collapsed$hom, c(20, 22))
    expect_equal(collapsed$dbh, c(100, 99))
  })
  
  it("outputs the same groups as input", {
    # Ungrouped
    collapsed <- pick_main_stem(cns)
    expect_equal(group_vars(collapsed), group_vars(cns))
    
    # Grouped
    bysp <- group_by(cns, sp)
    collapsed <- pick_main_stem(bysp)
    expect_equal(group_vars(collapsed), group_vars(bysp))
  })
  
  it("automatically groups by CensusID", {
    cns <- tibble::tribble(
      ~hom, ~dbh,   ~sp, ~treeID, ~stemID,
      10,   10, "sp1",     "1",   "1.1",
      10,  111, "sp1",     "1",   "1.2",
      10,   22, "sp2",     "2",   "2.1",
      22,   88, "sp2",     "2",   "2.2",
      10,   NA, "sp2",     "2",   "2.3"
    )
    
    cns$CensusID <- c(1, 2, 1, 2, 2)
    .cns <- arrange(cns, CensusID, treeID, stemID, dbh)
    out <- pick_main_stem(.cns)
    out <- arrange(out, CensusID, treeID, stemID, dbh)
    # Output all available censuses
    expect_equal(out$CensusID, as.double(c(1, 1, 2, 2)))
    # Output only one tree per census
    expect_equal(out$treeID, as.character(c(1, 2, 1, 2)))
    # Ouput largest stem per tree per census
    expect_equal(out$stemID, as.character(c(1.1, 2.1, 1.2, 2.2)))
  })
  
  it("drops missing values of censusid if there are multiple unique censusid", {
    cns <- tibble::tribble(
      ~hom, ~dbh,   ~sp, ~treeID, ~stemID,
      10,   10, "sp1",     "1",   "1.1",
      10,  111, "sp1",     "1",   "1.2",
      10,   22, "sp2",     "2",   "2.1",
      22,   88, "sp2",     "2",   "2.2",
      10,   NA, "sp2",     "2",   "2.3"
    )
    
    # Doesn't drop missing censusid if they are unambiguous (only one censusid)
    cns$CensusID <- c(1, 1, 1, 1, NA)
    expect_silent(out <- pick_main_stem(cns))
    
    # Drops missing censusid if they are unambiguous (multiple censusid)
    cns$CensusID <- c(1, 1, 2, 2, NA)
    expect_warning(
      out <- pick_main_stem(cns),
      "Dropping.*rows with missing.*values"
    )
    
    expect_false(any(is.na(out$CensusID)))
  })
  
  it("rejects data with multiple values of `plotname`", {
    cns <- tibble::tribble(
      ~hom, ~dbh,   ~sp, ~treeID, ~stemID,
      10,   10, "sp1",     "1",   "1.1",
      10,  111, "sp1",     "1",   "1.2",
      10,   22, "sp2",     "2",   "2.1",
      22,   88, "sp2",     "2",   "2.2",
      10,   NA, "sp2",     "2",   "2.3"
    )
    
    cns$PlotName <- c(1, 1, 2, 2, NA)
    expect_error(
      out <- pick_main_stem(cns),
      "must have a single plotname"
    )
  })
})

test_that("pick_main_stemid() leaves no duplicated stemid", {
  cns <- tibble::tribble(
    ~hom, ~dbh,   ~sp, ~treeID, ~stemID,
       1,    1, "sp1",     "1",   "1.1",
       2,    1, "sp1",     "1",   "1.1",  # Main stemid because hom is highest
       1,    1, "sp1",     "1",   "1.2"
  )
  out <- pick_main_stemid(cns)
  expect_false(is_duplicated(out$stemID))
  expect_length(out$stemID, 2)
  expect_equal(out$stemID, c("1.1", "1.2"))
  expect_equal(out$hom, c(2, 1))
})
