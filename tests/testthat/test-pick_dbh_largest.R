context("pick_dbh_largest")

library(dplyr)

cns <- tibble::tribble(
  ~dbh,   ~sp, ~treeID, ~stemID,
    10, "sp1",     "1",   "1.1",
   100, "sp1",     "1",   "1.2",
    22, "sp2",     "2",   "2.1",
    99, "sp2",     "2",   "2.2",
    NA, "sp2",     "2",   "2.3"
)

describe("pick_dbh_largest()", {
  it("outputs the same groups as input", {
    # Ungrouped
    collapsed <- pick_dbh_largest(cns)
    expect_equal(group_vars(collapsed), group_vars(cns))
    
    # Grouped
    bysp <- group_by(cns, sp)
    collapsed <- pick_dbh_largest(bysp)
    expect_equal(group_vars(collapsed), group_vars(bysp))
  })
  
  it("automatically groups by CensusID", {
    cns$CensusID <- c(1, 2, 1, 2, 2)
    .cns <- arrange(cns, CensusID, treeID, stemID, dbh)
    out <- pick_dbh_largest(.cns)
    out <- arrange(out, CensusID, treeID, stemID, dbh)
    # Output all available censuses
    expect_equal(out$CensusID, as.double(c(1, 1, 2, 2)))
    # Output only one tree per census
    expect_equal(out$treeID, as.character(c(1, 2, 1, 2)))
    # Ouput largest stem per tree per census
    expect_equal(out$stemID, as.character(c(1.1, 2.1, 1.2, 2.2)))
    
    # It picks an oviously larger stem added to census 2
    cns$CensusID <- c(1, 2, 1, 2, 2)
    .cns2 <- bind_rows(
      .cns, 
      list(dbh = 200, sp = "sp2", treeID = "2", stemID = "2.4", CensusID = 2)
    )
    out2 <- pick_dbh_largest(.cns2)
    picked <- filter(out2, CensusID == 2, treeID == 2)
    expect_equal(picked$stemID, "2.4")
    expect_equal(picked$dbh, 200)
  })
  
  it("drops missing values of censusid if there are multiple unique censusid", {
    # Doesn't drop missing censusid if they are unambiguous (only one censusid)
    cns$CensusID <- c(1, 1, 1, 1, NA)
    expect_silent(out <- pick_dbh_largest(cns))
    
    # Drops missing censusid if they are unambiguous (multiple censusid)
    cns$CensusID <- c(1, 1, 2, 2, NA)
    expect_warning(
      out <- pick_dbh_largest(cns),
      "Dropping.*rows with missing.*values"
    )
    
    expect_false(any(is.na(out$CensusID)))
  })
  
  it("rejects data with multiple values of `plotname`", {
    cns$PlotName <- c(1, 1, 2, 2, NA)
    expect_error(
      out <- pick_dbh_largest(cns),
      "must have a single plotname"
    )
  })
  
  it("picks only one row per treeid per census despite ties", {
    cns <- tibble::tribble(
      ~dbh,   ~sp, ~treeID, ~stemID,
        10, "sp1",     "1",   "1.1",
       100, "sp1",     "1",   "1.2",
        22, "sp2",     "2",   "2.1",
        99, "sp2",     "2",   "2.2",
        99, "sp2",     "2",   "2.3",
        NA, "sp2",     "2",   "2.4"
    )
    
    out <- pick_dbh_largest(cns)
    expect_equal(filter(out, treeID == 1)$dbh, 100)
    expect_equal(filter(out, treeID == 2)$dbh, 99)
    expect_named(out, c("dbh", "sp", "treeID", "stemID"))
  })
})
