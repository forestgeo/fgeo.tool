context("flag_if_group")

library(dplyr)
library(rlang)
library(fgeo.base)

describe("flag_if_group", {
  tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 2))
  msg <- "Flagged values were detected"
  
  it("handles grouped data", {
    by_censusid <- group_by(tree, CensusID)
    expect_silent(flag_if_group(by_censusid, "treeID", is_multiple))
    expect_warning(flag_if_group(tree, "treeID", is_multiple), msg)
  })

  it("handles variable that is also a grouping variable", {
    by_censusid <- group_by(tree, CensusID)
    expect_silent(flag_if_group(by_censusid, "CensusID", is_multiple))
  })

  it("handles multiple conditions and a custom message", {
    expect_warning(flag_if_group(tree, "treeID", is_multiple, warn), msg)
    expect_message(flag_if_group(tree, "treeID", is_multiple, inform), msg)
    expect_error(flag_if_group(tree, "treeID", is_multiple, abort), msg)

    mymsg <- "My message"
    expect_warning(flag_if_group(tree, "treeid", is_multiple, , mymsg), mymsg)
  })

  it("includes in the message the name of the variable being tested", {
    expect_warning(flag_if_group(tree, "treeID", is_multiple), "treeID")
  })

  it("is insensitive to case of variable names", {
    vft <- tibble(TreeID = c(1, 1))
    expect_silent(flag_if_group(vft, "treeid", is_multiple))
    vft <- tibble(TreeID = c(1, 2))
    expect_warning(flag_if_group(vft, "treeid", is_multiple))
  })
})
