context("flag_if_group")

library(dplyr)

describe("detect_if_group() and flag_if_group()", {
  msg <- "Flagged values were detected"

  it("handles grouped data", {
    tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 2))
    by_censusid <- group_by(tree, CensusID)
    expect_false(detect_if_group(by_censusid, "treeID", is_multiple))
    expect_silent(flag_if_group(by_censusid, "treeID", is_multiple))

    expect_true(detect_if_group(tree, "treeID", is_multiple))
    expect_warning(flag_if_group(tree, "treeID", is_multiple), msg)

    tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 1))
    by_censusid <- group_by(tree, CensusID)
    expect_false(
      detect_if_group(by_censusid, "treeID", is_duplicated)
    )
    expect_silent(flag_if_group(by_censusid, "treeID", is_duplicated))
    expect_true(detect_if_group(tree, "treeID", is_duplicated))
    expect_warning(flag_if_group(tree, "treeID", is_duplicated), msg)
  })

  it("handles variable that is also a grouping variable", {
    tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 1))
    by_censusid <- group_by(tree, CensusID)
    expect_silent(flag_if_group(by_censusid, "CensusID", is_duplicated))
  })

  it("handles multiple conditions and a custom message", {
    tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 2))
    expect_warning(flag_if_group(tree, "treeID", is_multiple, warn), msg)
    expect_message(flag_if_group(tree, "treeID", is_multiple, inform), msg)
    expect_error(flag_if_group(tree, "treeID", is_multiple, abort), msg)

    mymsg <- "My message"
    expect_warning(flag_if_group(tree, "treeid", is_multiple, , mymsg), mymsg)
  })

  it("includes in the message the name of the variable being tested", {
    tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 2))
    expect_warning(flag_if_group(tree, "treeID", is_multiple), "treeID")
  })

  it("is insensitive to case of variable names", {
    vft <- tibble(TreeID = c(1, 1))
    expect_silent(flag_if_group(vft, "treeid", is_multiple))
    vft <- tibble(TreeID = c(1, 2))
    expect_warning(flag_if_group(vft, "treeid", is_multiple))
  })
})
