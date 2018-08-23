library(tibble)
library(rlang)

# FIXME: Rename file

context("flag_duplicated_by_group")

describe("flag_duplicated_treeid_by_group", {
  tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 1))
  msg <- "Duplicated values were detected"
  
  it("handles grouped data", {
    by_censusid <- group_by(tree, CensusID)
    expect_silent(flag_duplicated_by_group_f("treeID")(by_censusid))
    expect_warning(flag_duplicated_by_group_f("treeID")(tree), msg)
  })

  it("handles multiple conditions and a custom message", {
    expect_message(flag_duplicated_by_group_f("treeID", message)(tree), msg)
    expect_error(flag_duplicated_by_group_f("treeID", rlang::abort)(tree), msg)
    
    custom_msg <- "Custom message"
    flag_dup_treeid_by_grp <- flag_duplicated_by_group_f("treeID", rlang::warn)
    expect_warning(flag_dup_treeid_by_grp(tree, custom_msg), custom_msg)
  })
  
  it("handles grouped data", {
    by_censusid <- group_by(tree, CensusID)
    expect_warning(flag_duplicated_by_group_f("treeid")(tree), msg)
  })
})



context("detect_duplicated_by_group")

describe("detect_duplicated_treeid_by_group", {
  it("handles grouped data", {
    tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 1))
    by_censusid <- group_by(tree, CensusID)
    expect_false(detect_duplicated_by_group_f("treeID")(by_censusid))
    expect_false(detect_duplicated_by_group_f("treeid")(by_censusid))
    expect_true(detect_duplicated_by_group_f("treeid")(tree))
    expect_true(detect_duplicated_by_group_f("treeID")(tree))
  })
})






context("flag_duplicated_var")

describe("*_duplicated_var()", {
  tree <- tibble(treeID = c(1, 1))
  it("creates functionsl closures", {
    expect_message(flag_duplicated_var(inform, treeid)(tree), "Detected")
    expect_warning(flag_duplicated_var(warn, treeid)(tree), "Detected")
    expect_error(flag_duplicated_var(abort, treeid)(tree), "Detected")
  })
})




describe("detect_duplicated_treeid_by_group", {
  it("is silent with a tree table", {
    tree <- tibble(treeID = c(1, 2))
    expect_silent(flag_duplicated_by_group_f("treeid")(tree))
  })
  
  it("works with a vft", {
    vft <- tibble(TreeID = c(1, 2))
    expect_false(detect_duplicated_treeid_by_group(vft))
    expect_silent(flag_duplicated_by_group_f("treeid", rlang::warn)(vft))
  })
  
  it("doesn't group by censusid", {
    # Not duplicated by census but is duplicated across the entire dataset
    # This is an issue but it's not the job of this function to deal with this
    tree <- tibble(CensusID = c(1, 2), treeID = c(1, 1))
    expect_true(detect_duplicated_treeid_by_group(tree))
  })
  
  it("handles grouped data", {
    tree <- tibble(CensusID = c(1, 2), treeID = c(1, 1))
    by_censusid <- group_by(tree, CensusID)
    expect_false(detect_duplicated_treeid_by_group(by_censusid))
  })
})
