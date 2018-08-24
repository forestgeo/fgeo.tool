library(tibble)
library(rlang)

context("detect_duplicated_by_group_f")

describe("detect_duplicated_by_group_f", {
  it("handles grouped data", {
    tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 1))
    by_censusid <- group_by(tree, CensusID)
    expect_false(detect_duplicated_by_group_f("treeID")(by_censusid))
    expect_false(detect_duplicated_by_group_f("treeid")(by_censusid))
    expect_true(detect_duplicated_by_group_f("treeid")(tree))
    expect_true(detect_duplicated_by_group_f("treeID")(tree))
  })
  
  it("handles grouped data", {
    tree <- tibble(CensusID = c(1, 2), treeID = c(1, 1))
    by_censusid <- group_by(tree, CensusID)
    expect_false(detect_duplicated_by_group_f("treeid")(by_censusid))
    expect_true(detect_duplicated_by_group_f("treeid")(tree))
  })
  
  it("handles variable that is also a grouping variable", {
    census <- tibble(CensusID = c(1, 2), treeID = c(1, 2))
    by_censusid <- group_by(census, CensusID)
    detect_duplicated_by_group_f("CensusID")(by_censusid)
  })
})



context("detect_multiple_by_group_f")

describe("detect_multiple_by_group_f", {
  it("handles grouped data", {
    tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 2))
    by_censusid <- group_by(tree, CensusID)
    expect_false(detect_multiple_by_group_f("treeID")(by_censusid))
    expect_false(detect_multiple_by_group_f("treeid")(by_censusid))
    expect_true(detect_multiple_by_group_f("treeid")(tree))
    expect_true(detect_multiple_by_group_f("treeID")(tree))
  })
  
  it("handles grouped data", {
    tree <- tibble(CensusID = c(1, 2), treeID = c(1, 2))
    by_censusid <- group_by(tree, CensusID)
    expect_false(detect_multiple_by_group_f("treeid")(by_censusid))
    expect_true(detect_multiple_by_group_f("treeid")(tree))
  })
  
  it("handles variable that is also a grouping variable", {
    census <- tibble(CensusID = c(1, 1), treeID = c(1, 2))
    by_censusid <- group_by(census, CensusID)
    detect_multiple_by_group_f("CensusID")(by_censusid)
  })
})



context("flag_duplicated_by_group_f")

describe("flag_duplicated_by_group_f", {
  tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 1))
  msg <- "Duplicated values were detected"
  
  it("handles grouped data", {
    by_censusid <- group_by(tree, CensusID)
    expect_silent(flag_duplicated_by_group_f("treeID")(by_censusid))
    expect_warning(flag_duplicated_by_group_f("treeID")(tree), msg)
  })
  
  it("handles variable that is also a grouping variable", {
    by_censusid <- group_by(tree, CensusID)
    flag_duplicated_by_group_f("CensusID")(by_censusid)
  })
  
  it("handles multiple conditions and a custom message", {
    expect_message(flag_duplicated_by_group_f("treeID", message)(tree), msg)
    expect_error(flag_duplicated_by_group_f("treeID", rlang::abort)(tree), msg)
    
    custom_msg <- "Custom message"
    flag_dup_treeid_by_grp <- flag_duplicated_by_group_f("treeID", rlang::warn)
    expect_warning(flag_dup_treeid_by_grp(tree, custom_msg), custom_msg)
  })
  
  it("includes in the message the name of the variable being tested", {
    expect_warning(flag_duplicated_by_group_f("treeID")(tree), "treeID")
  })
  
  it("is insensitive to case of variable names", {
    vft <- tibble(TreeID = c(1, 2))
    expect_false(detect_duplicated_by_group_f("treeid")(vft))
    expect_silent(flag_duplicated_by_group_f("treeid", rlang::warn)(vft))
  })
})



context("flag_multiple_by_group_f")

describe("flag_multiple_by_group_f", {
  tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 2))
  msg <- "Multiple values were detected"
  
  it("handles grouped data", {
    by_censusid <- group_by(tree, CensusID)
    expect_silent(flag_multiple_by_group_f("treeID")(by_censusid))
    expect_warning(flag_multiple_by_group_f("treeID")(tree), msg)
  })
  
  it("handles variable that is also a grouping variable", {
    by_censusid <- group_by(tree, CensusID)
    flag_multiple_by_group_f("CensusID")(by_censusid)
  })
  
  it("handles multiple conditions and a custom message", {
    expect_message(flag_multiple_by_group_f("treeID", message)(tree), msg)
    expect_error(flag_multiple_by_group_f("treeID", rlang::abort)(tree), msg)
    
    custom_msg <- "Custom message"
    flag_dup_treeid_by_grp <- flag_multiple_by_group_f("treeID", rlang::warn)
    expect_warning(flag_dup_treeid_by_grp(tree, custom_msg), custom_msg)
  })
  
  it("includes in the message the name of the variable being tested", {
    expect_warning(flag_multiple_by_group_f("treeID")(tree), "treeID")
  })
  
  it("is insensitive to case of variable names", {
    vft <- tibble(TreeID = c(1, 1))
    expect_silent(flag_multiple_by_group_f("treeid", rlang::warn)(vft))
    vft <- tibble(TreeID = c(1, 2))
    expect_warning(flag_multiple_by_group_f("treeid", rlang::warn)(vft))
  })
})
