library(tibble)
library(rlang)

context("detect_duplicated_var")

describe("*_duplicated_var()", {
  tree <- tibble(treeID = c(1, 1))
  it("creates functionsl closures", {
    expect_message(flag_duplicated_var(inform, treeid)(tree), "Detected")
    expect_warning(flag_duplicated_var(warn, treeid)(tree), "Detected")
    expect_error(flag_duplicated_var(abort, treeid)(tree), "Detected")
  })
})

describe("detect_duplicated_treeid", {
  it("is silent with a tree table", {
    tree <- tibble(treeID = c(1, 2))
    expect_silent(warn_duplicated_treeid(tree))
  })
  
  it("works with a vft", {
    vft <- tibble(TreeID = c(1, 2))
    expect_false(detect_duplicated_treeid(vft))
    expect_silent(warn_duplicated_treeid(vft))
  })
  
  it("doesn't group by censusid", {
    # Not duplicated by census but is duplicated across the entire dataset
    # This is an issue but it's not the job of this function to deal with this
    tree <- tibble(CensusID = c(1, 2), treeID = c(1, 1))
    expect_true(detect_duplicated_treeid(tree))
  })
  
  it("handles grouped data", {
    tree <- tibble(CensusID = c(1, 2), treeID = c(1, 1))
    by_censusid <- group_by(tree, CensusID)
    expect_false(detect_duplicated_treeid(by_censusid))
  })
})

describe("warn_duplicated_treeid", {
  it("warns with stem table", {
    stem <- tibble(treeID = c(1, 1), stemID = c(1.1, 1.2))
    expect_warning(
      warn_duplicated_treeid(stem), 
      "Detected duplicated values of treeid"
    )
  })
})

