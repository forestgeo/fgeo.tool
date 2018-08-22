context("multiple_var")

describe("multiple_var", {
  it("detects multiple censusid within each group", {
    multiple_censusid_by_group <- multiple_var_by_group("censusid")
    
    census <- tibble(CensusID = c(1, 2), treeID = c(1, 2))
    expect_true(multiple_censusid_by_group(census))
    expect_false(multiple_censusid_by_group(group_by(census, CensusID)))
  })
  
})
