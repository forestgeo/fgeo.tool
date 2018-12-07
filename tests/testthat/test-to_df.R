context("to_df")

describe("to_df.krig_lst", {
  skip_if_not_installed("fgeo.krig")
  
  vars <- c("c", "p")
  out_lst <- fgeo.krig::krig(fgeo.krig::soil_fake, vars, quiet = TRUE)
  out_df <- to_df(out_lst)
  
  it("passes silently with data of correct class", {
    expect_silent(head(out_df))
  })

  it("fails with unknown class", {
    expect_error(to_df(character(1)), "Can't deal with data of class")
    expect_error(to_df(data.frame(1)), "Can't deal with data of class")
    expect_error(to_df(list(1)), "Can't deal with data of class")
  
    expect_error(to_df(out_lst, name = 1))
    expect_error(to_df(out_lst, item = 1))
    expect_error(to_df(out_lst, item = "bad_item"))
    expect_error(to_df(out_lst, item = c("df", "df.poly")))
  })

  it("outputs an object of the expected class", {
    # no-longer class krig_lst
    expect_false(any("krig_lst" %in% class(to_df(out_lst))))
    expect_is(to_df(out_lst), "data.frame")
    expect_is(to_df(out_lst), "tbl")
  })
  
})



context("to_df.tt_lst")

describe("to_df.tt_lst", {
  skip_if_not_installed("fgeo.habitat")
  
  cns <- fgeo.habitat::luquillo_top3_sp
  spp <- unique(cns$sp)[1]
  hab_luq <- fgeo.x::habitat
  tt_lst <- fgeo.habitat::tt_test(cns, spp, hab_luq)

  it("outputs the expected dataframe", {
    expect_equal(class(tt_lst), c("tt_lst", "list"))
  
    out <- expect_silent(to_df(tt_lst))
    expect_is(out, c("tt_df"))
    vars <- c("habitat", "sp", "distribution", "stem_count")
    expect_true(all(vars %in% names(out)))
  })
})



context("to_df.demography_impl")

census1 <- fgeo.x::tree5
census2 <- fgeo.x::tree6

test_that("with split2 errs with informative message", {
  expect_warning(
    out <- recruitment_impl(
      census1, census2, split1 = census1$sp, 
      split2 = census1$quadrat
    ), "split2.*deprecated"
  )
  expect_error(to_df(out), "split2.*deprecated")
})

test_that("With no split, or `split1`, outputs consistent dataframe", {
  nms <- c("N2", "R", "rate", "lower", "upper", "time", "date1", "date2")
  
  .x <- recruitment_impl(census1, census2)
  expect_error(to_df(unclass(.x)), "Can't deal with data")
  
  .x <- recruitment_impl(census1, census2)
  expect_is(to_df(.x), "data.frame")
  expect_named(to_df(.x), nms)
  
  by <- census1$sp
  .x <- recruitment_impl(census1, census2, split1 = by)
  expect_is(to_df(.x), "data.frame")
  expect_named(to_df(.x), c("groups", nms))
  # Same
  by <- interaction(census1$sp)
  .x <- recruitment_impl(census1, census2, split1 = by)
  expect_is(to_df(.x), "data.frame")
  expect_named(to_df(.x), c("groups", nms))
  
  by <- interaction(census1$sp, census1$quadrat)
  .x <- recruitment_impl(census1, census2, split1 = by)
  expect_is(to_df(.x), "data.frame")
  expect_named(to_df(.x), c("groups", nms))
})

