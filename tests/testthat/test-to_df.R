context("to_df")

describe("to_df.krig_lst", {
  skip_if_not_installed("fgeo.habitat")
  
  vars <- c("c", "p")
  out_lst <- fgeo.habitat::krig(fgeo.habitat::soil_fake, vars, quiet = TRUE)
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

  it("outputs object of no-longer class krig_lst", {
    expect_false(any("krig_lst" %in% class(to_df(out_lst))))
  })
})



describe("to_df.tt_lst", {
  skip_if_not_installed("fgeo.habitat")
  
  cns <- fgeo.habitat::luquillo_top3_sp
  spp <- unique(cns$sp)[1]
  hab_luq <- fgeo.data::luquillo_habitat
  tt_lst <- fgeo.habitat::tt_test(cns, spp, hab_luq)

  it("outputs the expected dataframe", {
    expect_equal(class(tt_lst), c("tt_lst", "list"))
  
    out <- expect_silent(to_df(tt_lst))
    expect_is(out, c("tt_df"))
    vars <- c("habitat", "sp", "distribution", "stem_count")
    expect_true(all(vars %in% names(out)))
  })
})



pick10sp <- function(.data) dplyr::filter(.data, sp %in% unique(.data$sp)[1:10])
tiny1 <- pick10sp(fgeo.data::luquillo_tree5_random)
tiny2 <- pick10sp(fgeo.data::luquillo_tree6_random)
censuses <- list(tiny1 = tiny1, tiny2 = tiny2)

describe("to_df.demography_lst", {
  skip_if_not_installed("fgeo.demography")
  
  it("with `by = NULL` outputs the expected dataframe", {
    as_is <- to_df(fgeo.demography::mortality(censuses))
    
    expect_named(as_is, c("metric", "value"))
    expect_is(as_is, c("tbl"))
  })
})



describe("to_df.demography_lst_by", {
  it("with `by = <not NULL>` outputs the expected dataframe", {
    by_sp <- to_df(fgeo.demography::mortality(censuses, "sp"))
    expect_named(by_sp, c("by", "metric", "value"))
    expect_is(by_sp, c("tbl"))
    expect_false(all(is.na(by_sp$by)))
  })
})
