context("by_group")

library(dplyr)

describe("by_group()", {
  dfm <- tibble::tribble(
    ~x, ~y,  ~z,
    11, 21,  31,
    12, 22,  32,
    13, 23,  33
  )
  
  # Example
  first_row <- function(.x, to_chr = FALSE) {
    first <- .x[1, ]
    if (to_chr) {
      first[] <- lapply(first, as.character)
    }
    
    tibble::as.tibble(first)
  }
  
  it("works with ungrouped data", {
    out <- dfm %>% by_group(first_row)
    expect_equal(out, tibble::tibble(x = 11, y = 21, z = 31)) 
    expect_is(out, "tbl")
    expect_false(dplyr::is_grouped_df(out))
  })
  
  it("works with grouped data", {
    out <- dfm %>% group_by(x) %>% by_group(first_row)
    expect_equal(out, dfm)
    expect_is(out, "tbl")
    expect_true(dplyr::is_grouped_df(out))
    expect_named(out, c("x", "y", "z"))
  })
  
  it("is sensitive to arguments passed via ...", {
    grouped <-  dfm %>% group_by(x)
    out <- grouped %>% by_group(first_row, to_chr = TRUE)
    expect_equal(out, purrr::map_df(grouped, as.character))
  })
  
  it("fails with informative message", {
    expect_error(by_group("not a data.frame"), "is not TRUE")
    expect_error(by_group(dfm, "not a function"), "is not TRUE")
  })
})

