#' Count duplicated observations of a variable.
#' 
#' This is a shortcut for `filter(count(x, ...) > 1)` (from the __dplyr__
#' package) to count the number of observations of --specifically -- duplicated
#' observations of a variable. While this shortcut may be handy, it's best to
#' use __dplyr__'s longer form because -- as __dplyr__ well known -- your code
#' will be more readable. So this function is mostly to help you discover the
#' more general way of solving this problem with __dplyr__.
#' 
#' This function preserves __dplyr__'s style and thus non-standard evaluation.
#' If #' you want to use it inside your own functions you should learn about
#' tidy eval (implemented via the __rlang__ package). A good place to start is
#' at __dplyr__'s website.
#'
#' @param x A dataframe.
#' @inheritParams dplyr::count
#' 
#' @seealso [dplyr::count()], [dplyr::filter()]. 
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(a = c(1, 2, 2, 3, 3, 3))
#' count_duplicated(df, a)
#' count_duplicated(df, a, sort = TRUE)
#' 
#' # Count duplicates is a short-hand for dplyr::count() + dplyr::filter()
#' count(df, a) %>% filter(n > 1)
count_duplicated <- function(x, ..., wt = NULL, sort = FALSE) {
  if (length(dplyr::group_vars(x)) != 0) {
    warn("Can't handle grouped `x`. Ungrouping to continue.")
    x <- dplyr::ungroup(x)
  }
  
  group_vars <- enquos(...)
  count_call <- quo(dplyr::count(x, !!!group_vars, wt = !!wt, sort = !!sort))
  cnt <- eval_tidy(count_call, x)
  dplyr::filter(cnt, dplyr::last(cnt) > 1)
}

