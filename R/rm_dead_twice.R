#' Remove trees found dead in both the last and previous last censuses.
#'
#' Removes trees that were found dead both in the last and previous last
#' censuses. Thus the resulting data set contains trees that in the last census
#' were found either alive or dead for the first time.
#'
#' @param vft A dataframe -- specifically, a ForestGEO ViewFullTable.
#'
#' @return A modified version of the input data set:
#'     * With an additional variable indicating the status of each tree.
#'     * With the rows removed of all censuses except the last two.
#'     * With the rows removed of trees found dead on both the last and previous
#'       last censuses.
#' @export
#' @family functions to filter dataframes.
#'
#' @examples
#' vft <- tibble::tribble(
#'   ~CensusID, ~Tag,  ~Status,
#'           1,    1,   "alive",
#'           1,    1,    "dead",
#'           1,    2,   "alive",
#'           1,    2,   "alive",
#'           2,    1,   "alive",
#'           2,    1,    "dead",
#'           2,    2,    "dead",
#'           2,    2,    "dead",
#'           3,    1,   "alive",
#'           3,    1,    "dead",
#'           3,    2,    "dead",
#'           3,    2,    "dead"
#' )
#'
#' # Notice the rows where `status_tree` in census 3 and 2 is "dead"
#' # (`Status` refers to stems while `status_tree` refers to trees.)
#' add_status_tree(vft)
#'
#' # * Remove all censuses except the last two.
#' # * Remove trees found dead on both the last and previous last censuses.
#' rm_dead_twice(vft)
rm_dead_twice <- function(vft) {
  stopifnot(is.data.frame(vft))
  check_crucial_names(vft, c("CensusID", "Tag", "Status"))

  if (!length(unique(vft$CensusID)) >= 2) {
    warning("`The data set has less than two censuses; Keeping all trees")
    return(vft)
  }

  last <- max(vft$CensusID, na.rm = TRUE)
  last2 <- vft[vft$CensusID %in% c(last, last - 1), ]
  last2 <-  add_status_tree(last2)
  grouped <- group_by(last2, .data$CensusID, .data$Tag)
  to_filter <- ungroup(
    mutate(
      grouped, is_to_keep = !identical(.data$status_tree, c("dead", "dead"))
    )
  )
  to_filter[to_filter$is_to_keep, setdiff(names(to_filter), "is_to_keep")]
}

