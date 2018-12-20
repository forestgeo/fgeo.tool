#' Read ForestGEO censuses.
#' 
#' This function reads any number of ForestGEO censuses stored in .rdata files
#' and stores them in a nested dataframe of class 'censuses_df', which has a
#' convenient print method and enables using specific methods for working with
#' ForestGEO data.
#' 
#' @seealso [as_censuses()].
#'
#' @inheritParams rdata_df
#' 
#' @family functions to import ForestGEO data
#'
#' @return An object of class 'censuses_df'.
#' @export
#'
#' @examples
#' censuses <- read_censuses_df(tool_example("rdata"))
#' class(censuses)
#' censuses$data
#' 
#' pick(censuses, dbh > 50)$data
read_censuses_df <- function(path_dir, .match = NULL, .id = "censuses") {
  dfm <- rdata_df(path_dir = path_dir, .match = .match, .id = .id)
  as_censuses(tidyr::nest(dfm, -.id))
}

