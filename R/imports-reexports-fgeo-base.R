#' @importFrom fgeo.base check_crucial_names rename_matches
#' @importFrom fgeo.base is_multiple is_duplicated detect_if flag_if
NULL

#' @family general functions to edit data in place
#' @inherit fgeo.base::fill_na
#' @inherit fgeo.base::fill_na seealso
#' @export
fill_na <- fgeo.base::fill_na

#' @family general functions to edit data in place
#' @inherit fgeo.base::to_tidy_names
#' @export
to_tidy_names <- fgeo.base::to_tidy_names

#' @family functions to pick or drop rows of a ForestGEO dataframe
#' @inherit fgeo.base::pick_drop
#' @name pick_drop
NULL
#' @export
#' @rdname pick_drop
pick_dbh_min <- fgeo.base::pick_dbh_min
#' @export
#' @rdname pick_drop
pick_dbh_max <- fgeo.base::pick_dbh_max
#' @export
#' @rdname pick_drop
pick_dbh_under <- fgeo.base::pick_dbh_under
#' @export
#' @rdname pick_drop
pick_dbh_over <- fgeo.base::pick_dbh_over
#' @export
#' @rdname pick_drop
pick_status <- fgeo.base::pick_status
#' @export
#' @rdname pick_drop
drop_status <- fgeo.base::drop_status
