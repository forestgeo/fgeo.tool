#' Add a quadrat variable to a dataframe based based on qx and qy coordinates.
#'
#' @param df A dataframe with quadrat coordinates `QX` and `QY` (e.g. a
#'   ViewFullTable).
#' @param x_q,y_q Size in meters of a quadrat's side. For ForestGEO sites, a
#'   common value is 20.
#' @param x_sq,y_sq Size in meters of a subquadrat's side. For ForestGEO-CTFS
#'   sites, a common value is 5.
#' @param start_with0 If `TRUE` the first column is not 1 but 0, so the first
#'   subquadrat becomes 01. The resulting quadrats look like this"
#'
#'   ```R
#'   start_with0 = TRUE    start_with0 = FALSE
#'   -------------------    -------------------
#'   04 14 24 34            14 24 34 44
#'   03 13 23 33            13 23 33 43
#'   02 12 22 32            12 22 32 42
#'   01 11 21 31            11 21 31 41
#'   ````
#' @return Returns `df` with the additional variable `subquadrat`.
#' @author Anudeep Singh and Mauro Lepore.
#' @export
#'
#' @examples
#' vft <- tibble::tribble(
#'    ~QX,  ~QY,
#'   17.9,    0,
#'    4.1,   15,
#'    6.1, 17.3,
#'    3.8,  5.9,
#'    4.5, 12.4,
#'    4.9,  9.3,
#'    9.8,  3.2,
#'   18.6,  1.1,
#'   17.3,  4.1,
#'    1.5, 16.3
#' )
#' add_subquad(vft, 20, 20, 5, 5)
#' add_subquad(vft, 20, 20, 5, 5, start_with0 = TRUE)
add_subquad <- function(df,
                        x_q,
                        y_q = x_q,
                        x_sq,
                        y_sq = x_sq,
                        start_with0 = FALSE) {
  stopifnot(is.data.frame(df))
  old <- names(df)
  df <- rlang::set_names(df, tolower)
  fgeo.utils::check_crucial_names(df, c("qx", "qy"))
  check_subquad_dims(
    df = df,
    x_q = x_q, y_q = y_q, x_sq = x_sq, y_sq = y_sq,
    start_with0 = start_with0
  )

  # Simplify nested parentheses
  x_q_mns.1 <- x_q - 0.1
  y_q_mns.1 <- y_q - 0.1

  # Conditions (odd means that the coordinate goes beyond normal limits)
  is_odd_both <- df$qx >=  x_q & df$qy >=  y_q
  is_odd_x <- df$qx >=  x_q
  is_odd_y <- df$qy >=  y_q
  is_not_odd <- TRUE

  # Cases
  w_subquad <- mutate(df,
    subquadrat = dplyr::case_when(
      is_odd_both ~ paste0(
        (1 + floor((x_q_mns.1 - x_q * floor(x_q_mns.1 / x_q)) / x_sq)),
        (1 + floor((y_q_mns.1- y_q * floor(y_q_mns.1/ y_q)) / y_sq))
      ),
      is_odd_x ~ paste0(
        (1 + floor((x_q_mns.1 - x_q * floor(x_q_mns.1 / x_q)) / x_sq)),
        (1 + floor((df$qy - y_q * floor(df$qy/ y_q)) / y_sq))
      ),
      is_odd_y ~ paste0(
        (1 + floor((df$qx - x_q * floor(df$qx/ x_q)) / x_sq)),
        (1 + floor((y_q_mns.1- y_q * floor(y_q_mns.1 / y_q)) / y_sq))
      ),
      is_not_odd ~ paste0(
        (1 + floor((df$qx - x_q * floor(df$qx/ x_q)) / x_sq)),
        (1 + floor((df$qy - y_q * floor(df$qy/ y_q)) / y_sq))
      )
    )
  )
  w_subquad <- fgeo.utils::restore_names(w_subquad, "subquadrat", old)
  if (start_with0) start_with0(w_subquad) else w_subquad
}

start_with0 <- function(x) {
  mutate(x,
    digit1 = sub("^(.).", "\\1", .data$subquadrat),
    digit2 = sub("^.(.)", "\\1", .data$subquadrat),
    subquadrat = paste0(as.numeric(.data$digit1) - 1, .data$digit2),
    digit1 = NULL,
    digit2 = NULL
  )
}

check_subquad_dims <- function(df, x_q, y_q, x_sq, y_sq, start_with0) {
  stopifnot(is.data.frame(df))
  remaining_args <- list(x_q, y_q, x_sq, y_sq)
  lapply(remaining_args, function(x) stopifnot(is.numeric(x)))
  lapply(remaining_args, function(x) stopifnot(length(x) == 1))
  lapply(remaining_args, function(x) stopifnot(all(x >= 0)))
  lapply(remaining_args, function(x) stopifnot(all(abs(x) != Inf)))
  stopifnot(is.logical(start_with0))
}

