#' Add column `subquadrat` based on `QX` and `QY` coordinates.
#'
#' @param data A dataframe with quadrat coordinates `QX` and `QY` (e.g. a
#'   _ViewFullTable_).
#' @param x_q,y_q Size in meters of a quadrat's side. For ForestGEO sites, a
#'   common value is 20.
#' @param x_sq,y_sq Size in meters of a subquadrat's side. For ForestGEO sites,
#'   a common value is 5.
#' @param subquad_offset Either `-1` or `1`, to rest or add one unit to the
#'   digit of each subquadrat that represents the column number.
#'
#'   ```R
#'   First column is 0    First column is 1
#'   -----------------    -----------------
#'      04 14 24 34          14 24 34 44
#'      03 13 23 33          13 23 33 43
#'      02 12 22 32          12 22 32 42
#'      01 11 21 31          11 21 31 41
#'   ```
#' @return Returns `data` with the additional variable `subquadrat`.
#' @author Anudeep Singh and Mauro Lepore.
#'
#' @examples
#' vft <- tribble(
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
#' 
#' add_subquad(vft, 20, 20, 5, 5)
#' 
#' add_subquad(vft, 20, 20, 5, 5, subquad_offset = -1)
#' 
#' @family functions to add columns to dataframes
#' @family functions for ForestGEO data
#' @family functions for fgeo vft
#' @export
add_subquad <- function(data,
                        x_q,
                        y_q = x_q,
                        x_sq,
                        y_sq = x_sq,
                        subquad_offset = NULL) {
  stopifnot(is.data.frame(data))

  data_ <- set_names(data, tolower) %>%
    check_crucial_names(c("qx", "qy")) %>%
    check_subquad_dims(
      x_q = x_q, y_q = y_q, x_sq = x_sq, y_sq = y_sq,
      subquad_offset = subquad_offset
    ) %>%
    type_ensure(c("qx", "qy"), type = "numeric") %>%
    dplyr::filter(!is.na(.data$qx), !is.na(.data$qy))

  # Simplify nested parentheses
  x_q_mns.1 <- x_q - 0.1
  y_q_mns.1 <- y_q - 0.1

  # Conditions (odd means that the coordinate goes beyond normal limits)
  is_odd_both <- data_$qx >= x_q & data_$qy >= y_q
  is_odd_x <- data_$qx >= x_q
  is_odd_y <- data_$qy >= y_q
  is_not_odd <- TRUE

  # Cases
  data_ <- mutate(data_,
    subquadrat = dplyr::case_when(
      is_odd_both ~ paste0(
        (1 + floor((x_q_mns.1 - x_q * floor(x_q_mns.1 / x_q)) / x_sq)),
        (1 + floor((y_q_mns.1 - y_q * floor(y_q_mns.1 / y_q)) / y_sq))
      ),
      is_odd_x ~ paste0(
        (1 + floor((x_q_mns.1 - x_q * floor(x_q_mns.1 / x_q)) / x_sq)),
        (1 + floor((data_$qy - y_q * floor(data_$qy / y_q)) / y_sq))
      ),
      is_odd_y ~ paste0(
        (1 + floor((data_$qx - x_q * floor(data_$qx / x_q)) / x_sq)),
        (1 + floor((y_q_mns.1 - y_q * floor(y_q_mns.1 / y_q)) / y_sq))
      ),
      is_not_odd ~ paste0(
        (1 + floor((data_$qx - x_q * floor(data_$qx / x_q)) / x_sq)),
        (1 + floor((data_$qy - y_q * floor(data_$qy / y_q)) / y_sq))
      )
    )
  )

  data_ <- rename_matches(data_, data)

  if (!is.null(subquad_offset)) {
    recode_subquad(data_, offset = subquad_offset)
  } else {
    data_
  }
}

#' Recode subquadrat.
#'
#' @param x A dataframe with the variable `subquadrat`.
#' @param offset A number; either -1 or 1, to rest or add one unit to the number
#'   of column of each subquadrat.
#'
#'   ```R
#'   First column is 0    First column is 1
#'   -----------------    -----------------
#'      04 14 24 34          14 24 34 44
#'      03 13 23 33          13 23 33 43
#'      02 12 22 32          12 22 32 42
#'      01 11 21 31          11 21 31 41
#'   ```
#'
#' @return A modified version of the input.
#'
#' @examples
#' first_subquad_11 <- tibble(subquadrat = c("11", "12", "22"))
#' first_subquad_11
#' 
#' first_subquad_01 <- recode_subquad(first_subquad_11, offset = -1)
#' first_subquad_01
#' 
#' first_subquad_11 <- recode_subquad(first_subquad_01, offset = 1)
#' first_subquad_11
#' @keywords internal
#' @export
recode_subquad <- function(data, offset = -1) {
  check_recode_subquad(data, offset = offset)

  mutate(data,
    digit1 = sub("^(.).", "\\1", .data$subquadrat),
    digit2 = sub("^.(.)", "\\1", .data$subquadrat),
    subquadrat = paste0(as.numeric(.data$digit1) + offset, .data$digit2),
    digit1 = NULL,
    digit2 = NULL
  )
}

check_recode_subquad <- function(data, offset) {
  stopifnot(is.data.frame(data))
  check_crucial_names(data, "subquadrat")
  stopifnot(offset %in% c(1, -1))
  stop_if_invalid_subquad(data, offset = offset)
  invisible(data)
}

stop_if_invalid_subquad <- function(data, offset) {
  # What is the first column?
  column1 <- c(14, 24, 34, 44, 13, 23, 33, 43, 12, 22, 32, 42, 11, 21, 31, 41)
  column0 <- c(04, 14, 24, 34, 03, 13, 23, 33, 02, 12, 22, 32, 01, 11, 21, 31)
  column0 <- pad_dbl(column0, 2, pad = 0)
  if (offset == 1) {
    subquad <- column0
  } else {
    subquad <- column1
  }
  # Check that the subquadrats in the data make sense with the offset provided
  if (!all(unique(data$subquadrat) %in% as.character(subquad))) {
    stop(
      "Invalid subquadrats were detected.\n",
      "  * You chose `offset` = ", offset, "\n",
      "  * The subquadrats of your data are these:\n",
      commas(unique(data$subquadrat))
    )
  }
}

check_subquad_dims <- function(data, x_q, y_q, x_sq, y_sq, subquad_offset) {
  stopifnot(is.data.frame(data))
  remaining_args <- list(x_q, y_q, x_sq, y_sq)
  lapply(remaining_args, function(x) stopifnot(is.numeric(x)))
  lapply(remaining_args, function(x) stopifnot(length(x) == 1))
  lapply(remaining_args, function(x) stopifnot(all(x >= 0)))
  lapply(remaining_args, function(x) stopifnot(all(abs(x) != Inf)))
  if (!is.null(subquad_offset)) stopifnot(is.numeric(subquad_offset))
  invisible(data)
}
