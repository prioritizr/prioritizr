#' @include internal.R
NULL

#' Category vector
#'
#' Convert a \code{data.frame} or \code{matrix} containing binary
#' (\code{integer}) fields (columns) into a \code{integer} \code{vector}
#' indicating the column index where each row is \code{1}.
#'
#' @param x \code{matrix} object.
#'
#' @details This function is conceptually similar to \code{\link[base]{max.col}}
#'   except that rows with no values equal to \code{1} values are assigned a
#'   value of zero. Also, note that in the argument to \code{x}, each row must
#'   contain only a single value equal to \code{1}.
#'
#' @return \code{integer} \code{vector}
#'
#' @seealso \code{\link[base]{max.col}}
#'
#' @examples
#' # create matrix with logical fields
#' x <- matrix(c(1, 0, 0, NA, 0, 1, 0, NA, 0, 0, 0, NA), ncol = 3)
#'
#' # print matrix
#' print(x)
#'
#' # convert to category vector
#' y <- category_vector(x)
#'
#' # print category vector
#' print(y)
#' @name category_vector
#'
#' @export
category_vector <- function(x) UseMethod("category_vector")

#' @rdname category_vector
#' @method category_vector data.frame
#' @export
category_vector.data.frame <- function(x) {
  assertthat::assert_that(inherits(x, c("data.frame", "tbl_df")),
                          nrow(x) >= 1, ncol(x) >= 1,
                          all(vapply(x, inherits, logical(1), "numeric")))
  category_vector(as.matrix(x))
}

#' @rdname category_vector
#' @method category_vector matrix
#' @export
category_vector.matrix <- function(x) {
  assertthat::assert_that(is.matrix(x),
                          is.numeric(x),
                          all(round(x) == x, na.rm = TRUE),
                          min(x, na.rm = TRUE) >= 0,
                          max(x, na.rm = TRUE) <= 1,
                          nrow(x) >= 1,  ncol(x) >= 1)
  out <- max.col(x, ties.method = "first")
  out[out == 1 & x[, 1] == 0] <- 0
  out
}
