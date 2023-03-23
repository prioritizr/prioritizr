#' @include internal.R
NULL

#' Category vector
#'
#' Convert an object containing binary (`integer`) columns into a
#' `integer` vector indicating the column index where each row is
#' `1`.
#'
#' @param x `matrix`, `data.frame`, or [sf::sf()] object.
#'
#' @details This function is conceptually similar to [base::max.col()]
#'   except that rows with no values equal to `1` values are assigned a
#'   value of zero. Also, note that in the argument to `x`, each row must
#'   contain only a single value equal to `1`.
#'
#' @return An `integer` vector.
#'
#' @seealso [base::max.col()]
#'
#' @examples
#' # create matrix with logical columns
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
category_vector <- function(x) {
  assert_required(x)
  UseMethod("category_vector")
}

#' @rdname category_vector
#' @method category_vector data.frame
#' @export
category_vector.data.frame <- function(x) {
  assert(
    is.data.frame(x),
    nrow(x) >= 1,
    ncol(x) >= 1,
    all_columns_inherit(x, "numeric")
  )
  category_vector(as.matrix(x))
}

#' @rdname category_vector
#' @method category_vector sf
#' @export
category_vector.sf <- function(x) {
  assert(inherits(x, "sf"))
  category_vector(sf::st_drop_geometry(x))
}

#' @rdname category_vector
#' @method category_vector Spatial
#' @export
category_vector.Spatial <- function(x) {
  assert(inherits(x, "Spatial"))
  cli_warning(sp_pkg_deprecation_notice)
  category_vector(x@data)
}

#' @rdname category_vector
#' @method category_vector matrix
#' @export
category_vector.matrix <- function(x) {
  assert(
    is.matrix(x),
    is.numeric(x),
    nrow(x) >= 1,
    ncol(x) >= 1,
    all_binary(x)
  )
  out <- max.col(x, ties.method = "first")
  out[out == 1 & x[, 1] == 0] <- 0
  out
}
