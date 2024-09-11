#' Rescale a matrix
#'
#' Linearly rescale a matrix. Specifically, the values in the matrix
#' are rescaled so that the maximum value in the matrix is equal to
#' a new user-specified maximum value.
#'
#' @param x [`matrix`], [`array`], [`Matrix::Matrix-class`] object.
#'
#' @param max `numeric` new maximum value in matrix. Defaults to 1000.
#'
#' @details
#' This function is particularly useful for rescaling data prior to
#' optimization to avoid numerical issues.
#' For example, boundary length (e.g., generated using [boundary_matrix()]) or
#' connectivity data (e.g., generated using [connectivity_matrix()]) can
#' contain very large values (e.g., values greater than 1,000,000)
#' and such large values can, in turn, degrade the performance of
#' exact algorithm solvers (see Details section in [presolve_check()] for
#' more information on numerical issues).
#' By using this function to rescale boundary length or connectivity
#' data prior to optimization (e.g., before using [add_boundary_penalties()] or
#' [add_connectivity_penalties()], this can help avoid numerical issues
#' during optimization.
#'
#' @return
#' A [`matrix`], [`array`], or [`Matrix::Matrix-class`] object.
#' The returned object is the is the same class as the argument to `x`.
#'
#' @examples
#' # TODO
#'
#' @seealso
#' See [boundary_matrix()] and [connectivity_matrix()] for details on
#' creating boundary length and connectivity data.
#' Also, see [presolve_check()] for information on numerical issues.
#'
#' @export
rescale_matrix <- function(x, max = 1000) {
  assert_required(x)
  assert_required(max)
  UseMethod("rescale_matrix")
}

#' @export
rescale_matrix.matrix <- function(x, max = 1000) {
  # assert valid arguments
  assert_required(x)
  assert_required(max)
  assert(
    is.matrix(x),
    assertthat::is.number(max),
    assertthat::noNA(max)
  )

  # compute threshold for rescaling
  threshold <- max(x, na.rm = TRUE) / max

  # rescale data and return result
  x / threshold
}

#' @export
rescale_matrix.array <- function(x, max = 1000) {
  # assert valid arguments
  assert_required(x)
  assert_required(max)
  assert(
    is.array(x),
    assertthat::is.number(max),
    assertthat::noNA(max)
  )

  # compute threshold for rescaling
  threshold <- max(x, na.rm = TRUE) / max

  # rescale data and return result
  x / threshold
}

#' @export
rescale_matrix.Matrix <- function(x, max = 1000) {
  # assert valid arguments
  assert_required(x)
  assert_required(max)
  assert(
    is_inherits(x, "Matrix"),
    assertthat::is.number(max),
    assertthat::noNA(max)
  )

  # compute threshold for rescaling
  threshold <- max(x@x, na.rm = TRUE) / max

  # rescale data and return result
  x@x <- x@x / threshold
  x
}
