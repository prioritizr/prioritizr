#' @include external-classes.R
NULL

#' Convert sparse matrix to triplet data.frame
#'
#' Convert a sparse matrix to a triplet `data.frame`.
#'
#' @param x `Matrix` object.
#'
#' @return A `data.frame` object.
#'
#' @noRd
matrix_to_triplet_dataframe <- function(x) {
  if (inherits(x, "dsCMatrix"))
    x <- as_Matrix(x, "dsTMatrix")
  if (inherits(x, c("dgCMatrix", "matrix")))
    x <- as_Matrix(x, "dgTMatrix")
  data.frame(i = x@i + 1, j = x@j + 1, x = x@x)
}

#' Convert a triplet data.frame to a matrix
#'
#' Convert a triplet `data.frame` object to a sparse matrix.
#'
#' @param x `data.frame` object. The first column contains the row
#' numbers, the second column contains the column numbers, and the
#' third column contains the cell values.
#
#' @param forceSymmetric `logical` should matrix be coerced to symmetric?
#'
#' @return A [`Matrix::dgCMatrix-class`] object.
#'
#' @noRd
triplet_dataframe_to_matrix <- function(x, forceSymmetric = FALSE, ...) {
  # assert arguments are valid
  assert(
    inherits(x, "data.frame"),
    isTRUE(ncol(x) == 3),
    isTRUE(all(x[[1]] == round(x[[1]]))),
    isTRUE(all(x[[2]] == round(x[[2]]))),
    assertthat::is.flag(forceSymmetric),
    assertthat::noNA(forceSymmetric))
  # create sparse amtrix
  m <- triplet_sparse_matrix(i = x[[1]], j = x[[2]], x = x[[3]], ...)
  if (forceSymmetric) {
    # force the matrix to be symmetric
    # we cannot guarantee that the cells that are filled in belong to either
    # the upper or the lower diagonal
    m2 <- matrix(c(m@j + 1, m@i + 1, m@x), ncol = 3)
    m2 <- m2[m2[, 1] != m2[, 2], ]
    m[m2[, 1:2]] <- m2[, 3]
    m <- Matrix::forceSymmetric(m)
    m <- as_Matrix(m, "dsCMatrix")
  } else {
    m <- as_Matrix(m, "dgCMatrix")
  }
  # return result
  m
}

#' Sparse matrix (triplet)
#'
#' Create a \pkg{Matrix} triplet sparse matrix object.
#'
#' @details
#' This function is a wrapper for [Matrix::sparseMatrix()] that
#' is compatible with versions 1.2 and 1.3.
#'
#' @param ... passed to [Matrix::sparseMatrix()].
#
#' @return A [`dgTMatrix-class`] object.
#'
#' @noRd
triplet_sparse_matrix <- function(...) {
  # prepare arguments for creating matrix
  if (utils::packageVersion("Matrix") >= as.package_version("1.3")) {
    args <- list(..., repr = "T")
  } else {
    args <- list(..., giveCsparse = FALSE) #nocov
  }
  # return result
  do.call(Matrix::sparseMatrix, args)
}

#' Rescale
#'
#' Linearly rescale values in a vector to range between two thresholds.
#'
#' @param x `numeric` vector.
#'
#' @param from `numeric` vector indicating the original range of the
#' the data.
#'
#' @param to `numeric` new data range. Defaults to zero and one.
#'
#' @details
#' This function is based on the `rescale` function in the
#' \pkg{scales} package.
#'
#' @return A `numeric` vector.
#'
#' @noRd
rescale <- function(x, from = range(x), to = c(0, 1)) {
  if ((abs(diff(from)) < 1e-10) || abs(diff(to)) < 1e-10)
    return(mean(to))
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

#' Which least different?
#'
#' Identify the index of which set of parameters is most similar to a
#' reference set of parameters?
#'
#' @param x `numeric` matrix with a collection of parameter sets.
#' Here a parameter set refers to a set of different parameters
#' (e.g., weight parameter values for different objectives).
#' Each column corresponds to a different parameter in the set, and each row
#' corresponds to a different parameter set.
#'
#' @param y `numeric` matrix with the reference parameter set.
#'
#' @details
#' The most similar parameter set is determined based on Euclidean distances.
#' Thus this approach assumes that each of parameters in `x` and `y` (in other
#' words, the values in different columns of `x` and `y`) are all the same
#' units.
#'
#' @return
#' An `integer` value denoting the row number of `x` that has
#' the most similar parameters to those in `y`.
#'
#' @noRd
which_least_different <- function(x, y) {
  # assert valid arguments
  assert(
    is.numeric(x),
    is.numeric(y),
    is.matrix(x),
    is.matrix(y),
    identical(ncol(x), ncol(y)),
    nrow(x) >= 1,
    identical(nrow(y), 1L),
    .internal = TRUE
  )
  # if x has a single parameter set, then just return 1
  if (identical(nrow(x), 1L)) return(1L)
  # compute Euclidean distances between and parameter sets
  ## note that we don't bother with sqrt() after computing the sum of squares,
  ## because this is not needed to determine which is the smallest
  ## Euclidean distance and skipping this is more computationally efficient
  d <- rowSums(
    (x - matrix(c(y), ncol = ncol(x), nrow = nrow(x), byrow = TRUE))^2
  )
  # return index of parameter set with smallest value
  which.min(d)
}

#' Sanitize solver output
#'
#' This function is used to process solver outputs to ensure consistency.
#' In particular, integer values are rounded, and values are clamped
#' according to the lower and upper bounds. This is needed to resolve
#' discrepancies that arise due to floating point arithmetic.
#'
#' @param x `numeric` vector with solution values.
#'
#' @param lb `numeric` vector with lower bounds for values.
#'
#' @param ub `numeric` vector with upper bounds for values.
#'
#' @param is_integer `logical` vector with values indicating if each
#' value should have an integer value or not.
#'
#' @return A `numeric` vector with updated values for `x`.
#'
#' @noRd
sanitize_solver_output <- function(x, lb, ub, is_integer) {
  # round integer variables because default precision is 1e-5
  x[is_integer] <- round(x[is_integer])
  # truncate variables to account for rounding issues
  x <- pmax(x, lb)
  x <- pmin(x, ub)
  # return solution
  x
}
