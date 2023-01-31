#' @include external-classes.R
NULL

#' Check
#'
#' Check that the output from [assertthat::see_if()]
#' is valid.
#'
#' @param x `character` or `object`
#'
#' @details This object will return an error if the argument to `x`
#'   is `FALSE`, and for the error message, will show the error
#'   message attached to the object.
#'
#' @return An invisible `TRUE`.
#'
#' @noRd
check_that <- function(x) {
  if (!isTRUE(x))
    stop(attr(x, "msg")[1])
  invisible(TRUE)
}

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
  if (inherits(x, c("dsCMatrix")))
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
#'   numbers, the second column contains the column numbers, and the
#'   third column contains the cell values.
#
#' @param forceSymmetric `logical` should matrix be coerced to symmetric?
#'
#' @return A [`dgCMatrix-class`] object.
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
  if (utils::packageVersion("Matrix") >= 1.3) {
    args <- list(..., repr = "T")
  } else {
    args <- list(..., giveCsparse = FALSE) #nocov
  }
  # return result
  do.call(Matrix::sparseMatrix, args)
}

#' Atomic representation
#'
#' Return a pretty character representation of an object with elements and
#  names.
#'
#' @param x `object`.
#'
#' @return `character` object.
#'
#' @examples
#' repr_atomic(letters)
#' repr_atomic(letters, "characters")
#'
#' @noRd
repr_atomic <- function(x, description = "") {
  n <- length(x)
  if (nchar(description) > 0)
    description <- paste0(" ", description)
  if (length(x) <= 4) {
    x <- x[seq_len(min(length(x), 4))]
  } else {
    x <- c(x[seq_len(min(length(x), 3))], "...")
  }
  paste0(paste(x, collapse = ", "), " (", n, description, ")")
}

#' Rescale
#'
#' Linearly rescale values in a vector to range between two thresholds.
#'
#' @param x `numeric` vector.
#'
#' @param from `numeric` vector indicating the original range of the
#'  the data.
#'
#' @param to `numeric` new data range. Defaults to zero and one.
#'
#' @details This function is based on the `rescale` function in the
#'   \pkg{scales} package.
#'
#' @return `numeric` vector.
#'
#' @noRd
rescale <- function(x, from = range(x), to = c(0, 1)) {
  if ((abs(diff(from)) < 1e-10) || abs(diff(to)) < 1e-10)
    return(mean(to))
  (x - from[1]) / diff(from) * diff(to) + to[1]
}
