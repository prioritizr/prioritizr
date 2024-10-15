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
#'   numbers, the second column contains the column numbers, and the
#'   third column contains the cell values.
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

#' Can process in memory?
#'
#' Check if a [terra::rast()] can be processed in memory?
#'
#' @param x [terra::rast()] object.
#'
#' @param n `integer` number of copies.
#'
#' @return A `logical` indicating if the object can be processed in memory.
#'
#' @noRd
terra_can_process_in_memory <- function(x, n) {
  assert(
    is_inherits(x, "SpatRaster"),
    assertthat::is.count(n),
    assertthat::noNA(n)
  )
  msg <- utils::capture.output(
    y <- terra::mem_info(x = x, n = n)
  )
  # from https://github.com/rspatial/terra/blob/a72eb63cf178c637f76859476487cd8345b529bc/R/messages.R#L59
  isTRUE(round(y[[5]]) != 0)
}
