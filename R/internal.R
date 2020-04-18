#' Check
#'
#' Check that the output from \code{\link[assert_that]{see_if}}
#' is valid.
#'
#' @param x \code{character} or \code{object}
#'
#' @details This object will return an error if the argument to \code{x}
#'   is \code{FALSE}, and for the error message, will show the error
#'   message attached to the object.
#'
#' @return invisible \code{TRUE}.
#'
#' @noRd
check_that <- function(x) {
  if (!isTRUE(x))
    stop(attr(x, "msg")[1])
  invisible(TRUE)
}

#' Convert sparse matrix to triplet data.frame
#'
#' Convert a sparse matrix to a triplet \code{data.frame}.
#'
#' @param x \code{Matrix} object.
#'
#' @noRd
matrix_to_triplet_dataframe <- function(x) {
  if (inherits(x, c("dsCMatrix")))
    x <- methods::as(x, "dsTMatrix")
  if (inherits(x, c("dgCMatrix", "matrix")))
    x <- methods::as(x, "dgTMatrix")
  data.frame(i = x@i + 1, j = x@j + 1, x = x@x)
}

#' Convert a triplet data.frame to a matrix
#'
#' Convert a triplet data.framr object to a sparse matrix.
#'
#' @param x \code{data.frame} object. The first column contains the row
#'   numbers, the second column contains the column numbers, and the
#'   third column contains the cell values.
#
#' @return \code{\link[Matrix]{dgCMatrix-class}} object.
#'
#' @noRd
triplet_dataframe_to_matrix <- function(x, forceSymmetric=FALSE, ...) {
  assertthat::assert_that(inherits(x, "data.frame"), isTRUE(ncol(x) == 3),
    isTRUE(all(x[[1]] == round(x[[1]]))), isTRUE(all(x[[2]] == round(x[[2]]))))
  # create sparse amtrix
  m <- Matrix::sparseMatrix(i = x[[1]], j = x[[2]], x = x[[3]],
                            giveCsparse = FALSE, ...)
  if (forceSymmetric) {
    # force the matrix to be symmetric
    # we cannot gurantee that the cells that are filled in belong to either
    # the upper or the lower diagonal
    m2 <- matrix(c(m@j + 1, m@i + 1, m@x), ncol = 3)
    m2 <- m2[m2[, 1] != m2[, 2], ]
    m[m2[, 1:2]] <- m2[, 3]
    return(Matrix::forceSymmetric(m))
  } else {
    # return matrix in compressed format
    return(methods::as(m, "dgCMatrix"))
  }
}

#' Align text
#'
#' Format text by adding a certain number of spaces after new line characters.
#'
#' @param x \code{character} text.
#'
#' @param n \code{integer} number of spaces.
#'
#' @return \code{character}.
#'
#' @examples
#' # make some text
#' original_text <- "animals: horse\npig\nbear"
#'
#' # print text
#' message(original_text)
#'
#' # this look really ugly so we will align it
#' aligned_text <- align_text(original_text, 9)
#'
#' # print aligned text
#' message(aligned_text)
#'
#' @noRd
align_text <- function(x, n) {
  assertthat::assert_that(assertthat::is.string(x), assertthat::is.count(n))
  if (!grepl("\n", x))
    return(x)
  return(gsub("\n", paste0("\n", paste(rep(" ", n), collapse = "")), x,
              fixed = TRUE))
}

#' Default solver name
#'
#' This function returns the name of the default solver. If no sovlers are
#' detected on the system, then a \code{NULL} object is retured.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{Rsymphony}, \pkg{lpsymphony}, \pkg{gurobi}.
#'
#' @return \code{character} indicating the name of the default solver.
#'
#' @noRd
default_solver_name <- function() {
  if (requireNamespace("gurobi", quietly = TRUE)) {
    return("gurobi")
  } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
    return("Rsymphony")
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    return("lpsymphony")
  } else {
    return(NULL)
  }
}

#' Any solvers installed?
#'
#' Test if any solvers are installed.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{Rsymphony}, \pkg{lpsymphony}, \pkg{gurobi}.
#'
#' @return \code{logical} value indicating if any solvers are installed.
#'
#' @noRd
any_solvers_installed <- function() {
  !is.null(default_solver_name())
}

#' Atomic representation
#'
#' Return a pretty character representation of an object with elements and
#  names.
#'
#' @param x \code{object}.
#'
#' @return \code{character} object.
#'
#' @examples
#' repr_atomic(letters)
#' repr_atomic(letters, "characters")
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

#' No extra arguments
#'
#' Check that no additional unused arguments have been supplied to a function
#' through the \code{...}.
#'
#' @param ... arguments that are not used.
#'
#' @return \code{logical} indicating success.
#'
#' @noRd
no_extra_arguments <- function(...) {
  return(length(list(...)) == 0)
}

assertthat::on_failure(no_extra_arguments) <- function(call, env) {
  "unused arguments"
}

#' Verify if assertion is met
#'
#' Verify if an assertion is met and throw a \code{\link[base]{warning}} if it
#' is not. This function is equivalent to \code{\link[assertthat]{assert_that}}
#' except that it throws warnings and not errors.
#'
#' @param x \code{logical} condition.
#'
#' @return \code{logical} if assertion is met and a \code{warning} if it is not.
#'
#' @noRd
verify_that <- function(..., env = parent.frame()) {
  res <- assertthat::validate_that(..., env = env)
  if (isTRUE(res))
      return(TRUE)
  warning(res, immediate. = TRUE)
  FALSE
}

#' Are rasters comparable?
#'
#' This function checks if two \code{\link[raster]{Raster-class}} objects
#' are comparable.
#'
#' @param x \code{\link[raster]{Raster-class}} object.
#'
#' @param y \code{\link[raster]{Raster-class}} object.
#'
#' @return \code{logical} indicating if the two
#'   \code{\link[raster]{Raster-class}} objects have the same
#'   resolution, extent, dimensionality, and coordinate system.
#'
#' @noRd
is_comparable_raster <- function(x, y) {
  assertthat::assert_that(inherits(x, "Raster"), inherits(y, "Raster")) &&
  sf::st_crs(x@crs) == sf::st_crs(y@crs) &&
  raster::compareRaster(x, y, crs = FALSE, res = TRUE, tolerance = 1e-5,
                        stopiffalse = FALSE)
}

assertthat::on_failure(is_comparable_raster) <- function(call, env) {
  paste0(deparse(call$x), " and ", deparse(call$y),  " are not comparable: ",
         "they have different spatial resolutions, extents, ",
         "coordinate reference systems, or dimensionality (rows / columns)")
}

#' Rescale
#'
#' Linearly rescale values in a vector to range between two thresholds.
#'
#' @param x \code{numeric} vector.
#'
#' @param from \code{numeric} vector indicating the original range of the
#'  the data.
#'
#' @param to \code{numeric} new data range. Defaults to zero and one.
#'
#' @details This function is based on the \code{rescale} function in the
#'   \pkg{scales} package.
#'
#' @return \code{numeric} vector.
#'
#' @noRd
rescale <- function(x, from = range(x), to = c(0, 1)) {
  if ((abs(diff(from)) < 1e-10) || abs(diff(to)) < 1e-10)
    return(mean(to))
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

#' Do extents intersect?
#'
#' Verify if the extents of two spatial objects intersect or not.
#'
#' @param x \code{\link[raster]{Raster-class}}, \code{\link[sp]{Spatial-class}},
#'   or \code{\link[sf]{sf}} object.
#'
#' @param y \code{\link[raster]{Raster-class}}, \code{\link[sp]{Spatial-class}},
#'   or \code{\link[sf]{sf}} object.
#'
#' @return \code{logical}.
#'
#' @noRd
intersecting_extents <- function(x, y) {
  assertthat::assert_that(
    inherits(x, c("Raster", "Spatial", "sf")),
    inherits(y, c("Raster", "Spatial", "sf")))
  isTRUE(sf::st_intersects(
    sf::st_as_sf(methods::as(raster::extent(x), "SpatialPolygons")),
    sf::st_as_sf(methods::as(raster::extent(y), "SpatialPolygons")),
    sparse = FALSE)[[1]])
}

#' Geometry classes
#'
#' Extract geometry class names from a \code{\link[sf]{sf}} object.
#'
#' @param x  \code{\link[sf]{sf}} object.
#'
#' @return \code{character} object.
#'
#' @noRd
geometry_classes <- function(x) {
  assertthat::assert_that(inherits(x, "sf"))
  vapply(sf::st_geometry(x), class, character(3))[2, ]
}
