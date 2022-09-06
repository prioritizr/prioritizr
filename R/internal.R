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
#' @return invisible `TRUE`.
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
#' @return [`dgCMatrix-class`] object.
#'
#' @noRd
triplet_dataframe_to_matrix <- function(x, forceSymmetric = FALSE, ...) {
  # assert arguments are valid
  assertthat::assert_that(
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
#' @return [`dgTMatrix-class`] object.
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

#' Align text
#'
#' Format text by adding a certain number of spaces after new line characters.
#'
#' @param x `character` text.
#'
#' @param n `integer` number of spaces.
#'
#' @return `character`.
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

#' Any solvers installed?
#'
#' Test if any solvers are installed.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{Rsymphony}, \pkg{lpsymphony}, \pkg{gurobi}.
#'
#' @return `logical` value indicating if any solvers are installed.
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
#' @param x `object`.
#'
#' @return `character` object.
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
#' through the `...`.
#'
#' @param ... arguments that are not used.
#'
#' @return `logical` indicating success.
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
#' Verify if an assertion is met and throw a [base::warning()] if it
#' is not. This function is equivalent to [assertthat::assert_that()]
#' except that it throws warnings and not errors.
#'
#' @param x `logical` condition.
#'
#' @return `logical` if assertion is met and a `warning` if it is not.
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
#' This function checks if two [`Raster-class`] objects
#' are comparable.
#'
#' @param x [`Raster-class`] object.
#'
#' @param y [`Raster-class`] object.
#'
#' @return `logical` indicating if the two
#'   [`Raster-class`] objects have the same
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

#' Do extents intersect?
#'
#' Verify if the extents of two spatial objects intersect or not.
#'
#' @param x [`Raster-class`], [`Spatial-class`],
#'   or [sf::sf()] object.
#'
#' @param y [`Raster-class`], [`Spatial-class`],
#'   or [sf::sf()] object.
#'
#' @return `logical`.
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
#' Extract geometry class names from a [sf::sf()] object.
#'
#' @param x  [sf::sf()] object.
#'
#' @return `character` object.
#'
#' @noRd
geometry_classes <- function(x) {
  assertthat::assert_that(inherits(x, "sf"))
  vapply(sf::st_geometry(x), class, character(3))[2, ]
}

#' Convert to Matrix
#'
#' Convert an object to a matrix class provided by the \pkg{Matrix} package.
#'
#' @param object object.
#'
#' @param class `character` name of new classes.
#'
#' @details
#' This function is a wrapper that is designed to provide
#' compatibility with older and newer versions of the \pkg{Matrix} package.
#'
#' @return `Matrix` object.
#'
#' @noRd
as_Matrix <- function(object, class) {
  # assert valid argument
  assertthat::assert_that(
    assertthat::is.string(class),
    assertthat::noNA(class)
  )
  # if we just want to convert to generic Matrix class then do that...
  if (identical(class, "Matrix")) {
    return(methods::as(object, class))
  }
  # convert matrix
  # nocov start
  if (utils::packageVersion("Matrix") >= as.package_version("1.4-2")) {
    if (identical(class, "dgCMatrix")) {
      c1 <- "dMatrix"
      c2 <- "generalMatrix"
      c3 <- "CsparseMatrix"
    } else if (identical(class, "dgTMatrix")) {
      c1 <- "dMatrix"
      c2 <- "generalMatrix"
      c3 <- "TsparseMatrix"
    } else if (identical(class, "dsCMatrix")) {
      c1 <- "dMatrix"
      c2 <- "symmetricMatrix"
      c3 <- "CsparseMatrix"
    } else if (identical(class, "dsTMatrix")) {
      c1 <- "dMatrix"
      c2 <- "symmetricMatrix"
      c3 <- "TsparseMatrix"
    } else if (identical(class, "lgCMatrix")) {
      c1 <- "lMatrix"
      c2 <- "generalMatrix"
      c3 <- "CsparseMatrix"
    } else {
      stop("argument to \"class\" not recognized")
    }
    out <- methods::as(methods::as(methods::as(object, c1), c2), c3)
  } else {
    out <- methods::as(object, class)
  }
  # nocov end
  # return result
  out
}
