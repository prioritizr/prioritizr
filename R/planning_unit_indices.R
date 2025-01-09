#' @include internal.R
NULL

#' Planning unit index
#'
#' Extract indices for planning units in an object.
#'
#' @param x Planning unit data.
#'
#' @param cost_column `character` value indicating the column with the cost
#'  values.
#'
#' @param ... not used.
#'
#' @return An `integer` vector.
#'
#' @name planning_unit_indices
#'
#' @noRd
planning_unit_indices <- function(x, ...) UseMethod("planning_unit_indices")

#' @export
planning_unit_indices.default <- function(x, ...) {
  cli::cli_abort("{.arg x} is not a recognized class.")
}

#' @export
planning_unit_indices.sf <- function(x, cost_column, ...) {
  assert(
    inherits(x, "sf"),
    is.character(cost_column),
    all(assertthat::has_name(x, cost_column))
  )
  assert_dots_empty()
  x <- sf::st_drop_geometry(x)
  x <- x[, cost_column, drop = FALSE]
  unname(which(rowSums(!is.na(as.matrix(x))) > 0))
}

#' @export
planning_unit_indices.SpatRaster <- function(x, ...) {
  assert(inherits(x, "SpatRaster"))
  assert_dots_empty()
  terra::cells(terra::allNA(x), 0)[[1]]
}

#' @export
planning_unit_indices.data.frame <- function(x, cost_column, ...) {
  assert(
    is.data.frame(x),
    is.character(cost_column),
    all(assertthat::has_name(x, cost_column))
  )
  assert_dots_empty()
  x <- as.data.frame(x)
  x <- x[, cost_column, drop = FALSE]
   unname(which(rowSums(!is.na(as.matrix(x))) > 0))
}

#' @export
planning_unit_indices.matrix <- function(x, ...) {
  assert(is.matrix(x))
  assert_dots_empty()
  unname(which(rowSums(!is.na(x)) > 0))
}

#' @export
planning_unit_indices.numeric <- function(x, ...) {
  assert(is.numeric(x))
  planning_unit_indices(matrix(x, ncol = 1))
}

#' @export
planning_unit_indices.Raster <- function(x, ...) {
  assert(inherits(x, "Raster"))
  assert_dots_empty()
  if (raster::nlayers(x) == 1) {
    x <- raster::Which(!is.na(x), cells = TRUE)
  } else {
    x <- raster::Which(max(!is.na(x)) > 0, cells = TRUE)
  }
  x
}

#' @export
planning_unit_indices.Spatial <- function(x, cost_column, ...) {
  assert(
    inherits(x, "Spatial"),
    is.character(cost_column),
    all(assertthat::has_name(x, cost_column))
  )
  assert_dots_empty()
  x <- as.data.frame(x)
  x <- x[, cost_column, drop = FALSE]
  unname(which(rowSums(!is.na(as.matrix(x))) > 0))
}
