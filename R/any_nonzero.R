#' @include internal.R
NULL

#' Any non-zero?
#'
#' Check if an object has any non-zero values?
#'
#' @param object.
#'
#' @return A `logical` value.
#'
#' @noRd
any_nonzero <- function(x) UseMethod("any_nonzero")

assertthat::on_failure(any_nonzero) <- function(call, env) {
  x <- eval(call$x, envir = env)
  if (
    inherits(x, c("SpatRaster", "ZonesSpatRaster", "Raster", "ZonesRaster"))
  ) {
    msg <- paste0(
      "{.arg ",
      deparse(call$x),
      "} must not have a layer with only zero values."
    )
  } else if (inherits(x, c("data.frame", "sf", "Spatial"))) {
    msg <- paste0(
      "{.arg ",
      deparse(call$x),
      "} must not have a column with only zero values."
    )
  } else {
    msg <- paste0(
      "{.arg ",
      deparse(call$x),
      "} must not have only zero values."
    )
  }
  msg
}

any_nonzero.default <- function(x) {
  cli::cli_abort("{.arg x} is not a recognized class.")
}

#' @export
any_nonzero.numeric <- function(x) {
  if (all(!is.finite(x))) return(TRUE)
  isTRUE(max(abs(x), na.rm = TRUE) > 1e-6)
}

#' @export
any_nonzero.Matrix <- function(x) {
  any_nonzero(x@x)
}

#' @export
any_nonzero.matrix <- function(x) {
  any_nonzero(c(x))
}

#' @export
any_nonzero.Raster <- function(x) {
  any_nonzero(terra::rast(x))
}

#' @export
any_nonzero.SpatRaster <- function(x) {
  x <- as.matrix(terra::global(x, "range", na.rm = TRUE))
  if (all(!is.finite(x))) return(TRUE)
  all(rowSums(abs(x) > 1e-6, na.rm = TRUE) > 0)
}

#' @export
any_nonzero.ZonesRaster <- function(x) {
  any_nonzero(terra::rast(raster::stack(raster::as.list(x))))
}

#' @export
any_nonzero.ZonesSpatRaster <- function(x) {
  any_nonzero(terra::rast(terra::as.list(x)))
}

#' @export
any_nonzero.data.frame <- function(x) {
  all(vapply(x, any_nonzero, logical(1)))
}

#' @export
any_nonzero.Spatial <- function(x) {
  all(vapply(x@data, any_nonzero, logical(1)))
}

#' @export
any_nonzero.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), any_nonzero, logical(1)))
}
