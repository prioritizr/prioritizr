#' @include internal.R
NULL

#' All positive?
#'
#' Check if an object has only positive values?
#'
#' @param object.
#'
#' @return A `logical` value.
#'
#' @noRd
all_positive <- function(x) UseMethod("all_positive")

assertthat::on_failure(all_positive) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x), "} must not have negative values."
  )
}

#' @export
all_positive.default <- function(x) {
  cli::cli_abort("{.arg x} is not a recognized class.")
}

#' @export
all_positive.numeric <- function(x) {
  all(x >= 0, na.rm = TRUE)
}

#' @export
all_positive.Matrix <- function(x) {
  all_positive(x@x)
}

#' @export
all_positive.matrix <- function(x) {
  all_positive(c(x))
}

#' @export
all_positive.Raster <- function(x) {
  all_positive(terra::rast(x))
}

#' @export
all_positive.SpatRaster <- function(x) {
  all_positive(terra::global(x, "min", na.rm = TRUE)[[1]])
}

#' @export
all_positive.ZonesRaster <- function(x) {
  all_positive(terra::rast(raster::stack(raster::as.list(x))))
}

#' @export
all_positive.ZonesSpatRaster <- function(x) {
  all_positive(terra::rast(terra::as.list(x)))
}


#' @export
all_positive.data.frame <- function(x) {
  all(vapply(x, all_positive, logical(1)))
}

#' @export
all_positive.Spatial <- function(x) {
  all(vapply(x@data, all_positive, logical(1)))
}

#' @export
all_positive.sf <- function(x) {
  all(vapply(sf::st_drop_geometry(x), all_positive, logical(1)))
}
