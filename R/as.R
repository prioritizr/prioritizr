#' @include internal.R
NULL

#' @method as.list Zones
#' @export
as.list.Zones <- function(x, ...) {
  attributes(x) <- NULL
  class(x) <- "list"
  x
}

#' @method as.list OptimizationProblem
#' @export
as.list.OptimizationProblem <- function(x, ...) {
  rcpp_optimization_problem_as_list(x$ptr)
}

as.ZonesSpatRaster <- function(x) UseMethod("as.ZonesSpatRaster")

#' @method as.ZonesSpatRaster ZonesRaster
#' @export
as.ZonesSpatRaster.ZonesRaster <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- terra::rast(x[[i]])
  }
  class(x) <- c("ZonesSpatRaster", "Zones")
  x
}

as.ZonesRaster <- function(x) UseMethod("as.ZonesRaster")

#' @method as.ZonesRaster ZonesSpatRaster
#' @export
as.ZonesRaster.ZonesSpatRaster <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- raster::stack(x[[i]])
  }
  class(x) <- c("ZonesRaster", "Zones")
  x
}
