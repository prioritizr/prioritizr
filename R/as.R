#' @include internal.R
NULL

#' @method as.list Zones
#'
#' @export
as.list.Zones <- function(x, ...) {
  attributes(x) <- NULL
  class(x) <- "list"
  x
}

#' @method as.list OptimizationProblem
#'
#' @export
as.list.OptimizationProblem <- function(x, ...) {
  rcpp_optimization_problem_as_list(x$ptr)
}

as.ZonesSpatRaster <- function(x) UseMethod("as.ZonesSpatRaster")

as.ZonesSpatRaster.ZonesRaster <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- terra::rast(x[[i]])
  }
  class(x) <- c("ZonesSpatRaster", "Zones")
  x
}

.S3method("as.ZonesSpatRaster", "ZonesRaster", as.ZonesSpatRaster.ZonesRaster)

as.ZonesRaster <- function(x) UseMethod("as.ZonesRaster")

as.ZonesRaster.ZonesSpatRaster <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- raster::stack(x[[i]])
  }
  class(x) <- c("ZonesRaster", "Zones")
  x
}

.S3method("as.ZonesRaster", "ZonesSpatRaster", as.ZonesRaster.ZonesSpatRaster)
