#' @include internal.R
NULL

#' Find intersecting units
#'
#' Find which of the units in a spatial data object intersect
#' with the units in another spatial data object.
#'
#' @param x [sf::st_sf()] or [terra::rast()] object.
#'
#' @param y [sf::st_sf()] or [terra::rast()] object.
#'
#' @return An `integer` vector of indices of the units in `x` that intersect
#'   with `y`.
#'
#' @name intersecting_units
#'
#' @details
#' The performance of this function for large [terra::rast()] objects
#' can be improved by increasing the GDAL cache size.
#' The default cache size is 25 MB.
#' For example, the following code can be used to set the cache size to 4 GB.
#' ```
#' terra::gdalCache(size = 4000)
#' ```
#'
#' @seealso
#' See [fast_extract()] for extracting data from spatial datasets.
#'
#' @exportMethod intersecting_units
#'
#' @aliases intersecting_units,Raster,ANY-method intersecting_units,ANY,Raster-method intersecting_units,Spatial,ANY-method intersecting_units,ANY,Spatial-method intersecting_units,sf,sf-method intersecting_units,SpatRaster,sf-method intersecting_units,SpatRaster,SpatRaster-method intersecting_units,sf,SpatRaster-method intersecting_units,data.frame,ANY-method
#'
#' @examples
#' \dontrun{
#' # create data
#' r <- terra::rast(matrix(1:9, byrow = TRUE, ncol = 3))
#' r_with_holes <- r
#' r_with_holes[c(1, 5, 9)] <- NA
#' ply <- sf::st_as_sf(terra::as.polygons(r))
#' ply_with_holes <- sf::st_as_sf(terra::as.polygons(r_with_holes))
#'
#' # intersect raster with raster
#' par(mfrow = c(1, 2))
#' plot(r, main = "x = SpatRaster", axes = FALSE)
#' plot(r_with_holes, main = "y = SpatRaster", axes = FALSE)
#' print(intersecting_units(r, r_with_holes))
#'
#' # intersect raster with sf
#' par(mfrow = c(1, 2))
#' plot(r, main = "x = SpatRaster", axes = FALSE)
#' plot(ply_with_holes, main = "y = sf", key.pos = NULL, reset = FALSE)
#' print(intersecting_units(r, ply_with_holes))
#'
#' # intersect sf with raster
#' par(mfrow = c(1, 2))
#' plot(ply, main = "x = sf", key.pos = NULL, reset = FALSE)
#' plot(r_with_holes, main = "y = SpatRaster")
#' print(intersecting_units(ply, r_with_holes))
#'
#' # intersect sf with sf
#' par(mfrow = c(1, 2))
#' plot(ply, main = "x = sf", key.pos = NULL, reset = FALSE)
#' plot(ply_with_holes, main = "y = sf", key.pos = NULL, reset = FALSE)
#' print(intersecting_units(ply, ply_with_holes))
#' }
#'
#' @export
methods::setGeneric(
  "intersecting_units",
  signature = methods::signature("x", "y"),
  function(x, y) {
    assert_required(x)
    assert_required(y)
    assert(
      is_inherits(
        x, c("data.frame", "sf", "SpatRaster", "Spatial", "Raster")
      ),
      is_spatially_explicit(y)
    )
    standardGeneric("intersecting_units")
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{Raster,ANY}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "Raster", y = "ANY"),
  function(x, y) {
    cli_warning(raster_pkg_deprecation_notice)
    intersecting_units(terra::rast(x), y)
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{ANY,Raster}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "ANY", y = "Raster"),
  function(x, y) {
    cli_warning(raster_pkg_deprecation_notice)
    intersecting_units(x, terra::rast(y))
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{Spatial,ANY}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "Spatial", y = "ANY"),
  function(x, y) {
    cli_warning(sp_pkg_deprecation_notice)
    intersecting_units(sf::st_as_sf(x), y)
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{ANY,Spatial}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "ANY", y = "Spatial"),
  function(x, y) {
    cli_warning(sp_pkg_deprecation_notice)
    intersecting_units(x, sf::st_as_sf(y))
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{SpatRaster,SpatRaster}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "SpatRaster", y = "SpatRaster"),
  function(x, y) {
    # assert arguments are valid
    assert(
      inherits(x, "SpatRaster"),
      is_numeric_values(x),
      inherits(y, "SpatRaster"),
      is_numeric_values(y),
      terra::nlyr(x) == 1,
      is_same_crs(x, y),
      is_comparable_raster(x, y)
    )
    # extract first layer
    x <- x[[1]]
    y <- y[[1]]
    # return positive cells
    as.integer(terra::cells((y > 0) & !is.na(x), 1)[[1]])
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{sf,sf}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "sf", y = "sf"),
  function(x, y) {
    # assert arguments are valid
    assert(
      inherits(x, "sf"),
      inherits(y, "sf"),
      is_same_crs(x, y),
      is_spatial_extents_overlap(x, y)
    )
    ## find out which units in x have an interior that intersects with
    ## the interior of any units in y
    int <- sf::st_relate(x, y, pattern = "T********", sparse = TRUE)
    ## return indices
    which(lengths(int) > 0)
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{SpatRaster,sf}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "SpatRaster", y = "sf"),
  function(x, y) {
    # assert arguments are valid
    assert(
      inherits(x, "SpatRaster"),
      is_numeric_values(x),
      inherits(y, "sf"),
      terra::nlyr(x) == 1,
      is_same_crs(x, y),
      is_spatial_extents_overlap(x, y)
    )
    # add column with a value
    y$value <- 1
    # find intersecting units
    intersecting_units(
      x = x,
      y = terra::rasterize(terra::vect(y), x, field = "value")
    )
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{sf,SpatRaster}(x, y)
#' @rdname intersecting_units
methods::setMethod("intersecting_units",
  methods::signature(x = "sf", y = "SpatRaster"),
  function(x, y) {
    # assert arguments are valid
    assert(
      inherits(x, "sf"),
      inherits(y, "SpatRaster"),
      is_numeric_values(y),
      terra::nlyr(y) == 1,
      is_same_crs(x, y),
      is_spatial_extents_overlap(x, y)
    )
    assert(
      all(!st_geometry_classes(x) %in% c("GEOMETRYCOLLECTION", "MULTIPOINT")),
      msg = paste(
        "{.arg x} must not contain",
        "{.cls GEOMETRYCOLLECTION} or {.cls MULTIPOINT} geometries."
      )
    )
    # find out which units in y contain at least one element of x
    ## precision is 1e-7
    which(c(fast_extract(y[[1]] > 0, x, fun = "mean")) > 1e-7)
  }
)

na_crs <- "ENGCRS[\"Undefined Cartesian SRS\",\n    EDATUM[\"\"],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"
