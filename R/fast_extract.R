#' @include internal.R
NULL

#' Fast extract
#'
#' Extract data from a [terra::rast()] object.
#'
#' @param x [terra::rast()] object.
#'
#' @param y [sf::sf()] object.
#'
#' @param fun `character` name of statistic to summarize data. Defaults
#'   to `"mean"`. Available options include `"sum"` or `"mean"`.
#'   Defaults to `"mean"`.
#'
#' @param ... not used.
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
#' @return A `matrix` containing the summary amount of each feature
#'    within each planning unit. Rows correspond to different spatial features
#'   in the argument to `y` and columns correspond to different raster
#'   layers in the argument to `x`.
#'
#' @seealso [terra::extract()], [exactextractr::exact_extract()].
#'
#' @details
#' This function is simply a wrapper that uses
#' [exactextractr::exact_extract()] for polygon geometries, and
#' [terra::extract()] for other geometry types.
#'
#' @name fast_extract
#'
#' @exportMethod fast_extract
#'
#' @examples
#' # load data
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_features <- get_sim_features()
#'
#' # extract data
#' result <- fast_extract(sim_features, sim_pu_polygons)
#'
#' # show result
#' print(head(result))
#'
#' @aliases fast_extract,Raster,Spatial-method fast_extract,Raster,sf-method fast_extract,Raster,sfc-method fast_extract,SpatRaster,Spatial-method fast_extract,SpatRaster,sf-method fast_extract,SpatRaster,sfc-method
#'
#' @export
methods::setGeneric(
  "fast_extract",
  signature = methods::signature("x", "y"),
  function(x, y, ...) {
    rlang::check_required(x)
    rlang::check_required(y)
    assert(is_spatially_explicit(x))
    if (!inherits(y, "sfc")) {
      assert(is_spatially_explicit(y))
    }
    standardGeneric("fast_extract")
  }
)

#' @name fast_extract
#' @usage \S4method{fast_extract}{Raster,Spatial}(x, y, fun = "mean", ...)
#' @rdname fast_extract
methods::setMethod(
  "fast_extract",
  signature(x = "Raster", y = "Spatial"),
  function(x, y, fun = "mean", ...) {
    .Deprecated(raster_pkg_deprecation_notice)
    .Deprecated(sp_pkg_deprecation_notice)
    fast_extract(terra::rast(x), sf::st_as_sf(y), fun, ...)
})

#' @name fast_extract
#' @usage \S4method{fast_extract}{Raster,sfc}(x, y, fun = "mean", ...)
#' @rdname fast_extract
methods::setMethod(
  "fast_extract",
  signature(x = "Raster", y = "sfc"),
  function(x, y, fun = "mean", ...) {
    .Deprecated(raster_pkg_deprecation_notice)
    fast_extract(terra::rast(x), sf::st_sf(y), fun, ...)
})

#' @name fast_extract
#' @usage \S4method{fast_extract}{SpatRaster,sfc}(x, y, fun = "mean", ...)
#' @rdname fast_extract
methods::setMethod(
  "fast_extract",
  signature(x = "SpatRaster", y = "sfc"),
  function(x, y, fun = "mean", ...) {
    fast_extract(x, sf::st_sf(y), fun, ...)
})

#' @name fast_extract
#' @usage \S4method{fast_extract}{Raster,sf}(x, y, fun = "mean", ...)
#' @rdname fast_extract
methods::setMethod(
  "fast_extract",
  signature(x = "Raster", y = "sf"),
  function(x, y, fun = "mean", ...) {
    .Deprecated(raster_pkg_deprecation_notice)
    fast_extract(terra::rast(x), y, fun, ...)
})

#' @name fast_extract
#' @usage \S4method{fast_extract}{SpatRaster,sf}(x, y, fun = "mean", ...)
#' @rdname fast_extract
methods::setMethod(
  "fast_extract",
  signature(x = "SpatRaster", y = "sf"),
  function(x, y, fun = "mean", ...) {
    # assert arguments are valid
    assert(
      inherits(x, "SpatRaster"),
      inherits(y, "sf"),
      assertthat::is.string(fun),
      is_match_of(fun, c("mean", "sum")),
      is_same_crs(x, y),
      is_spatial_extents_overlap(x, y)
    )
    assert(
      all(!st_geometry_classes(y) %in% c("GEOMETRYCOLLECTION", "MULTIPOINT")),
      msg = paste(
        "{.arg y} must not contain",
        "{.cls GEOMETRYCOLLECTION} or {.cls MULTIPOINT} geometries."
      )
    )
    # determine summary statistic
    if (identical(fun, "mean")) fun2 <- mean
    if (identical(fun, "sum")) fun2 <- sum
    # since the coordinate reference systems have been verified,
    # coerce them to NA coordinate reference systems to avoid PROJ7 warnings
    # in exactextractr::exact_extract
    sf::st_crs(y) <- sf::st_crs(NA_character_)
    terra::crs(x) <- NA_character_
    # identify geometry classes
    geomc <- st_geometry_classes(y)
    # prepare output vector
    out <- matrix(NA_real_, nrow = nrow(y), ncol = terra::nlyr(x))
    # ensure unique names in raster data
    names(x) <- make.unique(names(x))
    # process point geometries
    point_idx <- grepl("POINT", geomc, fixed = TRUE)
    if (any(point_idx)) {
      out[point_idx, ] <- as.matrix(
        terra::extract(
          x = x,
          y = terra::vect(y[point_idx, ]),
          ID = FALSE,
          fun = fun2,
          na.rm = FALSE
        )
      )
    }
    # process line geometries
    line_idx <- grepl("LINE", geomc, fixed = TRUE)
    if (any(line_idx)) {
      out[line_idx, ] <- as.matrix(
        terra::extract(
          x = x,
          y = terra::vect(y[line_idx, ]),
          ID = FALSE,
          touches = TRUE,
          fun = fun2,
          na.rm = FALSE
        )
      )
    }
    # process polygon geometries
    poly_idx <- grepl("POLYGON", geomc, fixed = TRUE)
    if (any(poly_idx)) {
      out[poly_idx, ] <- as.matrix(
        exactextractr::exact_extract(
          x,
          y[poly_idx, ],
          fun = fun,
          progress = FALSE
        )
      )
    }
    # round really small values to zero
    out[abs(out) < 1e-10] <- 0
    # return result
    out
  }
)
