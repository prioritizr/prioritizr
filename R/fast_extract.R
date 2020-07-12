#' @include internal.R
NULL

#' Fast extract
#'
#' Extract data from a [`Raster-class`] object.
#'
#' @param x [`Raster-class`] object.
#'
#' @param y [`Spatial-class`] or
#'          [sf::sf()] object.
#'
#' @param fun `character` name of statistic to summarize data. Defaults
#'   to `"mean"`. Available options include `"sum"` or `"mean"`.
#'   Defaults to `"mean"`.
#'
#' @param ... not used.
#'
#' @return `matrix` containing the summary amount of each feature
#'    within each planning unit. Rows correspond to different spatial features
#'   in the argument to `y` and columns correspond to different raster
#'   layers in the argument to `x`.
#'
#' @seealso [raster::extract()],
#'   [exactextractr::exact_extract()].
#'
#' @details This function is simply a wrapper that uses
#'   [raster::extract()] to extract data for
#'   [`SpatialPoints-class`] and
#'   [`SpatialLines-class`] and
#'   non-polygonal [sf::sf()] data, and
#'   [exactextractr::exact_extract()] for
#'   [`SpatialPolygons-class`] and
#'   polygonal [sf::sf()] data.
#'
#' @name fast_extract
#'
#' @exportMethod fast_extract
#'
#' @examples
#' # load data
#' data(sim_pu_sf, sim_features)
#'
#' # extract data
#' result <- fast_extract(sim_features, sim_pu_sf)
#'
#' # show result
#' print(head(result))
#'
#' @aliases fast_extract,Raster,SpatialLines-method fast_extract,Raster,SpatialPoints-method fast_extract,Raster,SpatialPolygons-method fast_extract,Raster,sf-method fast_extract,Raster,sfc-method
#'
#' @export
methods::setGeneric("fast_extract",
                    signature = methods::signature("x", "y"),
                    function(x, y, ...) standardGeneric("fast_extract"))

#' @name fast_extract
#' @usage \S4method{fast_extract}{Raster,SpatialPolygons}(x, y, fun = "mean", ...)
#' @rdname fast_extract
methods::setMethod(
  "fast_extract",
  signature(x = "Raster", y = "SpatialPolygons"),
  function(x, y, fun = "mean", ...) {
    fast_extract(x, sf::st_as_sf(y), fun, ...)
})

#' @name fast_extract
#' @usage \S4method{fast_extract}{Raster,SpatialPoints}(x, y, fun = "mean", ...)
#' @rdname fast_extract
methods::setMethod(
  "fast_extract",
  signature(x = "Raster", y = "SpatialPoints"),
  function(x, y, fun = "mean", ...) {
    fast_extract(x, sf::st_as_sf(y), fun, ...)
})

#' @name fast_extract
#' @usage \S4method{fast_extract}{Raster,SpatialLines}(x, y, fun = "mean", ...)
#' @rdname fast_extract
methods::setMethod(
  "fast_extract",
  signature(x = "Raster", y = "SpatialLines"),
  function(x, y, fun = "mean", ...) {
    fast_extract(x, sf::st_as_sf(y), fun, ...)
})

#' @name fast_extract
#' @usage \S4method{fast_extract}{Raster,sfc}(x, y, fun = "mean", ...)
#' @rdname fast_extract
methods::setMethod(
  "fast_extract",
  signature(x = "Raster", y = "sfc"),
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
    # assert arguments are valid
    assertthat::assert_that(
      inherits(x, "Raster"),
      inherits(y, "sf"),
      assertthat::is.string(fun),
      sf::st_crs(x@crs) == sf::st_crs(y),
      intersecting_extents(x, y))
    assertthat::assert_that(all(!geometry_classes(y) %in%
                                  c("GEOMETRYCOLLECTION", "MULTIPOINT")))
    assertthat::assert_that(fun %in% c("mean", "sum"))
    # determine summary statistic
    if (identical(fun, "mean")) fun2 <- mean
    if (identical(fun, "sum")) fun2 <- sum
    # since the coordinate reference systems have been verified,
    # coerce them to NA coordinate reference systems to avoid PROJ7 warnings
    # in exactextractr::exact_extract
    sf::st_crs(y) <- sf::st_crs(NA_character_)
    x@crs <- sp::CRS(NA_character_)
    # identify geometry classes
    geomc <- geometry_classes(y)
    # prepare output vector
    out <- matrix(NA_real_, nrow = nrow(y), ncol = raster::nlayers(x))
    # process point geometries
    point_idx <- grepl("POINT", geomc, fixed = TRUE)
    if (any(point_idx)) {
        out[point_idx, ] <- as.matrix(raster::extract(
          x = x, y = y[point_idx, ], fun = fun2, df = TRUE,
          na.rm = FALSE)[, -1, drop = FALSE])
    }
    # process line geometries
    line_idx <- grepl("LINE", geomc, fixed = TRUE)
    if (any(line_idx)) {
        out[line_idx, ] <- as.matrix(raster::extract(
          x = x, y = y[line_idx, ], fun = fun2, df = TRUE,
          na.rm = FALSE)[, -1, drop = FALSE])
    }
    # process polygon geometries
    poly_idx <- grepl("POLYGON", geomc, fixed = TRUE)
    if (any(poly_idx)) {
      if (raster::canProcessInMemory(x, n = 1, verbose = FALSE)) {
        out[poly_idx, ] <-
          rcpp_summarize_exactextractr(exactextractr::exact_extract(
              x, y[poly_idx, ], fun = NULL, progress = FALSE),
              nrow = sum(poly_idx), ncol = raster::nlayers(x), fun = fun)
      } else {
        out[poly_idx, ] <-
          as.matrix(exactextractr::exact_extract(x, y[poly_idx, ], fun = fun,
                                                 progress = TRUE))
      }
    }
    # round really small values to zero
    out[abs(out) < 1e-10] <- 0
    # return result
    out
  }
)
