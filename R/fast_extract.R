#' @include internal.R parallel.R
NULL

#' Fast extract
#'
#' Extract data from a \code{\link[raster]{Raster-class}} object from a
#' \code{\link[sp]{Spatial-class}} object using performance enhancing tricks.
#'
#' @param x \code{\link[raster]{Raster-class}} object.
#'
#' @param y \code{\link[sp]{Spatial-class}} object.
#'
#' @param fun \code{function} used to summarize values. Defaults to
#'   \code{\link{sum}}. Note that this only used when \code{x} is a
#'   \code{\link[sp]{SpatialPolygons-class}} or a
#'   \code{\link[sp]{SpatialLines-class}} object. This function must
#'   have an \code{na.rm} argument.
#'
#' @param velox \code{logical} should the \code{\link[velox]{velox}}
#'   be used for geoprocessing? Defaults to \code{TRUE} if the package
#'   is installed. Note that this only used when \code{x} is a
#'   \code{\link[sp]{SpatialPolygons-class}} object.
#'
#' @param ... additional arguments passed to \code{\link[raster]{extract}}.
#'
#' @return \code{data.frame}, \code{matrix}, or \code{list} object
#'   depending on the arguments.
#'
#' @seealso \code{\link[raster]{extract}},
#'   \code{\link[velox]{VeloxRaster_extract}}.
#'
#' @details Spatial analyses will be conducted using the
#'   \code{\link[velox]{velox}} package if it is installed. Additionally,
#'   multiple threads can be used to speed up computation using the
#'   \code{\link{set_number_of_threads}} function.
#'
#' @name fast_extract
#'
#' @exportMethod fast_extract
#'
#' @examples
#' # load data
#' data(sim_pu_polygons, sim_features)
#' \donttest{
#' # we will investigate several ways for extracting values from a raster
#' # using polygons. Specifically, for each band in the raster,
#' # for each polygon in the vector layer, calculate the average
#' # of the cells that are inside the polygon.
#'
#' # perform the extraction using the standard raster::extract function
#' system.time({result <- fast_extract(sim_features, sim_pu_polygons)})
#'
#' # perform extract using the fast_extract function augmented using the
#' # "velox" package
#' system.time({result <- fast_extract(sim_features, sim_pu_polygons,
#'                                     velox = TRUE)})
#'
#' # perform extract using the fast_extract function with "velox" package
#' # and using two threads for processing. Note that this might be slower
#' # due to overheads but should yield faster processing times on larger
#' # spatial data sets
#' set_number_of_threads(2)
#' system.time({result <- fast_extract(sim_features, sim_pu_polygons,
#'                                     velox = TRUE)})
#' set_number_of_threads(1)
#' }
#'
#' @aliases fast_extract,Raster,SpatialLines-method fast_extract,Raster,SpatialPoints-method fast_extract,Raster,SpatialPolygons-method
#'
#' @export
methods::setGeneric("fast_extract",
                    signature = methods::signature("x", "y"),
                    function(x, y, ...) standardGeneric("fast_extract"))

#' @name fast_extract
#' @usage \S4method{fast_extract}{Raster,SpatialPolygons}(x, y, fun = mean, velox = requireNamespace("velox", quietly = TRUE), ...)
#' @rdname fast_extract
methods::setMethod(
    "fast_extract",
    signature(x = "Raster", y = "SpatialPolygons"),
    function(x, y, fun = mean,
             velox = requireNamespace("velox", quietly = TRUE), ...) {
    # assert arguments are valid
    assertthat::assert_that(inherits(x, "Raster"),
      inherits(y, "SpatialPolygons"),
      isTRUE(is.null(fun) || inherits(fun, "function")),
      assertthat::is.flag(velox), raster::compareCRS(x@crs, y@proj4string),
      rgeos::gIntersects(methods::as(raster::extent(x[[1]]), "SpatialPolygons"),
        methods::as(raster::extent(y), "SpatialPolygons")))
    if (velox & !requireNamespace("velox", quietly = TRUE))
      stop("the velox R package needs to be installed to use velox")
    # data processing
    args <- list(...)
    if (velox & !(isTRUE(args$cellnumbers) || isTRUE(args$sp))) {
      if (inherits(x, "RasterBrick"))
        x <- raster::stack(x)
      if (is.parallel()) {
        # use the velox with parallel processing
        parallel::clusterExport(.pkgenv$cluster, c("x", "y", "fun", "args",
          "velox_extract"), envir = environment())
        m <- plyr::llply(distribute_load(length(y)), .parallel = TRUE,
          function(i) {
              return(do.call(velox_extract, append(
                list(x = x, y = y[i, ], fun = fun), args)))
          })
        parallel::clusterEvalQ(.pkgenv$cluster, {
            rm("x", "y", "fun", "args")
        })
        m <- do.call(rbind, m)
      } else {
        # use the velox without parallel processing
        m <- velox_extract(x = x, y = y, fun = fun, ...)
      }
    } else {
      if (is.parallel()) {
        m <- parallelized_extract(x = x, y = y, fun = fun, ...)
      } else {
        m <- raster::extract(x = x, y = y, fun = fun, ...)
      }
    }
    # return result
    return(m)
  }
)

#' @name fast_extract
#' @usage \S4method{fast_extract}{Raster,SpatialLines}(x, y, fun = mean, ...)
#' @rdname fast_extract
methods::setMethod(
  "fast_extract",
  signature(x = "Raster", y = "SpatialLines"),
  function(x, y, fun = mean, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "Raster"), inherits(y, "SpatialLines"),
        inherits(fun, "function"), raster::compareCRS(x@crs, y@proj4string),
        rgeos::gIntersects(methods::as(raster::extent(x[[1]]),
                                       "SpatialPolygons"),
                           methods::as(raster::extent(y), "SpatialPolygons")))
    # data processing
    if (is.parallel()) {
      m <- parallelized_extract(x = x, y = y, fun = fun, ...)
    } else {
      m <- raster::extract(x = x, y = y, fun = fun, ...)
    }
    # return result
    return(m)
  }
)

#' @name fast_extract
#' @usage \S4method{fast_extract}{Raster,SpatialPoints}(x, y, fun = mean, ...)
#' @rdname fast_extract
methods::setMethod(
  "fast_extract",
  signature(x = "Raster", y = "SpatialPoints"),
  function(x, y, fun = mean, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "Raster"), inherits(y, "SpatialPoints"),
        inherits(fun, "function"), raster::compareCRS(x@crs, y@proj4string),
        rgeos::gIntersects(methods::as(raster::extent(x[[1]]),
                                       "SpatialPolygons"),
                           methods::as(raster::extent(y), "SpatialPolygons")))
    # return result
    return(raster::extract(x, y, fun = fun, ...))
  }
)
