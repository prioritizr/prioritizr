#' @include internal.R generics.R parallel.R
NULL

#' @export
methods::setMethod('fast_extract', signature(x='Raster', y='SpatialPolygons'), 
  function(x, y, fun=mean, velox=requireNamespace('velox'), ...) {
    # assert arguments are valid
    assertthat::assert_that(inherits(x, 'Raster'), inherits(y, 'SpatialPolygons'),
      isTRUE(is.null(fun) || inherits(fun, 'function')),
      assertthat::is.flag(velox), raster::compareCRS(x@crs, y@proj4string),
      rgeos::gIntersects(as(raster::extent(x[[1]]), 'SpatialPolygons'),
      as(raster::extent(y), 'SpatialPolygons')))
    if (velox & !requireNamespace('velox'))
      stop('the velox R package needs to be installed to use velox')
    # data processing
    args <- list(...)
    if (velox & !(isTRUE(args$cellnumbers) || isTRUE(args$sp))) {
      if (inherits(x, 'RasterBrick'))
        x <- raster::stack(x)
      if (is.parallel()) {
        # use the velox with parallel processing
        parallel::clusterExport(.pkgenv$cluster, c('x', 'y', 'fun', 'args',
          'velox_extract'), envir=environment())
        m <- plyr::llply(distribute_load(length(y)), .parallel=TRUE,
          function(i) {
              return(do.call(velox_extract, append(
                list(x=x, y=y[i,], fun=fun), args)))
          })
        parallel::clusterEvalQ(.pkgenv$cluster, {rm('x', 'y', 'fun', 'args')})
        m <- do.call(rbind, m)
      } else {
        # use the velox without parallel processing
        m <- velox_extract(x=x, y=y, fun=fun, ...)
      }
    } else {
      if (is.parallel()) {
        m <- parallelized_extract(x=x, y=y, fun=fun, ...)
      } else {
        m <- raster::extract(x=x, y=y, fun=fun, ...)
      }
    }
    # return result
    return(m)
  }
)

#' @export
methods::setMethod('fast_extract', signature(x='Raster', y='SpatialLines'), 
  function(x, y, fun=mean, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, 'Raster'), inherits(y, 'SpatialLines'),
        inherits(fun, 'function'),raster::compareCRS(x@crs, y@proj4string),
        rgeos::gIntersects(as(raster::extent(x[[1]]), 'SpatialPolygons'),
                         as(raster::extent(y), 'SpatialPolygons')))
    # data processing
    if (is.parallel()) {
      m <- parallelized_extract(x=x, y=y, fun=fun, ...)
    } else {
      m <- raster::extract(x=x, y=y, fun=fun, ...)
    }
    # return result
    return(m)
  }
)

#' @export
methods::setMethod('fast_extract', signature(x='Raster', y='SpatialPoints'), 
  function(x, y, fun=mean, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, 'Raster'), inherits(y, 'SpatialPoints'),
        inherits(fun, 'function'),raster::compareCRS(x@crs, y@proj4string),
        rgeos::gIntersects(as(raster::extent(x[[1]]), 'SpatialPolygons'),
                         as(raster::extent(y), 'SpatialPolygons')))
    # return result
    return(raster::extract(x, y, fun=fun, ...))
  }
)
