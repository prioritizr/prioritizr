#' @include internal.R generics.R parallel.R
NULL

#' @export
methods::setMethod(
  'rij_matrix',
  signature(x='Raster', y='Raster'),
  function(x, y, ...) {
    assertthat::assert_that(inherits(x, 'Raster'),
                            inherits(y, 'Raster'))
    assertthat::assert_that(
      isTRUE(raster::nlayers(x)==1),
      raster::compareRaster(x, y[[1]], res=TRUE,
                            tolerance=1e-5, stopiffalse=FALSE))
    
    included <- raster::Which(!is.na(x))
    if (raster::canProcessInMemory(x, n=raster::nlayers(y) + 2)) {
      m <- y[included]
      if (!is.matrix(m))
        m <- matrix(m, ncol=1)
      m[is.na(m)] <- 0
      m <- as(m, 'dgTMatrix')
    } else {
        m <- plyr::llply(seq_len(raster::nlayers(y)), .parallel=FALSE,
                         function(i) {
                          m <- m <- matrix(y[included], ncol=1)
                          m[is.na(m)] <- 0
                          m <- as(m, 'dgTMatrix')
                         })
      m <- do.call(rbind, m)
    }
    m
})

#' @export
methods::setMethod(
  'rij_matrix',
  signature(x='SpatialPolygons', y='Raster'),
  function(x, y, fun=sum, na.rm=TRUE, velox=('velox' %in% rownames(installed.packages())), ...) {
    assertthat::assert_that(inherits(x, 'SpatialPolygons'),
                            inherits(y, 'Raster'),
                            inherits(fun, 'function'),
                            assertthat::is.flag(na.rm),
                            assertthat::is.flag(velox))
    assertthat::assert_that(
        raster::compareCRS(x@proj4string, y@crs),
        rgeos::gIntersects(as(raster::extent(y[[1]]), 'SpatialPolygons'),
                         as(raster::extent(x), 'SpatialPolygons')))
    if (velox & (!'velox' %in% rownames(installed.packages())))
      stop('the velox R package is not installed.')
    
    if (velox) { 
      if (is.parallel()) {
        parallel::clusterExport(.pkgenv$cluster, c('x', 'y', 'fun'), envir=environment())
        m <- plyr::llply(distribute_load(length(x)),
                          .parallel=is.parallel(),
                          function(i) {
                            vx <- velox::velox(y)
                            vx$extract(x[i,], fun=fun)
                          })
        parallel::clusterEvalQ(.pkgenv$cluster, {rm('x', 'y', 'fun')})
        m <- do.call(rbind, m)
      } else {
        vx <- velox::velox(y)
        m <- vx$extract(x, fun=fun)
      }
    } else {
      if (is.parallel()) {
        parallel::clusterExport(.pkgenv$cluster, c('x', 'y', 'fun', 'na.rm'), envir=environment())
        m <- plyr::llply(distribute_load(length(x)),
                          .parallel=is.parallel(),
                          function(i) {
                            m <- raster::extract(y, x[i,], fun=fun, na.rm=na.rm, sp=FALSE)
                          })
        parallel::clusterEvalQ(.pkgenv$cluster, {rm('x', 'y', 'fun', 'na.rm')})
        m <- do.call(rbind, m)
      } else {
        m <- raster::extract(y, x, fun=fun, na.rm=na.rm, sp=FALSE)
      }
    }
    m[is.na(m[])] <- 0
    m <- as(m, 'dgTMatrix')
    m
})

#' @export
methods::setMethod(
  'rij_matrix',
  signature(x='SpatialLines', y='Raster'),
  function(x, y, fun=sum, na.rm=TRUE, ...) {
    assertthat::assert_that(inherits(x, 'SpatialLines'),
                            inherits(y, 'Raster'))
    assertthat::assert_that(
        inherits(fun, 'function'),
        raster::compareCRS(x@proj4string, y@crs),
        rgeos::gIntersects(as(raster::extent(y[[1]]), 'SpatialPolygons'),
                         as(raster::extent(x), 'SpatialPolygons')))
    
    if (is.parallel()) {
      parallel::clusterExport(.pkgenv$cluster, c('x', 'y', 'fun', 'na.rm'), envir=environment())
      m <- plyr::llply(distribute_load(length(x)),
                        .parallel=is.parallel(),
                        function(i) {
                          m <- raster::extract(y, x[i,], fun=fun,
                                               na.rm=na.rm, sp=FALSE)
                        })
      parallel::clusterEvalQ(.pkgenv$cluster, {rm('x', 'y', 'fun', 'na.rm')})
      m <- do.call(rbind, m)
    } else {
      m <- raster::extract(y, x, fun=fun, na.rm=na.rm, sp=FALSE)
    }
    m[is.na(m[])] <- 0
    m <- as(m, 'dgTMatrix')
    m
})

#' @export
methods::setMethod(
  'rij_matrix',
  signature(x='SpatialPoints', y='Raster'),
  function(x, y, ...) {
    assertthat::assert_that(inherits(x, 'SpatialPoints'),
                            inherits(y, 'Raster'))
    assertthat::assert_that(
        raster::compareCRS(x@proj4string, y@crs),
        rgeos::gIntersects(as(raster::extent(y[[1]]), 'SpatialPolygons'),
                         as(raster::extent(x), 'SpatialPolygons')))
    
    m <- raster::extract(y, x, na.rm=TRUE, sp=FALSE)
    m[is.na(m[])] <- 0
    m <- as(m, 'dgTMatrix')
    m
})
