#' @include internal.R generics.R fast_extract.R
NULL

#' @export
methods::setMethod('rij_matrix', signature(x='Raster', y='Raster'),
  function(x, y, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, 'Raster'), inherits(y, 'Raster'),
      isTRUE(raster::nlayers(x)==1), 
      raster::compareRaster(x, y[[1]], res=TRUE, tolerance=1e-5,
      stopiffalse=FALSE))
    # data processing
    included <- raster::Which(!is.na(x))
    if (raster::canProcessInMemory(x, n=raster::nlayers(y) + 2)) {
      # if the all the features can be fit into memory then processes
      # them all in memory
      m <- y[included]
      if (!is.matrix(m))
        m <- matrix(m, ncol=1)
      m[is.na(m)] <- 0
      m <- as(m, 'dgCMatrix')
    } else {
      # othewise, process each feature seperately
        m <- plyr::llply(seq_len(raster::nlayers(y)), .parallel=FALSE,
          function(i) {
            m <- matrix(y[included], ncol=1)
            m[is.na(m)] <- 0
            m <- as(m, 'dgCMatrix')
          })
      m <- do.call(rbind, m)
    }
    # return result
    return(Matrix::t(m))
})

#' @export
methods::setMethod('rij_matrix', signature(x='Spatial', y='Raster'),
  function(x, y, fun=sum, velox=requireNamespace('velox'), ...) {
    m <- fast_extract(x=y, y=x, fun=fun, velox=velox, df=FALSE, sp=FALSE)
    if (raster::nlayers(y)==1)
      m <- matrix(m, ncol=1)
    m[is.na(m[])] <- 0
    m <- as(m, 'dgCMatrix')
    return(Matrix::t(m))
})

