#' @include internal.R
NULL

#' Find intersecting units
#'
#' Determine the units in any given spatial data object that intersect
#' with any of the units in another spatial data object.
#'
#' @param x Object.
#'
#' @param y Object.
#'
#' @return the units in \code{x} that intersect with \code{y}.
#'
#' @name intersecting_units
#'
#' @exportMethod intersecting_units
#'
#' @export
methods::setGeneric('intersecting_units', 
                    signature=methods::signature('x', 'y'),
                    function(x, y) 
                      standardGeneric('intersecting_units'))

#' @name intersecting_units
#' @rdname intersecting_units
methods::setMethod('intersecting_units',
  methods::signature(x='Raster', y='Raster'),
  function(x, y) {
    # assert arguments are valid
    assertthat::assert_that(inherits(x, 'Raster'), inherits(y, 'Raster'),
      isTRUE(raster::nlayers(x)==1), raster::compareCRS(x@crs, y@crs),
      raster::compareRaster(x, y, crs=TRUE, res=TRUE, tolerance=1e-5,
        stopiffalse=FALSE))
    if (inherits(x, c('RasterStack', 'RasterBrick'))) x <- x[[1]]
    if (inherits(y, c('RasterStack', 'RasterBrick'))) y <- y[[1]]
    y <- as.logical(y)
    # find positive cells
    cells <- raster::Which(y & !is.na(x), cells=TRUE)
    # return result
    return(cells)
  }
)

#' @name intersecting_units
#' @rdname intersecting_units
methods::setMethod('intersecting_units',
  methods::signature(x='Spatial', y='Spatial'),
  function(x, y) {
    # assert arguments are valid
    assertthat::assert_that(
      inherits(x, 'Spatial'), inherits(x, 'Spatial'),
      raster::compareCRS(x@proj4string, y@proj4string),
      isTRUE(rgeos::gIntersects(as(raster::extent(x), 'SpatialPolygons'),
        as(raster::extent(y), 'SpatialPolygons'))))
    # find out which units in x intersect with any units in y
    intersects <- rgeos::gContains(x, y, byid=TRUE, 
      returnDense=FALSE)
    # convert dense list to sparse matrix
    names(intersects) <- as.character(seq_len(length(x)))
    intersects <- rcpp_list_to_matrix_indices(intersects)
    # return indices for x that are in sparse matrix
    return(unique(intersects$j))
  }
)

#' @name @solve
#' @rdname solve
methods::setMethod('intersecting_units',
  methods::signature(x='Raster', y='Spatial'),
  function(x, y) {
    # assert arguments are valid
    assertthat::assert_that(
      inherits(x, 'Raster'), inherits(y, 'Spatial'), 
      isTRUE(raster::nlayers(x)==1), raster::compareCRS(x@crs, y@proj4string),
      isTRUE(rgeos::gIntersects(as(raster::extent(x), 'SpatialPolygons'),
        as(raster::extent(y), 'SpatialPolygons'))))
    # find cells in x that intersect with y 
    cells <- fast_extract(x, y, fun=NULL, cellnumbers=TRUE, sp=FALSE, df=TRUE)
    cells <- unique(cells[[2]])
    # return cells
    return(cells)
  }
)

#' @name intersecting_units
#' @rdname intersecting_units
methods::setMethod('intersecting_units',
  methods::signature(x='Spatial', y='Raster'),
  function(x, y) {
    # assert arguments are valid
    assertthat::assert_that(
      inherits(x, 'Spatial'), inherits(y, 'Raster'), 
      isTRUE(raster::nlayers(y)==1), raster::compareCRS(x@proj4string, y@crs),
      isTRUE(rgeos::gIntersects(as(raster::extent(x), 'SpatialPolygons'),
        as(raster::extent(y), 'SpatialPolygons'))))
    if (inherits(y, c('RasterStack', 'RasterBrick'))) y <- y[[1]]
    y <- as.logical(y)
    # find maximum pixel value in each unit in x
    ext <- fast_extract(y, x, fun=max, na.rm=TRUE, df=TRUE)
    units <- ext[[1]][which(ext[[2]]==1)]
    # return result
    return(units)
  }
)
