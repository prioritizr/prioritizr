#' @include internal.R
NULL

#' Find intersecting units
#'
#' Find which of the units in a spatial data object intersect
#' with the units in another spatial data object.
#'
#' @param x \code{\link{Spatial-class}} or \code{\link{Raster-class}} object.
#'
#' @param y \code{\link{Spatial-class}} or \code{\link{Raster-class}} object.
#'
#' @return \code{integer} indices of the units in \code{x} that intersect with
#'   \code{y}.
#'
#' @details The \code{\link{set_number_of_threads}} can be used to distribute
#'   computations among multiple threads and potentially reduce run time.
#'
#' @name intersecting_units
#'
#' @seealso \code{\link{fast_extract}}, \code{\link{set_number_of_threads}},
#'   \code{\link{get_number_of_threads}}.
#'
#' @exportMethod intersecting_units
#'
#' @aliases intersecting_units,Raster,Raster-method intersecting_units,Raster,Spatial-method intersecting_units,Spatial,Raster-method intersecting_units,Spatial,Spatial-method intersecting_units,data.frame,ANY-method
#'
#' @examples
#' # create data
#' r <- raster(matrix(1:9, byrow = TRUE, ncol=3))
#' r_with_holes <- r
#' r_with_holes[c(1, 5, 9)] <- NA
#' ply <- rasterToPolygons(r)
#' ply_with_holes <- rasterToPolygons(r_with_holes)
#'
#' # intersect raster with raster
#' \donttest{
#' par(mfrow = c(1, 2))
#' plot(r, main = "x=Raster")
#' plot(r_with_holes, main = "y=Raster")
#' }
#' print(intersecting_units(r, r_with_holes))
#'
#' # intersect raster with polygons
#' \donttest{
#' par(mfrow = c(1, 2))
#' plot(r, main = "x=Raster")
#' plot(ply_with_holes, main = "y=Spatial")
#' }
#' print(intersecting_units(r, ply_with_holes))
#'
#' # intersect polygons with raster
#' \donttest{
#' par(mfrow = c(1, 2))
#' plot(ply, main = "x=Spatial")
#' plot(r_with_holes, main = "y=Raster")
#' }
#' print(intersecting_units(ply, r_with_holes))
#'
#' # intersect polygons with polygons
#' \donttest{
#' par(mfrow = c(1, 2))
#' plot(ply, main = "x=Spatial")
#' plot(ply_with_holes, main = "y=Spatial")
#' }
#' print(intersecting_units(ply, ply_with_holes))
#'
#' @export
methods::setGeneric("intersecting_units",
                    signature = methods::signature("x", "y"),
                    function(x, y)
                      standardGeneric("intersecting_units"))

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{Raster,Raster}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "Raster", y = "Raster"),
  function(x, y) {
    # assert arguments are valid
    assertthat::assert_that(inherits(x, "Raster"), inherits(y, "Raster"),
      isTRUE(raster::nlayers(x) == 1), raster::compareCRS(x@crs, y@crs),
      is_comparable_raster(x, y))
    if (inherits(x, c("RasterStack", "RasterBrick"))) x <- x[[1]]
    if (inherits(y, c("RasterStack", "RasterBrick"))) y <- y[[1]]
    y <- as.logical(y)
    # find positive cells
    cells <- raster::Which(y & !is.na(x), cells = TRUE)
    # return result
    return(cells)
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{Spatial,Spatial}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "Spatial", y = "Spatial"),
  function(x, y) {
    # assert arguments are valid
    assertthat::assert_that(
      inherits(x, "Spatial"), inherits(x, "Spatial"),
      raster::compareCRS(x@proj4string, y@proj4string),
      isTRUE(rgeos::gIntersects(methods::as(raster::extent(x),
                                            "SpatialPolygons"),
                                methods::as(raster::extent(y),
                                            "SpatialPolygons"))))
    # find out which units in x intersect with any units in y
    intersects <- rgeos::gContains(x, y, byid = TRUE, returnDense = FALSE)
    # convert dense list to sparse matrix
    names(intersects) <- as.character(seq_len(length(x)))
    intersects <- rcpp_list_to_matrix_indices(intersects)
    # return indices for x that are in sparse matrix
    return(unique(intersects$j))
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{Raster,Spatial}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "Raster", y = "Spatial"),
  function(x, y) {
    # assert arguments are valid
    assertthat::assert_that(
      inherits(x, "Raster"), inherits(y, "Spatial"),
      isTRUE(raster::nlayers(x) == 1), raster::compareCRS(x@crs, y@proj4string),
      isTRUE(rgeos::gIntersects(methods::as(raster::extent(x),
                                            "SpatialPolygons"),
                                methods::as(raster::extent(y),
                                            "SpatialPolygons"))))
    # find cells in x that intersect with y
    cells <- fast_extract(x, y, fun = NULL, cellnumbers = TRUE, sp = FALSE,
                          df = TRUE)
    cells <- unique(cells[[2]])
    # return cells
    return(cells)
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{Spatial,Raster}(x, y)
#' @rdname intersecting_units
methods::setMethod("intersecting_units",
  methods::signature(x = "Spatial", y = "Raster"),
  function(x, y) {
    # assert arguments are valid
    assertthat::assert_that(
      inherits(x, "Spatial"), inherits(y, "Raster"),
      isTRUE(raster::nlayers(y) == 1), raster::compareCRS(x@proj4string, y@crs),
      isTRUE(rgeos::gIntersects(methods::as(raster::extent(x),
                                            "SpatialPolygons"),
                                methods::as(raster::extent(y),
                                            "SpatialPolygons"))))
    if (inherits(y, c("RasterStack", "RasterBrick"))) y <- y[[1]]
    y <- as.logical(y)
    # find maximum pixel value in each unit in x
    ext <- fast_extract(y, x, fun = max, na.rm = TRUE, df = TRUE)
    units <- ext[[1]][which(ext[[2]] == 1)]
    # return result
    return(units)
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{data.frame,ANY}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "data.frame", y = "ANY"),
  function(x, y) {
    stop("planning units are stored as a data.frame and so the required ",
         "spatial analysis cannot be performed.")
  }
)
