#' @include internal.R
NULL

#' Find intersecting units
#'
#' Find which of the units in a spatial data object intersect
#' with the units in another spatial data object.
#'
#' @param x [`Spatial-class`] or [`Raster-class`] object.
#'
#' @param y [`Spatial-class`] or [`Raster-class`] object.
#'
#' @return `integer` indices of the units in `x` that intersect with
#'   `y`.
#'
#' @name intersecting_units
#'
#' @seealso [fast_extract()].
#'
#' @exportMethod intersecting_units
#'
#' @aliases intersecting_units,Raster,Raster-method intersecting_units,Raster,Spatial-method intersecting_units,Spatial,Raster-method intersecting_units,Spatial,Spatial-method intersecting_units,sf,Spatial-method intersecting_units,Spatial,sf-method intersecting_units,sf,sf-method intersecting_units,Raster,sf-method intersecting_units,sf,Raster-method intersecting_units,data.frame,ANY-method
#'
#' @examples
#' # create data
#' r <- raster(matrix(1:9, byrow = TRUE, ncol=3))
#' r_with_holes <- r
#' r_with_holes[c(1, 5, 9)] <- NA
#' ply <- rasterToPolygons(r)
#' ply_with_holes <- st_as_sf(rasterToPolygons(r_with_holes))
#'
#' # intersect raster with raster
#' \dontrun{
#' par(mfrow = c(1, 2))
#' plot(r, main = "x=Raster")
#' plot(r_with_holes, main = "y=Raster")
#' }
#' print(intersecting_units(r, r_with_holes))
#'
#' # intersect raster with polygons (sf)
#' \dontrun{
#' par(mfrow = c(1, 2))
#' plot(r, main = "x=Raster")
#' plot(ply_with_holes, main = "y=sf", key.pos = NULL, reset = FALSE)
#' }
#' print(intersecting_units(r, ply_with_holes))
#'
#' # intersect polygons (Spatial) with raster
#' \dontrun{
#' par(mfrow = c(1, 2))
#' plot(ply, main = "x=Spatial")
#' plot(r_with_holes, main = "y=Raster")
#' }
#' print(intersecting_units(ply, r_with_holes))
#'
#' # intersect polygons (Spatial) with polygons (sf)
#' \dontrun{
#' par(mfrow = c(1, 2))
#' plot(ply, main = "x=Spatial")
#' plot(ply_with_holes, main = "y=sf", key.pos = NULL, reset = FALSE)
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
      isTRUE(raster::nlayers(x) == 1),
      sf::st_crs(x@crs) == sf::st_crs(y@crs),
      is_comparable_raster(x, y))
    if (inherits(x, c("RasterStack", "RasterBrick"))) x <- x[[1]]
    if (inherits(y, c("RasterStack", "RasterBrick"))) y <- y[[1]]
    y <- as.logical(y)
    # return positive cells
    raster::Which(y & !is.na(x), cells = TRUE)
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{Spatial,Spatial}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "Spatial", y = "Spatial"),
  function(x, y) {
    intersecting_units(sf::st_as_sf(x), sf::st_as_sf(y))
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{sf,Spatial}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "sf", y = "Spatial"),
  function(x, y) {
    intersecting_units(x, sf::st_as_sf(y))
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{Spatial,Raster}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "Spatial", y = "Raster"),
  function(x, y) {
    intersecting_units(sf::st_as_sf(x), y)
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{Spatial,sf}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "Spatial", y = "sf"),
  function(x, y) {
    intersecting_units(sf::st_as_sf(x), y)
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{Raster,Spatial}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "Raster", y = "Spatial"),
  function(x, y) {
    intersecting_units(x, sf::st_as_sf(y))
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
    assertthat::assert_that(
      inherits(x, "sf"), inherits(y, "sf"),
      sf::st_crs(x) == sf::st_crs(y),
      intersecting_extents(x, y))
    # find out which units in x intersect with any units in y
    int1 <- sf::st_intersects(x, y, sparse = TRUE)
    int2 <- sf::st_touches(x, y, sparse = TRUE)
    # convert dense list to sparse matrix
    names(int1) <- as.character(seq_len(nrow(x)))
    names(int2) <- as.character(seq_len(nrow(x)))
    int1 <- rcpp_list_to_matrix_indices(int1)
    int2 <- rcpp_list_to_matrix_indices(int2)
    int1 <- Matrix::sparseMatrix(i = int1$i, j = int1$j, x = 1,
                                 dims = c(nrow(y), nrow(x)))
    int2 <- Matrix::sparseMatrix(i = int2$i, j = int2$j, x = 1,
                                 dims = c(nrow(y), nrow(x)))
    # exclude units from being intersecting if they only touch
    int1[int2 > 0.5] <- 0
    int1 <- as_Matrix(Matrix::drop0(int1), "dgTMatrix")
    int1@j + 1
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{Raster,sf}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "Raster", y = "sf"),
  function(x, y) {
    # assert arguments are valid
    assertthat::assert_that(
      inherits(x, "Raster"), inherits(y, "sf"),
      isTRUE(raster::nlayers(x) == 1),
      sf::st_crs(x@crs) == sf::st_crs(y),
      intersecting_extents(x, y))
    intersecting_units(x = x, y = fasterize::fasterize(y, x, field = NULL))
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{sf,Raster}(x, y)
#' @rdname intersecting_units
methods::setMethod("intersecting_units",
  methods::signature(x = "sf", y = "Raster"),
  function(x, y) {
    # assert arguments are valid
    assertthat::assert_that(
      inherits(x, "sf"), inherits(y, "Raster"),
      isTRUE(raster::nlayers(y) == 1),
      sf::st_crs(x) == sf::st_crs(y@crs),
      intersecting_extents(x, y))
    # prepare raster data
    if (inherits(y, c("RasterStack", "RasterBrick")))
      y <- y[[1]]
    y <- as.logical(y)
    # find out which units in y contain at least one element of x
    which(c(fast_extract(y, x, fun = "mean")) > 1e-7) # precision is 1e-7
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
         "spatial analysis cannot be performed")
  }
)
