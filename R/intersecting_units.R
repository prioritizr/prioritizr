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
#' @seealso [fast_extract()].
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
#' plot(r, main = "x=SpatRaster")
#' plot(r_with_holes, main = "y=SpatRaster")
#' print(intersecting_units(r, r_with_holes))
#'
#' # intersect raster with sf
#' par(mfrow = c(1, 2))
#' plot(r, main = "x=SpatRaster")
#' plot(ply_with_holes, main = "y=sf", key.pos = NULL, reset = FALSE)
#' print(intersecting_units(r, ply_with_holes))
#'
#' # intersect sf with raster
#' par(mfrow = c(1, 2))
#' plot(ply, main = "x=sf")
#' plot(r_with_holes, main = "y=SpatRaster")
#' print(intersecting_units(ply, r_with_holes))
#'
#' # intersect sf with sf
#' par(mfrow = c(1, 2))
#' plot(ply, main = "x=sf")
#' plot(ply_with_holes, main = "y=sf", key.pos = NULL, reset = FALSE)
#' print(intersecting_units(ply, ply_with_holes))
#' }
#'
#' @export
methods::setGeneric(
  "intersecting_units",
  signature = methods::signature("x", "y"),
  function(x, y) {
    rlang::check_required(x)
    rlang::check_required(y)
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
    .Deprecated(msg = raster_pkg_deprecation_notice)
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
    .Deprecated(msg = raster_pkg_deprecation_notice)
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
    .Deprecated(msg = sp_pkg_deprecation_notice)
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
    .Deprecated(msg = sp_pkg_deprecation_notice)
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
      inherits(y, "SpatRaster"),
      terra::nlyr(x) == 1,
      is_same_crs(x, y),
      is_comparable_raster(x, y)
    )
    # extract first layer
    x <- x[[1]]
    y <- y[[1]]
    y <- terra::as.bool(y)
    # return positive cells
    terra::cells(terra::as.bool(y) & !is.na(x), 1)[[1]]
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
    # find out which units in x intersect with any units in y
    int1 <- sf::st_intersects(x, y, sparse = TRUE)
    int2 <- sf::st_touches(x, y, sparse = TRUE)
    # convert dense list to sparse matrix
    names(int1) <- as.character(seq_len(nrow(x)))
    names(int2) <- as.character(seq_len(nrow(x)))
    int1 <- rcpp_list_to_matrix_indices(int1)
    int2 <- rcpp_list_to_matrix_indices(int2)
    int1 <- Matrix::sparseMatrix(
      i = int1$i, j = int1$j, x = 1,
      dims = c(nrow(y), nrow(x))
    )
    int2 <- Matrix::sparseMatrix(
      i = int2$i, j = int2$j, x = 1,
      dims = c(nrow(y), nrow(x))
    )
    # exclude units from being intersecting if they only touch
    int1[int2 > 0.5] <- 0
    int1 <- as_Matrix(Matrix::drop0(int1), "dgTMatrix")
    int1@j + 1
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
      terra::nlyr(y) == 1,
      is_same_crs(x, y),
      is_spatial_extents_overlap(x, y)
    )
    # prepare raster data
    y <- y[[1]]
    # find out which units in y contain at least one element of x
    ## precision is 1e-7
    which(c(fast_extract(terra::as.bool(y), x, fun = "mean")) > 1e-7)
  }
)

#' @name intersecting_units
#' @usage \S4method{intersecting_units}{data.frame,ANY}(x, y)
#' @rdname intersecting_units
methods::setMethod(
  "intersecting_units",
  methods::signature(x = "data.frame", y = "ANY"),
  function(x, y) {
    cli::cli_abort(
      "{.arg x} must not have planning units in a data frame.",
      "i" = paste(
        "This is data frames lack spatial information to",
        "perform spatial analyses."
      )
    )
  }
)

na_crs <- "ENGCRS[\"Undefined Cartesian SRS\",\n    EDATUM[\"\"],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"
