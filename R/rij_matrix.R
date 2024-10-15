#' @include internal.R fast_extract.R
NULL

#' Feature by planning unit matrix
#'
#' Generate a matrix showing the amount of each feature in each planning
#' unit (also known as an *rij* matrix).
#'
#' @param x [terra::rast()] or [sf::sf()] object representing planning units.
#'
#' @param y [terra::rast()]  object.
#'
#' @param fun `character` for summarizing values inside each planning unit.
#'   This parameter is only used when the argument to `x` is a
#'   [sf::sf()] object.
#'   Defaults to `"sum"`.
#'
#' @param memory `logical` should calculations be performed using a method
#'   that prioritizes reduced memory consumption over speed?
#'   This is useful when processing particularly large raster datasets.
#'   If `TRUE`, then calculations are performed by processing each raster layer
#'   in `y` in a sequential manner.
#'   If `FALSE`, then calculations are performed by processing all raster layers
#'   in `y` together.
#'   Defaults to `NA`, such that calculations are automatically performed
#'   using the best method given data requirements and the amount of available
#'   memory.
#'
#' @param ... not used.
#'
#' @details
#' Generally, processing [sf::st_sf()] data takes much longer to process than
#' [terra::rast()] data.
#' As such, it is recommended to use [terra::rast()] data for planning units
#' where possible.
#' The performance of this function for large [terra::rast()] datasets
#' can be improved by increasing the GDAL cache size.
#' The default cache size is 25 MB.
#' For example, the following code can be used to set the cache size to 4 GB.
#' ```
#' terra::gdalCache(size = 4000)
#' ```
#'
#' @return A [`Matrix::dgCMatrix-class`] sparse matrix object.
#'   The sparse matrix represents the spatial intersection between the
#'   planning units and the features. Rows correspond to features,
#'   and columns correspond to planning units. Values correspond to the amount
#'   (or presence/absence) of the feature in the planning unit. For example,
#'   the amount of the third species in the second planning unit would be
#'   stored in the third column and second row.
#'
#' @name rij_matrix
#'
#' @exportMethod rij_matrix
#'
#' @aliases rij_matrix,Raster,Raster-method rij_matrix,Spatial,Raster-method rij_matrix,sf,Raster-method rij_matrix,SpatRaster,SpatRaster-method rij_matrix,sf,SpatRaster-method
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create rij matrix using raster layer planning units
#' rij_raster <- rij_matrix(sim_pu_raster, sim_features)
#' print(rij_raster)
#'
#' # create rij matrix using polygon planning units
#' rij_polygons <- rij_matrix(sim_pu_polygons, sim_features)
#' print(rij_polygons)
#'
#' # create rij matrix using raster planning units with multiple zones
#' rij_zones_raster <- rij_matrix(sim_zones_pu_raster, sim_features)
#' print(rij_zones_raster)
#' }
#' @export
methods::setGeneric(
  "rij_matrix",
  signature = methods::signature("x", "y"),
  function(x, y, ...) {
    assert_required(x)
    assert_required(y)
    assert(
      is_inherits(x, c("sf", "SpatRaster", "Spatial", "Raster")),
      is_inherits(y, c("sf", "SpatRaster", "Spatial", "Raster"))
    )
    standardGeneric("rij_matrix")
  }
)

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{SpatRaster,SpatRaster}(x, y, memory, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "SpatRaster", y = "SpatRaster"),
  function(x, y, memory = NA, ...) {
    # assert that arguments are valid
    assert(
      inherits(x, "SpatRaster"),
      is_numeric_values(x),
      inherits(y, "SpatRaster"),
      is_numeric_values(y),
      terra::nlyr(x) > 0,
      terra::nlyr(y) > 0,
      is_comparable_raster(x, y),
      assertthat::is.flag(memory)
    )
    assert_dots_empty()
    # identify cells that should be considered planning units
    idx <- terra::cells(terra::allNA(x), 0)[[1]]
    # if needed, determine if calculations can be done in memory
    if (is.na(memory)) {
      memory <- terra_can_process_in_memory(y, n = 2)
    }
    # run processing
    if (!isTRUE(memory)) {
      # generate matrix
      m <- as.matrix(y[idx])
      m[is.na(m)] <- 0
      m <- Matrix::t(Matrix::drop0(methods::as(m, "dgCMatrix")))
    } else {
      # initialize matrix
      m <- Matrix::sparseMatrix(
        i = 1, j = 1, x = 0, repr = "C",
        dims = c(terra::nlyr(y), length(idx))
      )
      # import each layer separately and add to matrix
      for (i in seq_len(terra::nlyr(y))) {
        v <- y[[i]][idx][[1]]
        v[is.na(v)] <- 0
        m[i, ] <- v
        rm(v)
        m <- Matrix::drop0(m)
        invisible(gc())
      }
    }
    # add row names
    rownames(m) <- names(y)
    # return result
    m
})

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{sf,SpatRaster}(x, y, fun, memory, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "sf", y = "SpatRaster"),
  function(x, y, fun = "sum", memory = NA, ...) {
    # assert arguments valid
    assert(
      is_numeric_values(y),
      terra::nlyr(y) > 0,
      assertthat::is.flag(memory)
    )
    assert_dots_empty()
    # if needed, determine if calculations can be done in memory
    if (is.na(memory)) {
      memory <- terra_can_process_in_memory(y, n = 2)
    }
    # main processing
    if (!isTRUE(memory)) {
      ## extract data for all layers
      m <- fast_extract(x = y, y = x, fun = fun)
      m[is.na(m)] <- 0
      m <- Matrix::drop0(methods::as(m, "dgCMatrix"))
      m <- Matrix::t(m)
    } else {
      ## initialize matrix
      m <- Matrix::sparseMatrix(
        i = 1, j = 1, x = 0, repr = "C",
        dims = c(terra::nlyr(y), nrow(x))
      )
      ## import each layer separately and add to matrix
      for (i in seq_len(terra::nlyr(y))) {
        v <- c(fast_extract(x = y[[i]], y = x, fun = fun))
        v[is.na(v)] <- 0
        m[i, ] <- v
        rm(v)
        m <- Matrix::drop0(m)
        invisible(gc())
      }
    }
    # add row names
    rownames(m) <- names(y)
    # return result
    m
})

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{Raster,Raster}(x, y, memory, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "Raster", y = "Raster"),
  function(x, y, memory = NA, ...) {
    cli_warning(raster_pkg_deprecation_notice)
    rij_matrix(terra::rast(x), terra::rast(y), memory = memory, ...)
})

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{sf,Raster}(x, y, fun, memory, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "sf", y = "Raster"),
  function(x, y, fun = "sum", memory = NA, ...) {
    cli_warning(raster_pkg_deprecation_notice)
    rij_matrix(x, terra::rast(y), fun = fun, memory = memory, ...)
})

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{Spatial,Raster}(x, y, fun, memory, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "Spatial", y = "Raster"),
  function(x, y, fun = "sum", memory = NA, ...) {
    cli_warning(sp_pkg_deprecation_notice)
    rij_matrix(sf::st_as_sf(x), y, fun = fun, memory = memory, ...)
})
