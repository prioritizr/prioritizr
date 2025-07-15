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
#'   If `NA`, then the memory requirements will be estimated and, if required,
#'   processing will be performed using the method that reduces memory
#'   consumption.
#'   Defaults to `NA`.
#'
#' @param idx `integer` vector containing planning unit
#'   indices. Defaults to `NULL` such that the indices are computed
#'   automatically based on `x`.
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
#' @usage \S4method{rij_matrix}{SpatRaster,SpatRaster}(x, y, memory, idx, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "SpatRaster", y = "SpatRaster"),
  function(x, y, memory = NA, idx = NULL, ...) {
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
    if (!is.null(idx)) {
      assert(
        is.numeric(idx),
        length(idx) > 0
      )
    }
    assert_dots_empty()
    # if needed, identify cells that should be considered planning units
    if (is.null(idx)) {
      idx <- planning_unit_indices(x)
    }
    # process raster
    b <- terra::blocks(y, n = 5)
    if (
      (!identical(memory, FALSE)) &&
      (isTRUE(memory) || identical(length(b$row), 1L))
    ) {
      ## import entire raster and then subset relevant cells
      m <- terra::values(y)[idx, , drop = FALSE]
      ## convert NA values to zeros
      m[is.na(m)] <- 0
      ## convert to output format
      m <- Matrix::t(Matrix::drop0(methods::as(m, "dgCMatrix")))
    } else {
      ## set up raster reading
      terra::readStart(y)
      on.exit(terra::readStop(y), add = TRUE, after = FALSE)
      # process raster in chunks
      m <- lapply(seq_len(b$n), function(i) {
        ## import subset of raster
        m2 <- terra::readValues(
          y, row = b$row[[i]], nrows = b$nrows[[i]],
          dataframe = FALSE, mat = TRUE
        )
        ## determine indices to keep
        m2_idx <-
          (terra::cellFromRowCol(y, b$row[[i]], 1) - 1) +
          seq_len(nrow(m2))
        m2 <- m2[m2_idx %in% idx, , drop = FALSE]
        ## convert NA values to zeros
        m2[is.na(m2)] <- 0
        ## convert to output format
        Matrix::drop0(Matrix::t(methods::as(m2, "TsparseMatrix")))
      })
      # combine chunks results together
      if (length(m) > 1) {
        m <- do.call(cbind, m) # nocov
      } else {
        m <- m[[1]]
      }
      # if needed, coerce to sparse matrix format
      if (!inherits(m, "dgCMatrix")) {
        m <- as_Matrix(m, "dgCMatrix") # nocov
      }
    }
    # add row names
    rownames(m) <- names(y)
    # return result
    m
})

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{sf,SpatRaster}(x, y, fun, memory, idx, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "sf", y = "SpatRaster"),
  function(x, y, fun = "sum", memory = FALSE, idx = NULL, ...) {
    # assert arguments valid
    assert(
      is_numeric_values(y),
      terra::nlyr(y) > 0,
      assertthat::is.flag(memory)
    )
    assert_dots_empty()
    # if needed, determine if calculations can be done in memory
    if (is.na(memory)) {
      memory <- !terra_can_process_in_memory(y, n = 1)
    }
    # if needed, subset x according to indices
    if (!is.null(idx)) {
      assert(
        is.numeric(idx),
        length(idx) > 0
      )
      x <- x[idx, , drop = FALSE]
    }
    # main processing
    if (!isTRUE(memory)) {
      ## extract data for all layers
      m <- fast_extract(x = y, y = x, fun = fun)
      m[is.na(m)] <- 0
      m <- Matrix::drop0(methods::as(m, "dgCMatrix"))
      m <- Matrix::t(m)
    } else {
      ## determine number of layers to process at once
      nl <- terra_n_process_in_memory(y[[c(1, 2)]])
      ## split layers into chunks
      lidx <- parallel::splitIndices(
        terra::nlyr(y),
        ceiling(terra::nlyr(y) / nl)
      )
      ## import layers in batches and add to matrix
      m <- do.call(rbind, lapply(seq_along(lidx), function(j) {
        k <- fast_extract(x = y[[lidx[[j]]]], y = x, fun = fun)
        k[is.na(k)] <- 0
        k <- Matrix::t(Matrix::drop0(methods::as(k, "dgCMatrix")))
        invisible(gc())
        k
      }))
      m <- as_Matrix(m, "dgCMatrix")
    }
    # add row names
    rownames(m) <- names(y)
    # return result
    m
})

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{Raster,Raster}(x, y, memory, idx, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "Raster", y = "Raster"),
  function(x, y, memory = NA, idx = NULL, ...) {
    cli_warning(raster_pkg_deprecation_notice)
    rij_matrix(terra::rast(x), terra::rast(y), memory = memory, idx = idx, ...)
})

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{sf,Raster}(x, y, fun, memory, idx, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "sf", y = "Raster"),
  function(x, y, fun = "sum", memory = NA, idx = NULL, ...) {
    cli_warning(raster_pkg_deprecation_notice)
    rij_matrix(x, terra::rast(y), fun = fun, memory = memory, idx = idx, ...)
})

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{Spatial,Raster}(x, y, fun, memory, idx, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "Spatial", y = "Raster"),
  function(x, y, fun = "sum", memory = NA, idx = NULL, ...) {
    cli_warning(sp_pkg_deprecation_notice)
    rij_matrix(sf::st_as_sf(x), y, fun = fun, memory = memory, idx = idx, ...)
})
