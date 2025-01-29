#' @include internal.R
NULL

#' Boundary matrix
#'
#' Generate a matrix describing the amount of shared boundary length
#' between different planning units, and the total amount of boundary length
#' for each planning unit.
#'
#' @param x [terra::rast()] or [sf::sf()] object representing planning units.
#'
#' @param ... not used.
#'
#' @details
#' This function assumes the data are in a coordinate
#' system where Euclidean distances accurately describe the proximity
#' between two points on the earth. Thus spatial data in a
#' longitude/latitude coordinate system (i.e.,
#' [WGS84](https://spatialreference.org/ref/epsg/4326/))
#' should be reprojected to another coordinate system before using this
#' function. Note that for [terra::rast()] objects
#' boundaries are missing for cells that have  missing (`NA`) values in all
#' cells.
#'
#' @section Notes:
#' In earlier versions, this function had an extra `str_tree` parameter
#' that could be used to leverage STR query trees to speed up processing
#' for planning units in vector format.
#' Although this functionality improved performance, it was not
#' enabled by default because the underlying function
#' (i.e., `rgeos:gUnarySTRtreeQuery()`) was documented as experimental.
#' The `boundary_matrix()` function has since been updated so that it will
#' use STR query trees to speed up processing for planning units in vector
#' format (using [terra::sharedPaths()]).
#'
#' Also, note that in previous versions, cell values along the matrix
#' diagonal indicated the perimeter associated with planning units
#' that did not contain any neighbors. This has now changed such
#' that values along the diagonal now correspond to the total
#' perimeter associated with each planning unit.
#'
#' @return A [`Matrix::dsCMatrix-class`] symmetric sparse matrix object.
#'   Each row and column represents a planning unit.
#'   Cell values indicate the shared boundary length between different pairs
#'   of planning units. Values along the matrix diagonal indicate the
#'   total perimeter associated with each planning unit.
#'
#' @name boundary_matrix
#'
#' @rdname boundary_matrix
#'
#' @seealso
#' Boundary matrix data might need rescaling to improve optimization
#' performance, see [rescale_matrix()] to perform these calculations.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_pu_polygons <- get_sim_pu_polygons()
#'
#' # subset data to reduce processing time
#' r <- terra::crop(sim_pu_raster, c(0, 0.3, 0, 0.3))
#' ply <- sim_pu_polygons[c(1:3, 11:13, 20:22), ]
#'
#' # create boundary matrix using raster data
#' bm_raster <- boundary_matrix(r)
#'
#' # create boundary matrix using polygon data
#' bm_ply <- boundary_matrix(ply)
#'
#' # plot raster data
#' plot(r, main = "raster", axes = FALSE)
#'
#' # plot boundary matrix
#' # here each row and column corresponds to a different planning unit
#' Matrix::image(bm_raster, main = "boundary matrix")
#'
#' # plot polygon data
#' plot(ply[, 1], main = "polygons", axes = FALSE)
#'
#' # plot boundary matrix
#' # here each row and column corresponds to a different planning unit
#' Matrix::image(bm_ply, main = "boundary matrix")
#' }
#' @export
boundary_matrix <- function(x, ...) {
  assert_required(x)
  UseMethod("boundary_matrix")
}

#' @rdname boundary_matrix
#' @method boundary_matrix Raster
#' @export
boundary_matrix.Raster <- function(x, ...) {
  # assert that arguments are valid
  assert(inherits(x, "Raster"))
  # deprecation notice
  cli_warning(raster_pkg_deprecation_notice)
  # convert to SpatRaster for processing
  boundary_matrix.SpatRaster(terra::rast(x), ...)
}

#' @rdname boundary_matrix
#' @method boundary_matrix SpatRaster
#' @export
boundary_matrix.SpatRaster <- function(x, ...) {
  # assert that arguments are valid
  assert(inherits(x, "SpatRaster"))
  # indices of cells with finite values
  include <- terra::cells(terra::allNA(x), 0)[[1]]
  # set x to a single raster layer with only values in pixels that are not
  # NA in all layers
  x <- terra::setValues(x[[1]], NA_real_)
  x[include] <- 1
  # find the neighboring indices of these cells
  ud <- matrix(c(0, 0, 0, 1, 0, 1, 0, 0, 0), 3, 3)
  lr <- matrix(c(0, 1, 0, 0, 0, 0, 0, 1, 0), 3, 3)
  # determine correct resolution values
  ## note that this depends on the version of terra that is installed
  if (utils::packageVersion("terra") >= package_version("1.8-10")) {
    lr_res <- terra::xres(x)
    ud_res <- terra::yres(x)
  } else { # nocov
    lr_res <- terra::yres(x) # nocov
    ud_res <- terra::xres(x) # nocov
  }
  b <- rbind(
    data.frame(
      terra::adjacent(x, include, pairs = TRUE, directions = lr),
      boundary = lr_res
    ),
    data.frame(
      terra::adjacent(x, include, pairs = TRUE, directions = ud),
      boundary = ud_res
    )
  )
  names(b) <- c("id1", "id2", "boundary")
  b$id1 <- as.integer(b$id1)
  b$id2 <- as.integer(b$id2)
  b <- b[(b$id1 %in% include) & (b$id2 %in% include), , drop = FALSE]
  # coerce to sparse matrix object
  m <- Matrix::forceSymmetric(
    Matrix::sparseMatrix(
      i = b[[1]],
      j = b[[2]],
      x = b[[3]],
      dims = rep(terra::ncell(x), 2)
    )
  )
  # set total boundary of each cell for matrix diagonal
  Matrix::diag(m)[include] <- sum(terra::res(x)) * 2
  # return matrix
  as_Matrix(m, "dsCMatrix")
}

#' @rdname boundary_matrix
#' @method boundary_matrix SpatialPolygons
#' @export
boundary_matrix.SpatialPolygons <- function(x, ...) {
  # assert that arguments are valid
  assert(inherits(x, "SpatialPolygons"))
  # deprecation notice
  cli_warning(sp_pkg_deprecation_notice)
  # convert to sf format for processing
  boundary_matrix.sf(sf::st_as_sf(x))
}

#' @rdname boundary_matrix
#' @method boundary_matrix SpatialLines
#' @export
boundary_matrix.SpatialLines <- function(x, ...) {
  assert(inherits(x, "SpatialLines"))
  cli::cli_abort(
    c(
      "{.arg x} must not contain line geometries.",
      "i" = "This is because lines do not have boundaries.",
      "i" = "See {.topic constraints} for alternative constraints."
    )
  )
}

#' @rdname boundary_matrix
#' @method boundary_matrix SpatialPoints
#' @export
boundary_matrix.SpatialPoints <- function(x, ...) {
  assert(inherits(x, "SpatialPoints"))
  cli::cli_abort(
    c(
      "{.arg x} must not contain point geometries.",
      "i" = "This is because points do not have boundaries.",
      "i" = "See {.topic constraints} for alternative constraints."
    )
  )
}

#' @rdname boundary_matrix
#' @method boundary_matrix sf
#' @export
boundary_matrix.sf <- function(x, ...) {
  # assert valid arguments
  assert(inherits(x, "sf"), is_valid_geometries(x))
  geomc <- st_geometry_classes(x)
  assert(
    !any(grepl("POINT", geomc, fixed = TRUE)),
    msg = c(
      "{.arg x} must not contain point geometries.",
      "i" = "This is because points do not have boundaries.",
      "i" = "See {.topic constraints} for alternative constraints."
    )
  )
  assert(
    !any(grepl("LINE", geomc, fixed = TRUE)),
    msg = c(
      "{.arg x} must not contain line geometries.",
      "i" = "This is because lines do not have boundaries.",
      "i" = "See {.topic constraints} for alternative constraints."
    )
  )

  # convert to terra object
  x <- terra::vect(x)

  # calculate shared paths
  d <- terra::sharedPaths(x)

  # return result
  Matrix::sparseMatrix(
    i = c(d$id1, seq_len(nrow(x))),
    j = c(d$id2, seq_len(nrow(x))),
    x = round(suppressWarnings(c(terra::perim(d), terra::perim(x))), 8),
    symmetric = TRUE,
    dims = rep(nrow(x), 2)
  )
}

#' @rdname boundary_matrix
#' @method boundary_matrix default
#' @export
boundary_matrix.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "{.arg x} must be a {.cls sf} or {.cls SpatRaster}.",
      "x" = "{.arg x} is a {.cls {class(x)}}."
    )
  )
}
