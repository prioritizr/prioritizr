#' @include internal.R
NULL

#' Category layer
#'
#' Convert a multi-layer [terra::rast()] object into a single-layer
#' [terra::rast()] object where pixel values indicate which input layer
#' had the greatest value.
#'
#' @param x [terra::rast()] object containing multiple
#'   layers.
#'
#' @details
#' This function is provided to help manage data that encompass
#' multiple management zones. For instance, this function may be helpful
#' for interpreting solutions for problems associated with multiple zones that
#' have binary decisions.
#' It is essentially a wrapper for [terra::which.max()].
#'
#' @return A [terra::rast()] object.
#'
#' @seealso
#' The [binary_stack()] function performs the reverse of this function.
#'
#' @examples
#' # create a binary raster stack
#' x <- terra::rast(list(
#'  terra::rast(matrix(c(1, 0, 0, 1, NA, 0), nrow = 3)),
#'  terra::rast(matrix(c(0, 1, 0, 0, NA, 0), nrow = 3)),
#'  terra::rast(matrix(c(0, 0, 1, 0, NA, 1), nrow = 3))
#' ))
#'
#' # plot data
#' plot(x)
#'
#' # convert to category layer
#' y <- category_layer(x)
#'
#' # plot result
#' \dontrun{
#' plot(y)
#' }
#' @export
category_layer <- function(x) {
  assert_required(x)
  UseMethod("category_layer")
}

#' @rdname category_layer
#' @method category_layer Raster
#' @export
category_layer.Raster <- function(x) {
  assert(inherits(x, "Raster"))
  cli_warning(raster_pkg_deprecation_notice)
  raster::raster(category_layer.default(terra::rast(x)))
}

#' @rdname category_layer
#' @method category_layer default
#' @export
category_layer.default <- function(x) {
  # assert valid arguments
  assert(
    inherits(x, "SpatRaster"),
    is_numeric_values(x)
  )
  # create layer
  r <- terra::which.max(x)
  # ensure that category layer has zeros in correct places
  r[all(x == 0)] <- 0
  # return result
  r
}
