#' @include internal.R
NULL

#' Binary stack
#'
#' Convert a [terra::rast()] object containing
#' integer/categorical values into a raster
#' object where each layer corresponds to a different integer/categorical
#' value and pixel values denote the presence/absence of the given
#' integer/categorical values.
#'
#' @param x [terra::rast()] object with a single layer.
#'
#' @details This function is provided to help manage data that encompass
#'   multiple management zones. For instance, this function may be helpful
#'   for preparing raster data for [add_locked_in_constraints()] and
#'   [add_locked_out_constraints()] since they require binary
#'   rasters as input arguments.
#'   It is essentially a wrapper for [terra::segregate()].
#'
#' @return A [terra::rast()] object.
#'
#' @seealso [category_layer()].
#'
#' @examples
#' # create raster with categorical values
#' x <- terra::rast(matrix(c(1, 2, 3, 1, NA, 1), nrow = 3))
#'
#' # plot the raster
#' plot(x, main = "x")
#'
#' # convert to binary stack
#' y <- binary_stack(x)
#'
#' # plot result
#' \dontrun{
#' plot(y)
#' }
#' @export
binary_stack <- function(x) UseMethod("binary_stack")

#' @rdname binary_stack
#' @method binary_stack Raster
#' @export
binary_stack.Raster <- function(x) {
  assertthat::assert_that(inherits(x, "Raster"))
  .Deprecated(msg = raster_pkg_deprecation_notice)
  raster::raster(binary_stack.default(terra::rast(x)))
}

#' @rdname binary_stack
#' @method binary_stack default
#' @export
binary_stack.default <- function(x) {
  terra::segregate(x, classes = NULL, keep = FALSE, other = 0, round = FALSE)
}
