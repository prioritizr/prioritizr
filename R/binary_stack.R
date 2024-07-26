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
#' @param x [terra::rast()] object with a single layer that contains integer
#'   values.
#'
#' @details
#' This function is provided to help manage data that encompass
#' multiple management zones. For instance, this function may be helpful
#' for preparing raster data for [add_locked_in_constraints()] and
#' [add_locked_out_constraints()] since they require binary
#' rasters as input arguments.
#' It is essentially a wrapper for [terra::segregate()].
#' Note that this function assumes `x` contains integer values.
#'
#' @return A [terra::rast()] object.
#'
#' @seealso [category_layer()].
#'
#' @examples
#' # create raster with categorical values
#' x <- terra::rast(matrix(c(1, 2, 4, 0, NA, 1), nrow = 3))
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
binary_stack <- function(x) {
  assert_required(x)
  UseMethod("binary_stack")
}

#' @rdname binary_stack
#' @method binary_stack Raster
#' @export
binary_stack.Raster <- function(x) {
  assert(inherits(x, "Raster"))
  cli_warning(raster_pkg_deprecation_notice)
  raster::stack(binary_stack(terra::rast(x)))
}

#' @rdname binary_stack
#' @method binary_stack SpatRaster
#' @export
binary_stack.SpatRaster <- function(x) {
  assert(
    inherits(x, "SpatRaster"),
    is_numeric_values(x)
  )
  # create segregated raster
  r <- terra::segregate(
    x,
    classes = seq_len(round(terra::global(x, "max", na.rm = TRUE)[[1]])),
    keep = FALSE,
    round = TRUE
  )
  # check if additional blank rasters are needed
  if (!identical(names(r), as.character(seq_len(terra::nlyr(r))))) {
    # create a zero layer
    z <- r[[1]] * 0
    # insert additional layers as needed
    for (i in setdiff(as.character(seq_len(terra::nlyr(r))), names(r))) {
      names(z) <- i
      r <- c(r, z)
    }
    # re-order layers
    r <- r[[as.character(seq_len(terra::nlyr(r)))]]
  }
  # return result
  r
}
