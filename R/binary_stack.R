#' @include internal.R
NULL

#' Binary stack
#'
#' Convert a single-layer [terra::rast()] object that contains integer values
#' into a multi-layer [terra::rast()] object with cell values denote the
#' presence/absence of a given integer value. This is methodology is also known
#' as "one-hot encoding".
#'
#' @param x [terra::rast()] object with a single layer that contains integer
#'   values.
#'
#' @param keep_all `logical` value indicating if all integers should be kept
#'   in the output. If `TRUE`, the output will contain a layer for each
#'   sequential integer between 1 and the maximum value in `x`. If `FALSE`,
#'   the output will only contain layers for integer values present in `x`.
#'   Defaults to `TRUE.`
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
#' @seealso [category_layer()], [terra::segregate()].
#'
#' @examples
#' # create raster with categorical values
#' x <- terra::rast(matrix(c(1, 2, 4, 0, NA, 1), nrow = 3))
#'
#' # plot the raster
#' \dontrun{
#' plot(x, main = "x")
#' }
#'
#' # convert to binary stack
#' y <- binary_stack(x)
#'
#' # plot result
#' \dontrun{
#' plot(y)
#' }
#' @export
binary_stack <- function(x, keep_all = TRUE) {
  assert_required(x)
  UseMethod("binary_stack")
}

#' @rdname binary_stack
#' @method binary_stack Raster
#' @export
binary_stack.Raster <- function(x, keep_all = TRUE) {
  assert(inherits(x, "Raster"))
  cli_warning(raster_pkg_deprecation_notice)
  raster::stack(binary_stack(terra::rast(x, keep_all = keep_all)))
}

#' @rdname binary_stack
#' @method binary_stack SpatRaster
#' @export
binary_stack.SpatRaster <- function(x, keep_all = TRUE) {
  # assert valid arguments
  assert(
    inherits(x, "SpatRaster"),
    is_numeric_values(x),
    assertthat::is.flag(keep_all),
    assertthat::noNA(keep_all)
  )

  # create segregated raster
  y <- terra::segregate(
    x,
    classes = seq_len(round(terra::global(x, "max", na.rm = TRUE)[[1]])),
    keep = FALSE,
    round = TRUE
  )

  # if needed, remove empty layers
  if (!isTRUE(keep_all)) {
    idx <- which(terra::global(y, "max", na.rm = TRUE)[[1]] > 0)
    y <- y[[idx]]
  }

  # return result
  y
}
