#' @include internal.R
NULL

#' Binary stack
#'
#' Convert a [`RasterLayer-class`] object containing
#' categorical identifiers into a [`RasterStack-class`]
#' object where each layer corresponds to a different identifier and values
#' indicate the presence/absence of that category in the input object.
#'
#' @param x [`Raster-class`] object containing a single layer.
#'
#' @details This function is provided to help manage data that encompass
#'   multiple management zones. For instance, this function may be helpful
#'   for preparing raster data for [add_locked_in_constraints()] and
#'   [add_locked_out_constraints()] since they require binary
#'   [`RasterStack-class`] objects as input arguments.
#'
#' @return [`RasterStack-class`] object.
#'
#' @seealso [category_layer()].
#'
#' @examples
#' # create raster with categorical identifers
#' x <- raster(matrix(c(1, 2, 3, 1, NA, 1), nrow = 3))
#'
#' # convert to binary stack
#' y <- binary_stack(x)
#'
#' # plot categorical raster and binary stack representation
#' \donttest{
#' plot(stack(x, y), main = c("x", "y[[1]]", "y[[2]]", "y[[3]]"), nr = 1)
#' }
#' @export
binary_stack <- function(x) {
  # validate argument
  assertthat::assert_that(inherits(x, "Raster"), raster::nlayers(x) == 1,
                          raster::cellStats(!is.na(x), "sum") > 0,
                          raster::cellStats(x, "min") > 0)
  max_value <- raster::cellStats(x, "max")
  assertthat::assert_that(assertthat::is.count(max_value))
  # initialize raster stack
  out <- raster::setValues(x, 0)
  out[raster::Which(is.na(x))] <- NA_real_
  out <- out[[rep(1, max_value)]]
  # populate raster stack
  for (i in seq_len(max_value))
    out[[i]][raster::Which(x == i)] <- 1
  # return result
  out
}
