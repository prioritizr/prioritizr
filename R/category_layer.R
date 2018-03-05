#' @include internal.R
NULL

#' Category layer
#'
#' Convert a \code{\link[raster]{RasterStack-class}}
#' object where each layer corresponds to a different identifier and values
#' indicate the presence/absence of that category into a
#' \code{\link[raster]{RasterLayer-class}} object containing categorical
#' identifiers.
#'
#' @param x \code{\link[raster]{Raster-class}} object containing a multiple
#'   layers. Note that pixels must be 0, 1 or \code{NA} values.
#'
#' @details This function is provided to help manage data that encompass
#'   multiple management zones. For instance, this function may be helpful
#'   for interpreting solutions for problems associated with multiple zones that
#'   have binary decisions.
#'
#' @return \code{\link[raster]{RasterLayer-class}} object.
#'
#' @seealso \code{\link{binary_stack}}.
#'
#' @examples
#' # create a binary raster stack
#' x <- stack(raster(matrix(c(1, 0, 0, 1, NA, 0), nrow = 3)),
#'            raster(matrix(c(0, 1, 0, 0, NA, 0), nrow = 3)),
#'            raster(matrix(c(0, 0, 1, 0, NA, 1), nrow = 3)))
#'
#' # convert to binary stack
#' y <- category_layer(x)
#'
#' # plot categorical raster and binary stack representation
#' plot(stack(x, y), main = c("x[[1]]", "x[[2]]", "x[[3]]", "y"), nr = 1)
#'
#' @export
category_layer <- function(x) {
  # validate argument
  assertthat::assert_that(inherits(x, "Raster"), raster::nlayers(x) > 1,
                          all(raster::cellStats(!is.na(x), "sum") > 0),
                          all(raster::cellStats(x, "max") == 1),
                          raster::cellStats(sum(x), "max") == 1)
  # initialize raster layer
  out <- raster::setValues(x[[1]], 0)
  out[raster::Which(is.na(x[[1]]))] <- NA_real_
  # populate raster layer
  for (i in seq_len(raster::nlayers(x)))
    out[raster::Which(x[[i]] == 1)] <- i
  # return result
  out
}
