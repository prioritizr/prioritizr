#' @include internal.R pproto.R ConservationProblem-proto.R
NULL

#' Log-linear interpolation
#'
#' Log-linearly interpolate values between two thresholds.
#'
#' @param x `numeric` *x* values for which interpolate *y*
#'   values.
#'
#' @param coordinate_one_x `numeric` value for lower *x*-coordinate.
#'
#' @param coordinate_one_y `numeric` value for lower *y*-coordinate.
#'
#' @param coordinate_two_x `numeric` value for upper *x*-coordinate.
#'
#' @param coordinate_two_y `numeric` value for upper *y*-coordinate.
#'
#' @details Values are log-linearly interpolated at the *x*-coordinates
#'   specified in `x` using the lower and upper coordinate arguments to
#'   define the line. Values lesser or greater than these numbers are assigned
#'   the minimum and maximum *y* coordinates.
#'
#' @return `numeric` values.
#'
#' @examples
#' # create series of x-values
#' x <- seq(0, 1000)
#'
#' # interpolate y-values for the x-values given the two reference points:
#' # (200, 100) and (900, 15)
#' y <- loglinear_interpolation(x, 200, 100, 900, 15)
#'
#' # plot the interpolated values
#' \donttest{
#' plot(y ~ x)
#'
#' # add the reference points to the plot (shown in red)
#' points(x = c(200, 900), y = c(100, 15), pch = 18, col = "red", cex = 2)
#' }
#' @export
loglinear_interpolation <- function(x, coordinate_one_x, coordinate_one_y,
                                  coordinate_two_x, coordinate_two_y) {
  assertthat::assert_that(is.numeric(x), isTRUE(all(is.finite(x))),
                          assertthat::is.scalar(coordinate_one_x),
                          assertthat::is.scalar(coordinate_one_y),
                          assertthat::is.scalar(coordinate_two_x),
                          assertthat::is.scalar(coordinate_two_y),
                          coordinate_one_x < coordinate_two_x)
  out <- rep(NA_real_, length(x))
  out[x <= coordinate_one_x] <- coordinate_one_y
  out[x >= coordinate_two_x] <- coordinate_two_y
  between.pos <- which(is.na(out))
  out[between.pos] <- stats::approx(
                             x = log(c(coordinate_one_x, coordinate_two_x)),
                             y = c(coordinate_one_y, coordinate_two_y),
                             xout = log(x[between.pos]), method = "linear")$y
  return(out)
}
