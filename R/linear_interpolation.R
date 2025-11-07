#' @include internal.R
NULL

#' Linear interpolation
#'
#' Linearly interpolate values between two thresholds.
#'
#' @inheritParams loglinear_interpolation
#'
#' @inherit loglinear_interpolation details return
#'
#' @examples
#' \dontrun{
#' # create series of x-values
#' x <- seq(0, 1000)
#'
#' # interpolate y-values for the x-values given the two reference points:
#' # (200, 100) and (900, 15)
#' y <- loglinear_interpolation(x, 200, 100, 900, 15)
#'
#' # plot the interpolated values
#' plot(y ~ x)
#'
#' # add the reference points to the plot (shown in red)
#' points(x = c(200, 900), y = c(100, 15), pch = 18, col = "red", cex = 2)
#' }
#' @export
linear_interpolation <- function(x, coordinate_one_x, coordinate_one_y,
                                    coordinate_two_x, coordinate_two_y) {
  assert_required(x)
  assert_required(coordinate_one_x)
  assert_required(coordinate_one_y)
  assert_required(coordinate_two_x)
  assert_required(coordinate_two_y)
  assert(
    is.numeric(x),
    all_finite(x),
    assertthat::is.number(coordinate_one_x),
    assertthat::is.number(coordinate_one_y),
    assertthat::is.number(coordinate_two_x),
    assertthat::is.number(coordinate_two_y),
    assertthat::noNA(coordinate_one_x),
    assertthat::noNA(coordinate_one_y),
    assertthat::noNA(coordinate_two_x),
    assertthat::noNA(coordinate_two_y),
    coordinate_one_x <= coordinate_two_x
  )
  out <- rep(NA_real_, length(x))
  out[x < coordinate_one_x] <- coordinate_one_y
  out[x > coordinate_two_x] <- coordinate_two_y
  idx <- is.na(out)
  if (any(idx)) {
    out[idx] <- stats::approx(
      x = c(coordinate_one_x, coordinate_two_x),
      y = c(coordinate_one_y, coordinate_two_y),
      xout = x[idx], method = "linear"
    )$y
  }
  out
}
