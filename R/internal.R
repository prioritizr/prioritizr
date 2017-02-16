
#' @export
methods::setOldClass('waiver')

#' Waiver
#' 
#' Create a \code{waiver} object. 
#' 
#' @details This object is used to represent that the user has not manually 
#' specified a setting, and so defaults should be used. By explictly 
#' using a \code{waiver()}, this means that \code{NULL} objects can be a 
#' valid setting. The use of a "waiver" object was inspired by the
#' \emph{ggplot2} package.
#' 
#' @return \code{object} of class \code{Waiver}.
#'
#' @noRd
waiver <- function() structure(NULL, class = "Waiver")

#' @export
methods::setOldClass('Id')

#' Identifier
#'
#' Generate a new unique identifier.
#'
#' @details Identifiers are made using the \code{\link[uuid]{UUIDgenerate}}.
#'
#' @return \code{Id} object.
#' 
#' @seealso \code{\link[uuid]{UUIDgenerate}}.
#'
#' @aliases Id
#'
#' @noRd
new_id <- function() {
  x <- uuid::UUIDgenerate()
  class(x) <- c('Id', class(x))
  x
}

#' Log-linear interpolation
#' 
#' Loglinearly interpolate values. 
#'
#' @param x \code{numeric} \emph{x} values to interpolate emph{y} values at.
#'
#' @param coordinate_one_x \code{numeric} value for lower \emph{x}-coordinate.
#'
#' @param coordinate_one_y \code{numeric} value for lower \emph{y}-coordinate.
#'
#' @param coordinate_two_x \code{numeric} value for upper \emph{x}-coordinate.
#'
#' @param coordinate_two_y \code{numeric} value for upper \emph{y}-coordinate.
#'
#' @details Values are log-linearly interpolated at the \emph{x}-coordinates 
#'   specified in \code{x} using the lower and upper coordnate arguments to 
#'   define the line. Values lesser or greater than these numbers are assigned
#'   the minimum and maximum \emph{y} coordinates.
#'
#' @return \code{numeric} values.
#'
#' @example
#' x <- seq(0, 1000)
#' y <- loglinear_interpolate(x, 200, 100, 900, 15)
#' plot(y~x)
#' points(x=c(200, 900), y=c(100, 15), pch=18, col='red', cex=2)
#'
#' @noRd
loglinear_interpolate <- function(x, coordinate_one_x, coordinate_one_y,
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
  out[between.pos] <- approx(x=log(c(coordinate_one_x, coordinate_two_x)),
                             y=c(coordinate_one_y, coordinate_two_y),
                             xout=log(x[between.pos]), method='linear')$y
  out
}

#' Check
#'
#' Check that the output from \code{\link[assert_that]{see_if}}
#' is valid.
#' 
#' @param x \code{character} or \code{object}
#' 
#' @details This object will return an error if the argument to \code{x}
#'   is \code{FALSE}, and for the error message, will show the error
#'   message attached to the object.
#' 
#' @return invisible \code{TRUE}.
#'
#' @noRd
check_that <- function(x) {
  if (!isTRUE(x))
    stop(attr(x, 'msg')[1])
  invisible(TRUE)
}

