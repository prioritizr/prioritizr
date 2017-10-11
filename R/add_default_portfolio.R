#' @include Portfolio-proto.R
NULL

#' Default portfolio method
#'
#' Generate a portfolio containing one solution.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @seealso \code{\link{portfolios}}.
#'
#' @noRd
add_default_portfolio <- function(x) {
  return(add_shuffle_portfolio(x, 1))
}
