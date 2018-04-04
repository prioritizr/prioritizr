#' @include Portfolio-proto.R
NULL

#' Default portfolio
#'
#' Generate a portfolio for a conservation planning \code{\link{problem}}
#' that contains a single solution.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @seealso \code{\link{portfolios}}.
#'
#' @noRd
add_default_portfolio <- function(x) {
  return(add_shuffle_portfolio(x, 1))
}
