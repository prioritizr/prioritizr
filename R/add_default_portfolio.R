#' @include Portfolio-proto.R
NULL

#' Default portfolio
#'
#' Generate a portfolio for a conservation planning [problem()]
#' that contains a single solution.
#'
#' @param x [problem()] (i.e. [`ConservationProblem-class`]) object.
#'
#' @seealso [portfolios].
#'
#' @noRd
add_default_portfolio <- function(x) {
  return(add_shuffle_portfolio(x, 1))
}
