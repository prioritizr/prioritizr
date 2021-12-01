#' @include Portfolio-proto.R
NULL

#' Default portfolio
#'
#' Generate a portfolio for a conservation planning [problem()]
#' that contains a single solution.
#'
#' @param x [problem()] (i.e., [`ConservationProblem-class`]) object.
#'
#' @seealso
#' See [portfolios] for an overview of all functions for adding a portfolio.
#'
#' @family portfolios
#'
#' @inherit add_cuts_portfolio return
#'
#' @noRd
add_default_portfolio <- function(x) {
  return(add_shuffle_portfolio(x, 1))
}
