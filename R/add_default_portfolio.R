#' @include Portfolio-class.R
NULL

#' Add a default portfolio
#'
#' Generate a portfolio containing a single solution.
#'
#' @param x [problem()] object.
#'
#' @details
#' By default, this is portfolio is added to [problem()] objects if no
#' other portfolios is manually specified.
#'
#' @inherit add_cuts_portfolio return
#'
#' @seealso
#' See [portfolios] for an overview of all functions for adding a portfolio.
#'
#' @family portfolios
#'
#' @export
add_default_portfolio <- function(x) {
  # assert that arguments are valid
  assert_required(x)
  assert(is_conservation_problem(x))
  # add portfolio
  x$add_portfolio(
    R6::R6Class(
      "DefaultPortfolio",
      inherit = Portfolio,
      public = list(
        name = "default portfolio",
        run = function(x, solver) {
          # solve problem
          return(list(solver$solve(x)))
        }
      )
    )$new()
  )
}
