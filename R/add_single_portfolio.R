#' @include Portfolio-class.R
NULL

#' Add a single portfolio
#'
#' Generate a portfolio containing a single solution.
#'
#' @inheritParams add_default_portfolio
#'
#' @inherit add_cuts_portfolio return seealso
#'
#' @family portfolios
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create minimal problem with default portfolio
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.05) %>%
#'   add_single_portfolio() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # plot solution
#' plot(s)
#' }
#' @export
add_single_portfolio <- function(x) {
  # assert that arguments are valid
  assert_required(x)
  assert(is_conservation_problem(x))
  # add portfolio
  x$add_portfolio(
    R6::R6Class(
      "SinglePortfolio",
      inherit = Portfolio,
      public = list(
        name = "single portfolio",
        run = function(x, solver) {
          # solve problem
          list(solver$solve(x))
        }
      )
    )$new()
  )
}
