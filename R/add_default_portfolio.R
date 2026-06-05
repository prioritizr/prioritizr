#' @include Portfolio-class.R
NULL

#' Add a default portfolio
#'
#' Generate a portfolio based on defaults.
#'
#' @inheritParams add_cuts_portfolio
#'
#' @details
#' By default, this is portfolio is added to [problem()] objects if no
#' other portfolios is manually specified. In particular, this
#' function adds the [add_single_portfolio()] function to `x` so
#' that only a single solution is generated.
#'
#' @inherit add_cuts_portfolio return seealso
#'
#' @family portfolios
#'
#' @examplesIf prioritizr::do_run_example()
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
#'   add_default_portfolio() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # plot solution
#' plot(s)
#'
#' @export
add_default_portfolio <- function(x) {
  # assert that arguments are valid
  assert_required(x)
  assert(is_conservation_problem(x))
  # add portfolio
  add_single_portfolio(x)
}
