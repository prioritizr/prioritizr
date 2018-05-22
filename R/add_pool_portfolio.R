#' @include Portfolio-proto.R
NULL

#' Add a pool portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' \code{\link{problem}} by extracting all the feasible solutions
#' discovered during the optimization process.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @details This strategy for generating a portfolio requires problems to
#'   be solved using the \emph{Gurobi} software suite (i.e. using
#'   \code{\link{add_gurobi_solver}}. Specifically, version 8.0.0 (or greater)
#'   of the \pkg{gurobi} package must be installed. The solution pool is
#'   generated using the default pool search mode of zero
#'   (see \url{http://www.gurobi.com/documentation/8.0/refman/poolsearchmode.html#parameter:PoolSearchMode}).
#'
#' @inherit add_cuts_portfolio seealso return
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#' \donttest{
#' # create minimal problem with pool portfolio
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_pool_portfolio() %>%
#'       add_default_solver(gap = 0.02, verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print number of solutions found
#' print(length(s1))
#'
#' # plot solutions
#' plot(stack(s1), axes = FALSE, box = FALSE)
#'
#' # build multi-zone conservation problem with pool portfolio
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions() %>%
#'       add_pool_portfolio() %>%
#'       add_default_solver(gap = 0.02, verbose = FALSE)
#'
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print number of solutions found
#' print(length(s2))
#'
#' # print solutions
#' str(s2, max.level = 1)
#'
#' # plot solutions in portfolio
#' plot(stack(lapply(s2, category_layer)), main = "solution", axes = FALSE,
#'      box = FALSE)
#' }
#' @name add_pool_portfolio
NULL

#' @export
methods::setClass("PoolPortfolio", contains = "Portfolio")

#' @rdname add_pool_portfolio
#' @export
add_pool_portfolio <- function(x) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # check that version 8.0.0 or greater of gurobi is installed
  if (!requireNamespace("gurobi", quietly = TRUE))
    stop(paste("the gurobi R package is required to generate solutions ",
               "using the pool portfolio"))
  if (utils::packageVersion("gurobi") < as.package_version("8.0.0"))
    stop(paste("version 8.0.0 (or greater) of the gurobi package is required ",
               "to generate solution using the pool portfolio"))
  # add portfolio
  x$add_portfolio(pproto(
    "PoolPortfolio",
    Portfolio,
    name = "Pool portfolio",
    run = function(self, x, solver) {
      ## check that problems has gurobi solver
      if (!inherits(solver, "GurobiSolver"))
        stop("problems must be solved using Gurobi to use the pool portfolio")
      ## solve problem
      sol <- solver$solve(x, PoolSearchMode = 0, PoolSolutions = 2000000000)
      ## compile results
      if (!is.null(sol$pool)) {
        sol <- append(list(sol[-5]),
                      lapply(sol$pool,
                             function(z) list(x = z$xn, objective = z$objval,
                                              status = z$status,
                                              runtime = sol$runtime)))
      } else {
       sol <- list(sol)
      }
      ## return solution
      return(sol)
    }
))}
