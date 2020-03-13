#' @include Portfolio-proto.R
NULL

#' Add an extra portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' \code{\link{problem}} by storing feasible solutions
#' discovered during the optimization process.
#' This method is useful for quickly obtaining multiple solutions,
#' but does not provide any guarantees on the number of solutions, or
#' the quality of solutions.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @details This strategy for generating a portfolio requires problems to
#'   be solved using the \emph{Gurobi} software suite (i.e. using
#'   \code{\link{add_gurobi_solver}}. Specifically, version 8.0.0 (or greater)
#'   of the \pkg{gurobi} package must be installed.
#'
#' @inherit add_cuts_portfolio seealso return
#'
#' @examples
#' \donttest{
#' # set seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create minimal problem with a portfolio for extra solutions
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.05) %>%
#'       add_extra_portfolio() %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem and generate portfolio
#' s1 <- solve(p1)
#'
#' # print number of solutions found
#' print(length(s1))
#'
#' # plot solutions
#' plot(stack(s1), axes = FALSE, box = FALSE)
#'
#' # create multi-zone problem with a portfolio for extra solutions
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_extra_portfolio() %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem and generate portfolio
#' s2 <- solve(p2)
#'
#' # print number of solutions found
#' print(length(s2))
#'
#' # plot solutions in portfolio
#' plot(stack(lapply(s2, category_layer)), main = "solution", axes = FALSE,
#'      box = FALSE)
#' }
#' @name add_extra_portfolio
NULL

#' @export
methods::setClass("ExtraPortfolio", contains = "Portfolio")

#' @rdname add_extra_portfolio
#' @export
add_extra_portfolio <- function(x) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # check that version 8.0.0 or greater of gurobi is installed
  if (!requireNamespace("gurobi", quietly = TRUE))
    stop(paste("the gurobi R package is required to generate solutions ",
               "using this portfolio method"))
  if (utils::packageVersion("gurobi") < as.package_version("8.0.0"))
    stop(paste("version 8.0.0 (or greater) of the gurobi package is required ",
               "to generate solution using this portfolio method"))
  # add portfolio
  x$add_portfolio(pproto(
    "ExtraPortfolio",
    Portfolio,
    name = "Extra portfolio",
    run = function(self, x, solver) {
      ## check that problems has gurobi solver
      if (!inherits(solver, "GurobiSolver"))
        stop(paste("add_gurobi_solver must be used to solve problems",
                   "with portfolio method"))
      ## solve problem
      sol <- solver$solve(x, PoolSearchMode = 1)
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
