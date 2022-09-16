#' @include Portfolio-proto.R
NULL

#' Add a top portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' [problem()] by finding a pre-specified number of solutions that
#' are closest to optimality (i.e the top solutions).
#'
#' @param x [problem()] (i.e., [`ConservationProblem-class`]) object.
#'
#' @param number_solutions `integer` number of solutions required.
#'
#' @details This strategy for generating a portfolio requires problems to
#'   be solved using the *Gurobi* software suite (i.e., using
#'   [add_gurobi_solver()]. Specifically, version 9.0.0 (or greater)
#'   of the \pkg{gurobi} package must be installed.
#'   Note that the number of solutions returned may be less than the argument to
#'   `number_solutions`, if the total number of feasible solutions
#'   is less than the number of solutions requested.
#'
#' @inherit add_cuts_portfolio return
#'
#' @seealso
#' See [portfolios] for an overview of all functions for adding a portfolio.
#'
#' @family portfolios
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create minimal problem with a portfolio for the top 5 solutions
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.05) %>%
#'       add_top_portfolio(number_solutions = 5) %>%
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
#' # create multi-zone problem with a portfolio for the top 5 solutions
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_top_portfolio(number_solutions = 5) %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem and generate portfolio
#' s2 <- solve(p2)
#'
#' # print number of solutions found
#' print(length(s2))
#'
#' # plot solutions in portfolio
#' plot(stack(lapply(s2, category_layer)),
#'      main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_top_portfolio
NULL

#' @rdname add_top_portfolio
#' @export
add_top_portfolio <- function(x, number_solutions) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    assertthat::is.count(number_solutions), assertthat::noNA(number_solutions))
  # check that version 8.0.0 or greater of gurobi is installed
  if (!requireNamespace("gurobi", quietly = TRUE))
    stop(paste("the \"gurobi\" package is required to generate solutions ",
               "using this portfolio method"))
  if (utils::packageVersion("gurobi") < as.package_version("8.0.0"))
    stop(paste("version 8.0.0 (or greater) of the Gurobi software is required ",
               "to generate solution using this portfolio method"))
  # add portfolio
  x$add_portfolio(pproto(
    "TopPortfolio",
    Portfolio,
    name = "Top portfolio",
    parameters = parameters(
      integer_parameter("number_solutions", number_solutions,
                        lower_limit = 1L)),
    run = function(self, x, solver) {
      ## check that problems has gurobi solver
      if (!inherits(solver, "GurobiSolver"))
        stop(paste("add_gurobi_solver must be used to solve problems",
                   "with portfolio method"))
      ## solve problem, and with gap of zero
      sol <- solver$solve(x, PoolSearchMode = 2,
        PoolSolutions = self$parameters$get("number_solutions"), MIPGap = 0)
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
