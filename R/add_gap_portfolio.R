#' @include Portfolio-proto.R
NULL

#' Add a gap portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' [problem()] by finding a certain number of solutions that
#' are all within a pre-specified optimality gap. This method is useful for
#' generating multiple solutions that can be used to calculate selection
#' frequencies for moderate and large-sized problems (similar to
#' *Marxan*).
#'
#' @param x [problem()] (i.e. [`ConservationProblem-class`]) object.
#'
#' @param number_solutions `integer` number of solutions required.
#'
#' @param pool_gap `numeric` gap to optimality for solutions in the portfolio.
#'  This relative gap specifies a threshold worst-case performance for
#'  solutions in the portfolio. For example, value of 0.1 will result in the
#'  portfolio returning solutions that are within 10% of an optimal solution.
#'  Note that the gap specified in the solver (i.e.
#'  [add_gurobi_solver()] must be less than or equal to the gap
#'  specified to generate the portfolio. Defaults to 0.1.
#'
#' @details This strategy for generating a portfolio requires problems to
#'   be solved using the *Gurobi* software suite (i.e. using
#'   [add_gurobi_solver()]. Specifically, version 9.0.0 (or greater)
#'   of the \pkg{gurobi} package must be installed.
#'   Note that the number of solutions returned may be less than the argument to
#'   `number_solutions`, if the total number of solutions that
#'   meet the optimality gap is less than the number of solutions requested.
#'
#' @inherit add_gap_portfolio details
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
#' # create minimal problem with a portfolio containing 10 solutions within 20%
#' # of optimality
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.05) %>%
#'       add_gap_portfolio(number_solutions = 5, pool_gap = 0.2) %>%
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
#' # create multi-zone  problem with a portfolio containing 10 solutions within
#' # 20% of optimality
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_gap_portfolio(number_solutions = 5, pool_gap = 0.2) %>%
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
#' @name add_gap_portfolio
NULL

#' @rdname add_gap_portfolio
#' @export
add_gap_portfolio <- function(x, number_solutions, pool_gap = 0.1) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    assertthat::is.count(number_solutions), assertthat::noNA(number_solutions),
    assertthat::is.number(pool_gap), assertthat::noNA(pool_gap), isTRUE(pool_gap >= 0))
  # check that version 8.0.0 or greater of gurobi is installed
  if (!requireNamespace("gurobi", quietly = TRUE))
    stop(paste("the gurobi R package is required to generate solutions ",
               "using this portfolio method"))
  if (utils::packageVersion("gurobi") < as.package_version("8.0.0"))
    stop(paste("version 8.0.0 (or greater) of the gurobi package is required ",
               "to generate solution using this portfolio method"))
  # add portfolio
  x$add_portfolio(pproto(
    "GapPortfolio",
    Portfolio,
    name = "Gap portfolio",
    parameters = parameters(
      integer_parameter("number_solutions", number_solutions,
                        lower_limit = 1L),
      numeric_parameter("pool_gap", pool_gap, lower_limit = 0)),
    run = function(self, x, solver) {
      ## check that problems has gurobi solver
      if (!inherits(solver, "GurobiSolver"))
        stop(paste("add_gurobi_solver must be used to solve problems",
                   "with portfolio method"))
      ## check that solver gap <= portfolio gap
      assertthat::assert_that(
        solver$parameters$get("gap") <= self$parameters$get("pool_gap"),
        msg = "solver gap not smaller than or equal to portfolio gap")
      ## solve problem, and with gap of zero
      sol <- solver$solve(x, PoolSearchMode = 2,
        PoolSolutions = self$parameters$get("number_solutions"),
        PoolGap = self$parameters$get("pool_gap"))
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
