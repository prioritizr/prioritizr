#' @include Portfolio-class.R
NULL

#' Add a gap portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' problem by finding a certain number of solutions that
#' are all within a pre-specified optimality gap. This method is useful for
#' generating multiple solutions that can be used to calculate selection
#' frequencies for moderate and large-sized problems (similar to
#' *Marxan*).
#'
#' @param x [problem()] object.
#'
#' @param number_solutions `integer` number of solutions required.
#'   Defaults to 10.
#'
#' @param pool_gap `numeric` gap to optimality for solutions in the portfolio.
#'  This relative gap specifies a threshold worst-case performance for
#'  solutions in the portfolio. For example, value of 0.1 will result in the
#'  portfolio returning solutions that are within 10% of an optimal solution.
#'  Note that the gap specified in the solver (i.e.,
#'  [add_gurobi_solver()] must be less than or equal to the gap
#'  specified to generate the portfolio. Defaults to 0.1.
#'
#' @details This strategy for generating a portfolio requires problems to
#'   be solved using the *Gurobi* software suite (i.e., using
#'   [add_gurobi_solver()]. Specifically, version 9.0.0 (or greater)
#'   of the \pkg{gurobi} package must be installed.
#'   Note that the number of solutions returned may be less than the argument to
#'   `number_solutions`, if the total number of solutions that
#'   meet the optimality gap is less than the number of solutions requested.
#'   Also, note that this portfolio function only works with problems
#'   that have binary decisions (i.e., specified using
#'   [add_binary_decisions()]).
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
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create minimal problem with a portfolio containing 10 solutions within 20%
#' # of optimality
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.05) %>%
#'   add_gap_portfolio(number_solutions = 5, pool_gap = 0.2) %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem and generate portfolio
#' s1 <- solve(p1)
#'
#' # convert portfolio into a multi-layer raster
#' s1 <- terra::rast(s1)
#'
#' # print number of solutions found
#' print(terra::nlyr(s1))
#'
#' # plot solutions
#' plot(s1, axes = FALSE)
#'
#' # create multi-zone  problem with a portfolio containing 10 solutions within
#' # 20% of optimality
#' p2 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_gap_portfolio(number_solutions = 5, pool_gap = 0.2) %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem and generate portfolio
#' s2 <- solve(p2)
#'
#' # convert portfolio into a multi-layer raster of category layers
#' s2 <- terra::rast(lapply(s2, category_layer))
#'
#' # print number of solutions found
#' print(terra::nlyr(s2))
#'
#' # plot solutions in portfolio
#' plot(s2, axes = FALSE)
#' }
#' @name add_gap_portfolio
NULL

#' @rdname add_gap_portfolio
#' @export
add_gap_portfolio <- function(x, number_solutions = 10, pool_gap = 0.1) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(number_solutions)
  assert_required(pool_gap)
  assert(
    is_conservation_problem(x),
    assertthat::is.count(number_solutions),
    assertthat::noNA(number_solutions),
    assertthat::is.number(pool_gap),
    assertthat::noNA(pool_gap),
    pool_gap >= 0,
    is_installed("gurobi")
  )
  assert(
    utils::packageVersion("gurobi") >= as.package_version("8.0.0"),
    msg = paste(
      "{.fn add_gap_portfolio} requires version 8.0.0 (or greater)",
      "of the Gurobi software."
    )
  )
  # add portfolio
  x$add_portfolio(
    R6::R6Class(
      "GapPortfolio",
      inherit = Portfolio,
      public = list(
        name = "gap portfolio",
        data = list(
          number_solutions = number_solutions,
          pool_gap = pool_gap
        ),
        run = function(x, solver) {
          ## check that problems has gurobi solver
          assert(
            inherits(solver, "GurobiSolver"),
            call = rlang::expr(add_gap_portfolio()),
            msg = "The solver must be specified using {.fn add_gurobi_solver}."
          )
          ## check that solver gap <= portfolio gap
          assert(
            solver$get_data("gap") <= self$get_data("pool_gap"),
            call = rlang::expr(add_gap_portfolio()),
            msg = paste(
              "{.arg gap} for {.fn add_gurobi_solver} must be",
              "{cli::symbol$leq} than the {.arg gap} for",
              "{.fn add_gap_portfolio}."
            )
          )
          ## solve problem, and with gap of zero
          sol <- solver$solve(
            x,
            PoolSearchMode = 2,
            PoolSolutions = self$get_data("number_solutions"),
            PoolGap = self$get_data("pool_gap")
          )
          ## compile results
          if (!is.null(sol$pool)) {
            sol <- append(
              list(sol[-which(names(sol) == "pool")]),
              lapply(sol$pool, function(z) {
                list(
                  x = z$xn,
                  objective = z$objective,
                  status = z$status,
                  runtime = sol$runtime,
                  gap = z$gap,
                  objbound = z$objbound
                )
              })
            )
          } else {
           sol <- list(sol)
          }
          ## return solution
          return(sol)
        }
      )
    )$new()
  )
}
