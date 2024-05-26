#' @include Portfolio-class.R
NULL

#' Add a top portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' problem by finding a pre-specified number of solutions that
#' are closest to optimality (i.e, the top solutions).
#'
#' @param x [problem()] object.
#'
#' @param number_solutions `integer` number of solutions required.
#'   Defaults to 10.
#'
#' @details This strategy for generating a portfolio requires problems to
#'   be solved using the *Gurobi* software suite (i.e., using
#'   [add_gurobi_solver()]. Specifically, version 8.0.0 (or greater)
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
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create minimal problem with a portfolio for the top 5 solutions
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.05) %>%
#'   add_top_portfolio(number_solutions = 5) %>%
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
#' # create multi-zone problem with a portfolio for the top 5 solutions
#' p2 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_top_portfolio(number_solutions = 5) %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem and generate portfolio
#' s2 <- solve(p2)
#'
#' # convert each solution in the portfolio into a single category layer
#' s2 <- terra::rast(lapply(s2, category_layer))
#'
#' # print number of solutions found
#' print(terra::nlyr(s2))
#'
#' # plot solutions in portfolio
#' plot(s2, axes = FALSE)
#' }
#' @name add_top_portfolio
NULL

#' @rdname add_top_portfolio
#' @export
add_top_portfolio <- function(x, number_solutions = 10) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(number_solutions)
  assert(
    is_conservation_problem(x),
    assertthat::is.count(number_solutions),
    assertthat::noNA(number_solutions),
    is_installed("gurobi")
  )
  # check that version 9.0.0 or greater of gurobi is installed
  assert(
    utils::packageVersion("gurobi") >= as.package_version("8.0.0"),
    msg =
      "Version 8.0.0 (or greater) of the Gurobi software must be installed."
  )
  # add portfolio
  x$add_portfolio(
    R6::R6Class(
      "TopPortfolio",
      inherit = Portfolio,
      public = list(
        name = "top portfolio",
        data = list(number_solutions = number_solutions),
        run = function(x, solver) {
          ## check that problems has gurobi solver
          assert(
            inherits(solver, "GurobiSolver"),
            call = rlang::expr(add_gap_portfolio()),
            msg = "The solver must be specified using {.fn add_gurobi_solver}."
          )
          ## solve problem, and with gap of zero
          sol <- solver$solve(
            x,
            PoolSearchMode = 2,
            PoolSolutions = self$get_data("number_solutions"),
            MIPGap = 0
          )
          ## compile results
          if (!is.null(sol$pool)) {
            sol <- append(
              list(sol[-which(names(sol) == "pool")]),
              lapply(
                sol$pool,
                function(z) list(
                  x = z$xn,
                  objective = z$objective,
                  status = z$status,
                  runtime = sol$runtime,
                  gap = z$gap
                )
              )
            )
          } else {
           sol <- list(sol)
          }
          ## return solution
          sol
        }
      )
    )$new()
  )
}
