#' @include Portfolio-proto.R
NULL

#' Add a top portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' [problem()] by finding a pre-specified number of solutions that
#' are closest to optimality (i.e the top solutions).
#'
#' @param x [problem()] object.
#'
#' @param number_solutions `integer` number of solutions required.
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
#' sim_pu_zones_raster <- get_sim_pu_zones_raster()
#' sim_features <- get_sim_features()
#' sim_features_zones <- get_sim_features_zones()
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
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
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
add_top_portfolio <- function(x, number_solutions) {
  # assert that arguments are valid
  assertthat::assert_that(
    is_conservation_problem(x),
    assertthat::is.count(number_solutions),
    assertthat::noNA(number_solutions),
    is_installed("gurobi", "add_top_portfolio()")
  )
  # check that version 9.0.0 or greater of gurobi is installed
  assertthat::assert_that(
    utils::packageVersion("gurobi") >= as.package_version("8.0.0"),
    msg = paste(
      "add_top_portfolio() requires version 8.0.0 (or greater)",
      "of the Gurobi software"
    )
  )
  # add portfolio
  x$add_portfolio(pproto(
    "TopPortfolio",
    Portfolio,
    name = "Top portfolio",
    parameters = parameters(
      integer_parameter(
        "number_solutions", number_solutions, lower_limit = 1L
      )
    ),
    run = function(self, x, solver) {
      ## check that problems has gurobi solver
      assertthat::assert_that(
        inherits(solver, "GurobiSolver"),
        msg = "add_top_portfolio() requires use of add_gurobi_solver()"
      )
      ## solve problem, and with gap of zero
      sol <- solver$solve(
        x,
        PoolSearchMode = 2,
        PoolSolutions = self$parameters$get("number_solutions"),
        MIPGap = 0
      )
      ## compile results
      if (!is.null(sol$pool)) {
        sol <- append(
          list(sol[-5]),
          lapply(
            sol$pool,
            function(z) list(
              x = z$xn, objective = z$objval,
              status = z$status,
              runtime = sol$runtime
            )
          )
        )
      } else {
       sol <- list(sol)
      }
      ## return solution
      sol
    }
  ))
}
