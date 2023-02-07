#' @include Portfolio-proto.R
NULL

#' Add an extra portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' [problem()] by storing feasible solutions
#' discovered during the optimization process.
#' This method is useful for quickly obtaining multiple solutions,
#' but does not provide any guarantees on the number of solutions, or
#' the quality of solutions.
#'
#' @param x [problem()] object.
#'
#' @details This strategy for generating a portfolio requires problems to
#'   be solved using the *Gurobi* software suite (i.e., using
#'   [add_gurobi_solver()]. Specifically, version 8.0.0 (or greater)
#'   of the \pkg{gurobi} package must be installed.
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
#' sim_pu_zones_raster <- get_sim_zones_pu_raster()
#' sim_features_zones <- get_sim_zones_features()
#'
#' # create minimal problem with a portfolio for extra solutions
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.05) %>%
#'   add_extra_portfolio() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem and generate portfolio
#' s1 <- solve(p1)
#'
#' # convert portfolio into a multi-layer raster object
#' s1 <- terra::rast(s1)
#'
#' # print number of solutions found
#' print(terra::nlyr(s1))
#'
#' # plot solutions
#' plot(s1, axes = FALSE)
#'
#' # create multi-zone problem with a portfolio for extra solutions
#' p2 <-
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_extra_portfolio() %>%
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
#' @name add_extra_portfolio
NULL

#' @rdname add_extra_portfolio
#' @export
add_extra_portfolio <- function(x) {
  # assert that arguments are valid
  rlang::check_required(x)
  assert(is_conservation_problem(x))
  # assert dependencies available
  assert(is_installed("gurobi"))
  assert(
    utils::packageVersion("gurobi") >= as.package_version("8.0.0"),
    msg = paste(
      "This portfolio requires version 8.0.0 (or greater) of the",
      "Gurobi software."
    )
  )
  # add portfolio
  x$add_portfolio(pproto(
    "ExtraPortfolio",
    Portfolio,
    name = "extra portfolio",
    run = function(self, x, solver) {
      ## check that problem has gurobi solver
      assert(
        inherits(solver, "GurobiSolver"),
        call = rlang::expr(add_gap_portfolio()),
        msg = "The solver must be specified using {.fn add_gurobi_solver}."
      )
      ## solve problem
      sol <- solver$solve(x, PoolSearchMode = 1)
      ## compile results
      if (!is.null(sol$pool)) {
        sol <- append(
          list(sol[-5]),
          lapply(
            sol$pool,
            function(z) list(
              x = z$xn,
              objective = z$objval,
              status = z$status,
              runtime = sol$runtime
            )
          )
        )
      } else {
       sol <- list(sol)
      }
      ## return solution
      return(sol)
    }
  ))
}
