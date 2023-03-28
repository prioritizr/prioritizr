#' @include Portfolio-class.R
NULL

#' Add Bender's cuts portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' problem using Bender's cuts (discussed in Rodrigues
#' *et al.* 2000). This is recommended as a replacement for
#' [add_gap_portfolio()] when the *Gurobi* software is not
#' available.
#'
#' @param x [problem()] object.
#'
#' @param number_solutions `integer` number of attempts to generate
#'   different solutions. Defaults to 10.
#'
#' @details
#' This strategy for generating a portfolio of solutions involves
#' solving the problem multiple times and adding additional constraints
#' to forbid previously obtained solutions. In general, this strategy is most
#' useful when problems take a long time to solve and benefit from
#' having multiple threads allocated for solving an individual problem.
#
#' @section Notes:
#' In early versions (< 4.0.1), this function was only compatible with
#' *Gurobi* (i.e., [add_gurobi_solver()]). To provide functionality with
#' exact algorithm solvers, this function now adds constraints to the
#' problem formulation to generate multiple solutions.
#'
#' @return An updated [problem()] object with the portfolio added to it.
#'
#' @seealso
#' See [portfolios] for an overview of all functions for adding a portfolio.
#'
#' @family portfolios
#'
#' @references
#' Rodrigues AS, Cerdeira OJ, and Gaston KJ (2000) Flexibility,
#' efficiency, and accountability: adapting reserve selection algorithms to
#' more complex conservation problems. *Ecography*, 23: 565--574.
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create minimal problem with cuts portfolio
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_cuts_portfolio(10) %>%
#'   add_default_solver(gap = 0.2, verbose = FALSE)
#'
#' # solve problem and generate 10 solutions within 20% of optimality
#' s1 <- solve(p1)
#'
#' # convert portfolio into a multi-layer raster object
#' s1 <- terra::rast(s1)
#'
#' # plot solutions in portfolio
#' plot(s1, axes = FALSE)
#'
#' # build multi-zone conservation problem with cuts portfolio
#' p2 <-
#'  problem(sim_zones_pu_raster, sim_zones_features) %>%
#'  add_min_set_objective() %>%
#'  add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'  add_binary_decisions() %>%
#'  add_cuts_portfolio(10) %>%
#'  add_default_solver(gap = 0.2, verbose = FALSE)
#'
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print solution
#' str(s2, max.level = 1)
#'
#' # convert each solution in the portfolio into a single category layer
#' s2 <- terra::rast(lapply(s2, category_layer))
#'
#' # plot solutions in portfolio
#' plot(s2, main = "solution", axes = FALSE)
#' }
#' @name add_cuts_portfolio
NULL

#' @rdname add_cuts_portfolio
#' @export
add_cuts_portfolio <- function(x, number_solutions = 10) {
  # assert that arguments are valid
  assert_required(x)
  assert_required(number_solutions)
  assert(
    is_conservation_problem(x),
    assertthat::is.count(number_solutions),
    all_finite(number_solutions)
  )
  # add portfolio
  x$add_portfolio(
    R6::R6Class(
      "CutsPortfolio",
      inherit = Portfolio,
      public = list(
        name = "cuts portfolio",
        data = list(number_solutions = number_solutions),
        run = function(x, solver) {
          ## extract number of desired solutions
          n <- self$get_data("number_solutions")
          ## solve problem to verify that it is feasible
          sol <- solver$solve(x)
          ## if solving the problem failed then return NULL
          if (is.null(sol)) return(sol) # nocov
          ## generate additional solutions
          sol <- list(sol)
          for (i in seq_len(n - 1) + 1) {
            ### add cuts
            rcpp_forbid_solution(x$ptr, sol[[i - 1]][[1]])
            ### solve solution
            curr_sol <- solver$solve(x)
            ### if contains valid solution then
            if (!is.null(curr_sol$x)) {
              sol[[i]] <- curr_sol
            } else {
              break()
            }
          }
          ## compile results
          return(sol)
        }
      )
    )$new()
  )
}
