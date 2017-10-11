#' @include Portfolio-proto.R
NULL

#' Add a Bender's cuts portfolio
#'
#' Generate a portfolio of solutions using Bender's cuts.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param number_solutions \code{integer} number of attempts to generate
#'   different solutions. Defaults to 10.
#'
#' @details This strategy for generating a portfolio of solutions involves
#'   solving the problem multiple times and adding additional constraints
#'   to forbid previously obtained solutions. In general, this strategy is most
#'   useful when problems take a long time to solve and benefit from
#'   having multiple threads allocated for solving an individual problem.
#'
#' @seealso \code{\link{portfolios}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_min_set_objective() %>%
#'      add_relative_targets(0.1) %>%
#'      add_binary_decisions() %>%
#'      add_cuts_portfolio(10) %>%
#'      add_default_solver(gap = 0.2, verbose = FALSE)
#'
#' \donttest{
#' # solve problem and generate 10 solutions within 20 % of optimality
#' s <- solve(p)
#'
#' # plot solution
#' plot(s, axes = FALSE, box = FALSE)
#' }
#'
#' @name add_cuts_portfolio
NULL

#' @export
methods::setClass("CutsPortfolio", contains = "Portfolio")

#' @rdname add_cuts_portfolio
#' @export
add_cuts_portfolio <- function(x, number_solutions = 10L) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          assertthat::is.count(number_solutions),
                          isTRUE(all(is.finite(number_solutions))))
  # add solver
  x$add_portfolio(pproto(
    "CutsPortfolio",
    Portfolio,
    name = "Cuts portfolio",
    parameters = parameters(
      integer_parameter("number_solutions", number_solutions,
                        lower_limit = 1L)),
    run = function(self, x, solver) {
      ## attempt initial solution for problem
      sol <- solver$solve(x)
      # if solving the problem failed then return NULL
      if (is.null(sol))
        return(sol)
      ## generate additional solutions
      sol <- list(sol)
      for (i in seq_len(self$parameters$get("number_solutions") - 1) + 1) {
        # add cuts
        rcpp_forbid_solution(x$ptr, sol[[i - 1]][[1]])
        # solve solution
        curr_sol <- solver$solve(x)
        # if contains valid solution then
        if(!is.null(curr_sol$x)) {
          sol[[i]] <- curr_sol
        } else {
          if ((i + 1) < self$parameters$get("number_solutions"))
            warning(paste("there are only", length(sol),
                          "feasible solutions within the optimality gap"))
          break()
        }
      }
      ## compile results
      return(sol)
    }
))}
