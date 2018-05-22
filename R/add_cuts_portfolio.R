#' @include Portfolio-proto.R
NULL

#' Add Bender's cuts portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' \code{\link{problem}} using Bender's cuts (discussed in Rodrigues
#' \emph{et al.} 2000).
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
#'   having multiple threads allocated for solving an individual problem. If
#'   version 8.0.0 (or greater) of the \code{Gurobi} optimization software is
#'   used to solve the problems, then the solutions are obtained from the
#'   solution pool using the pool search mode designed to find different
#'   solutions (i.e. 2; see \url{http://www.gurobi.com/documentation/8.0/refman/poolsearchmode.html#parameter:PoolSearchMode}).
#'
#' @seealso \code{\link{portfolios}}.
#'
#' @return \code{\link{ConservationProblem-class}} object with the portfolio
#'   added to it.
#'
#' @references
#' Rodrigues AS, Cerdeira OJ, and Gaston KJ (2000) Flexibility,
#' efficiency, and accountability: adapting reserve selection algorithms to
#' more complex conservation problems. \emph{Ecography}, 23: 565--574.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#'
#' # create minimal problem with cuts portfolio
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_cuts_portfolio(10) %>%
#'       add_default_solver(gap = 0.2, verbose = FALSE)
#'
#' \donttest{
#' # solve problem and generate 10 solutions within 20 % of optimality
#' s1 <- solve(p1)
#'
#' # plot solutions in portfolio
#' plot(stack(s1), axes = FALSE, box = FALSE)
#' }
#' # build multi-zone conservation problem with cuts portfolio
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions() %>%
#'       add_cuts_portfolio(10) %>%
#'       add_default_solver(gap = 0.2, verbose = FALSE)
#'
#' \donttest{
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print solution
#' str(s2, max.level = 1)
#'
#' # plot solutions in portfolio
#' plot(stack(lapply(s2, category_layer)), main = "solution", axes = FALSE,
#'      box = FALSE)
#' }
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
  # add portfolio
  x$add_portfolio(pproto(
    "CutsPortfolio",
    Portfolio,
    name = "Cuts portfolio",
    parameters = parameters(
      integer_parameter("number_solutions", number_solutions,
                        lower_limit = 1L)),
    run = function(self, x, solver) {
      ## extract number of solutions
      n <- self$parameters$get("number_solutions")
      if (inherits(solver, "GurobiSolver") &&
          requireNamespace("gurobi", quietly = TRUE) &&
          utils::packageVersion("gurobi") >= as.package_version("8.0.0")) {
        ## generate portfolio using gurobi solution pool
        sol <- solver$solve(x, PoolSearchMode = 2, PoolSolutions = n)
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
        ## throw warning if number of solutions not equal to desired number
        if (length(sol) < n) {
          warning(paste("there are only", length(sol),
                        "feasible solutions within the optimality gap"))
        }
      } else {
        ## generate portfolio by explicitly adding bender's cut
        ### if solving the problem failed then return NULL
        sol <- solver$solve(x)
        if (is.null(sol))
          return(sol)
        ### generate additional solutions
        sol <- list(sol)
        for (i in seq_len(n - 1) + 1) {
          #### add cuts
          rcpp_forbid_solution(x$ptr, sol[[i - 1]][[1]])
          #### solve solution
          curr_sol <- solver$solve(x)
          #### if contains valid solution then
          if(!is.null(curr_sol$x)) {
            sol[[i]] <- curr_sol
          } else {
            if ((i + 1) < self$parameters$get("number_solutions"))
              warning(paste("there are only", length(sol),
                            "feasible solutions within the optimality gap"))
            break()
          }
        }
      }
      ## compile results
      return(sol)
    }
))}
