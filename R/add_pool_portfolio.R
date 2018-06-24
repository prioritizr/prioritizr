#' @include Portfolio-proto.R
NULL

#' Add a pool portfolio
#'
#' Generate a portfolio of solutions for a conservation planning
#' \code{\link{problem}} by extracting all the feasible solutions
#' discovered during the optimization process.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param method \code{numeric} search method identifier that determines how
#'   multiple solutions should be generated. Available search modes for
#'   generating a portfolio of solutions include: \code{0}
#'   recording all solutions identified whilst trying to find
#'   a solution that is within the specified optimality gap, \code{1} finding
#'   one solution within the optimality gap and a number of additional
#'   solutions that are of any level of quality (such that the total number of
#'   solutions is equal to \code{number_solutions}), and \code{2} finding a
#'   specified number of solutions that are nearest to optimality. These
#'   search methods correspond to the parameters used by the \emph{Gurobi}
#'   software suite (see \url{http://www.gurobi.com/documentation/8.0/refman/poolsearchmode.html#parameter:PoolSearchMode}).
#'   Defaults to 0.
#'
#' @param number_solutions \code{integer} number of attempts to generate
#'   different solutions. Note that this argument has no effect if the
#'   argument to \code{method} is \code{0}. Defaults to 10.
#'
#' @details This strategy for generating a portfolio requires problems to
#'   be solved using the \emph{Gurobi} software suite (i.e. using
#'   \code{\link{add_gurobi_solver}}. Specifically, version 8.0.0 (or greater)
#'   of the \pkg{gurobi} package must be installed. \strong{Please note that
#'   although the solution pool methods are faster than the other methods
#'   for generating portfolios of solutions, none of the pool methods
#'   are guaranteed to return only solutions within a specified optimality gap.
#'   Also, except for when the \code{method} argument is set to 2, none of
#'   the search methods provide any guarantees on the number of returned
#'   solutions.}
#'
#' @inherit add_cuts_portfolio seealso return
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#' \donttest{
#' # create minimal problem with pool portfolio
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_pool_portfolio() %>%
#'       add_default_solver(gap = 0.02, verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print number of solutions found
#' print(length(s1))
#'
#' # plot solutions
#' plot(stack(s1), axes = FALSE, box = FALSE)
#'
#' # create minimal problem with pool portfolio and find the top 5 solutions
#' p2 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.5) %>%
#'       add_pool_portfolio(method = 2, number_solutions = 5) %>%
#'       add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # print number of solutions found
#' print(length(s2))
#'
#' # plot solutions
#' plot(stack(s2), axes = FALSE, box = FALSE)
#'
#' # build multi-zone conservation problem with pool portfolio
#' p3 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions() %>%
#'       add_pool_portfolio() %>%
#'       add_default_solver(gap = 0.02, verbose = FALSE)
#'
#' # solve the problem
#' s3 <- solve(p3)
#'
#' # print number of solutions found
#' print(length(s3))
#'
#' # print solutions
#' str(s3, max.level = 1)
#'
#' # plot solutions in portfolio
#' plot(stack(lapply(s3, category_layer)), main = "solution", axes = FALSE,
#'      box = FALSE)
#' }
#' @name add_pool_portfolio
NULL

#' @export
methods::setClass("PoolPortfolio", contains = "Portfolio")

#' @rdname add_pool_portfolio
#' @export
add_pool_portfolio <- function(x, method = 0, number_solutions = 10) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          assertthat::is.scalar(method),
                          assertthat::noNA(method),
                          assertthat::is.count(number_solutions),
                          assertthat::noNA(number_solutions),
                          isTRUE(method >= 0),
                          isTRUE(method <= 2))
  # check that version 8.0.0 or greater of gurobi is installed
  if (!requireNamespace("gurobi", quietly = TRUE))
    stop(paste("the gurobi R package is required to generate solutions ",
               "using the pool portfolio"))
  if (utils::packageVersion("gurobi") < as.package_version("8.0.0"))
    stop(paste("version 8.0.0 (or greater) of the gurobi package is required ",
               "to generate solution using the pool portfolio"))
  # add portfolio
  x$add_portfolio(pproto(
    "PoolPortfolio",
    Portfolio,
    name = "Pool portfolio",
    parameters = parameters(
      integer_parameter("method", method,
                        lower_limit = 0L, upper_limit = 2L),
      integer_parameter("number_solutions", number_solutions,
                        lower_limit = 1L)),
    run = function(self, x, solver) {
      ## check that problems has gurobi solver
      if (!inherits(solver, "GurobiSolver"))
        stop("problems must be solved using Gurobi to use the pool portfolio")
      ## solve problem
      sol <- solver$solve(x, PoolSearchMode = self$parameters$get("method"),
        PoolSolutions = self$parameters$get("number_solutions"))
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
