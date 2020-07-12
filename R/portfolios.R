#' @include internal.R Collection-proto.R
NULL

#' Solution portfolios
#'
#' Conservation planners often desire a portfolio of solutions
#' to present to decision makers. This is because conservation planners
#' often do not have access to "perfect" information, such as cost data that
#' accurately reflects stakeholder preferences, and so having multiple
#' near-optimal solutions can be a useful.
#'
#' @details All methods for generating portfolios will return solutions that
#'   are within the specified optimality gap.
#'
#'   The following portfolios can be added to a conservation planning
#'   [problem()]:
#'
#'   \describe{
#'
#'   \item{`add_default_portfolio`}{Generate a single solution.}
#'
#'   \item{[add_extra_portfolio()]}{Generate a portfolio of solutions
#'     by storing feasible solutions found during the optimization
#'     process. This method is useful for quickly obtaining multiple solutions,
#'     but does not provide any guarantees on the number of solutions, or
#'     the quality of solutions.
#'     Note that it requires the *Gurobi* solver.}
#'
#'   \item{[add_top_portfolio()]}{Generate a portfolio of
#'     solutions by finding a pre-specified number of solutions that
#'     are closest to optimality (i.e the top solutions). This is useful
#'     for examining differences among near-optimal solutions.
#'     It can also be used to generate multiple solutions and, in turn,
#'     to calculate selection frequencies for small problems.
#'     Note that it requires the *Gurobi* solver.}
#'
#'   \item{[add_gap_portfolio()]}{Generate a portfolio of solutions
#'     by finding a certain number of solutions that are all within a pre-
#'     specified optimality gap. This method is useful for generating
#'     multiple solutions that can be used to calculate selection frequencies
#'     for moderate and large-sized problems (similar to *Marxan*).
#'     Note that it requires the *Gurobi* solver.}
#'
#'   \item{[add_cuts_portfolio()]}{Generate a portfolio of distinct
#'     solutions within a pre-specified optimality gap using Bender's cuts.
#'     This is recommended as a replacement for [add_top_portfolio()]
#'     when the *Gurobi* software is not available.}
#
#'   \item{[add_shuffle_portfolio()]}{Generate a portfolio of
#'     solutions by randomly reordering the data prior to attempting to solve
#'     the problem.
#'     This is recommended as a replacement for [add_gap_portfolio()]
#'     when the *Gurobi* software is not available.}
#'
#' }
#'
#' @seealso [constraints],  [decisions],
#'  [objectives] [penalties], [problem()],
#'  [solvers], [targets].
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
#'      add_default_solver(gap = 0.02, verbose = FALSE)
#'
#' # create problem with cuts portfolio with 4 solutions
#' p1 <- p %>% add_cuts_portfolio(4)
#'
#' # create problem with shuffle portfolio with 4 solutions
#' p2 <- p %>% add_shuffle_portfolio(4)
#' \donttest{
#' # create problem with extra portfolio
#' p3 <- p %>% add_extra_portfolio()
#'
#' # create problem with top portfolio with 4 solutions
#' p4 <- p %>% add_top_portfolio(4)
#'
#' # create problem with gap portfolio with 4 solutions within 50% of optimality
#' p5 <- p %>% add_gap_portfolio(4, 0.5)
#'
#' # solve problems and create solution portfolios
#' s <- list(solve(p1), solve(p2), solve(p3), solve(p4), solve(p5))
#'
#' # plot solutions from extra portfolio
#' plot(stack(s[[1]]), axes = FALSE, box = FALSE)
#'
#' # plot solutions from top portfolio
#' plot(stack(s[[2]]), axes = FALSE, box = FALSE)
#'
#' # plot solutions from gap portfolio
#' plot(stack(s[[3]]), axes = FALSE, box = FALSE)
#'
#' # plot solutions from cuts portfolio
#' plot(stack(s[[4]]), axes = FALSE, box = FALSE)
#'
#' # plot solutions from shuffle portfolio
#' plot(stack(s[[5]]), axes = FALSE, box = FALSE)
#'

#' }
#' @name portfolios
NULL
