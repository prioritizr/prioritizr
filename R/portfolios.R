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
#'   \code{\link{problem}}:
#'
#'   \describe{
#'
#'   \item{\code{add_default_portfolio}}{Generate a single solution.}
#'
#'   \item{\code{\link{add_cuts_portfolio}}}{Generate a portfolio of distinct
#'     solutions within a pre-specified optimality gap using Bender's cuts.}
#'
#'   \item{\code{\link{add_pool_portfolio}}}{Generate a portfolio of solutions
#'     by extracting all the feasible solutions discovered during the
#'     optimization process.}
#'
#'   \item{\code{\link{add_shuffle_portfolio}}}{Generate a portfolio of
#'     solutions by randomly reordering the data prior to attempting to solve
#'     the problem.}
#'
#' }
#'
#' @seealso \code{\link{constraints}},  \code{\link{decisions}},
#'  \code{\link{objectives}} \code{\link{penalties}}, \code{\link{problem}},
#'  \code{\link{solvers}}, \code{\link{targets}}.
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
#' # create problem with cuts portfolio
#' p1 <- p %>% add_cuts_portfolio(4)
#'
#' # create problem with shuffle portfolio
#' p2 <- p %>% add_shuffle_portfolio(4)
#' \donttest{
#' # create problem with pool portfolio
#' p3 <- p %>% add_pool_portfolio()
#'
#' # solve problems and create solution portfolios
#' s <- list(solve(p1), solve(p2), solve(p3))
#'
#' # plot solutions from cuts portfolio
#' plot(stack(s[[1]]), axes = FALSE, box = FALSE)
#'
#' # plot solutions from shuffle portfolio
#' plot(stack(s[[2]]), axes = FALSE, box = FALSE)
#'
#' # plot solutions from pool portfolio
#' plot(stack(s[[3]]), axes = FALSE, box = FALSE)
#' }
#' @name portfolios
NULL
