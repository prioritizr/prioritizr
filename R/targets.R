#' @include internal.R pproto.R ConservationProblem-proto.R
NULL

#' Targets
#'
#' Targets are used to specify the minimum amount or proportion of a feature's
#' distribution that needs to be protected.
#'
#' Please note that failing to specify targets will return a default
#' error message when solving.The exception is the maximum cover problem
#' (see \code{\link{add_max_cover_objective}}), which maximizes all features
#' in the solution and therefore does not require targets.
#'
#' @details
#' The following list contains the targets can be added to a conservation
#' planning \code{\link{problem}}.
#'
#' \describe{
#'
#'   \item{\code{\link{add_relative_targets}}}{Set targets as a proportion
#'   (between 0 and 1) of the maximum level of representation of features in
#'   the study area.}
#'
#'   \item{\code{\link{add_absolute_targets}}}{Set targets expressed as the
#'   actual value of features in the study area that need to be represented in
#'   the prioritization.}
#'
#'   \item{\code{\link{add_loglinear_targets}}}{Set targets as a proportion
#'   (between 0 and 1) and calculated using a log-linear equation and four
#'   tuning parameters.}
#'
#'  }
#'
#' @seealso \code{\link{constraints}}, \code{\link{decisions}},
#'  \code{\link{objectives}} \code{\link{penalties}},
#'  \code{\link{portfolios}}, \code{\link{problem}},
#'  \code{\link{solvers}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create base problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_min_set_objective() %>%
#'      add_binary_decisions()
#'
#' # create problem with added relative targets
#' p1 <- p %>% add_relative_targets(0.1)
#'
#' # create problem with added absolute targets
#' p2 <- p %>% add_absolute_targets(3)
#'
#' # create problem with added loglinear targets
#' p3 <- p %>% add_loglinear_targets(10, 0.9, 100, 0.2)
#' \donttest{
#' # solve problem
#' s <- stack(solve(p1), solve(p2), solve(p3))
#'
#' # plot solution
#' plot(s, axes = FALSE, box = FALSE,
#'      main = c("relative targets", "absolute targets", "loglinear targets"))
#' }
#'
#' @name targets
NULL

add_default_targets <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # throw error because targets must be chosen by the user
  stop("problem is missing targets and they must be explicitly defined")
}
