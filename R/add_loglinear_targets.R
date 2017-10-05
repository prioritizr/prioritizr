#' @include internal.R pproto.R ConservationProblem-proto.R
NULL

#' Add Loglinear Targets
#'
#' Set targets as a proportion (between 0 and 1) and calculated using a
#' log-linear  equation and four tuning parameters (as used in Rodrigues
#' \emph{et al.} 2004). The first tuning parameter specifies the first cut-off
#' range size, and the second specifies  the second cut-off range size, the
#' third argument specifies the target required  for species with a range size
#' equal to or less than the first cut-off range size,  and the fourth argument
#' specifies the target required for species with a range  size equal to or
#' greater than the required range size.
#'
#' Note that with the exception of the maximum cover problem, targets must
#' be added to a \code{\link{problem}} or solving will return an error.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param lower_bound_amount \code{numeric} lower bound for the total amount
#'   of the features.
#'
#' @param lower_bound_target \code{numeric} relative target that should be
#'   applied to features with a total amount that is less
#'  than or equal to \code{lower_bound_amount}.
#'
#' @param upper_bound_amount \code{numeric} upper bound for the total amount
#'   of features.
#'
#' @param upper_bound_target \code{numeric} relative target that should be
#'   applied to features with a total amount that is greater
#'   than or equal to \code{upper_bound_amount}.
#'
#' @param ... not used.
#'
#' @details
#' Targets are used to specify the minimum amount or proportion of a feature's
#' distribution that needs to be protected. All conservation planning problems
#' require adding targets with the exception of the maximum cover problem
#' (see \code{\link{add_max_cover_objective}}), which maximizes all features
#' in the solution and therefore does not require targets.
#'
#' @return \code{\link{ConservationProblem-class}} object with the target added
#'   to it.
#'
#' @seealso \code{\link{targets}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create basic problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective()
#'
#' # create problem with added relative targets
#' p1 <- p %>% add_relative_targets(0.1)
#'
#' # create problem with added absolute targets
#' p2 <- p %>% add_absolute_targets(3)
#'
#' # create problem with added log-linear target
#' p3 <- p %>% add_loglinear_targets(10, 0.9, 100, 0.2)
#'
#' \donttest{
#' # solve solutions
#' s <- stack(solve(p1), solve(p2), solve(p3))
#'
#' # plot solutions
#' plot(s, main=c("relative targets", "absolute targets",
#'                "log-linear targets"))
#' }
#'
#'
#' @name add_loglinear_targets
#'
#' @docType methods
NULL

#' @rdname add_loglinear_targets
#' @export
add_loglinear_targets <- function(x, lower_bound_amount,
                                  lower_bound_target,
                                  upper_bound_amount,
                                  upper_bound_target) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(lower_bound_amount))),
                          assertthat::is.scalar(lower_bound_amount),
                          isTRUE(lower_bound_amount >= 0),
                          isTRUE(all(is.finite(upper_bound_amount))),
                          assertthat::is.scalar(upper_bound_amount),
                          isTRUE(upper_bound_amount >= 0),
                          isTRUE(all(is.finite(lower_bound_target))),
                          assertthat::is.scalar(lower_bound_target),
                          isTRUE(lower_bound_target >= 0),
                          isTRUE(lower_bound_target <= 1),
                          isTRUE(all(is.finite(upper_bound_target))),
                          assertthat::is.scalar(upper_bound_target),
                          isTRUE(upper_bound_target >= 0),
                          isTRUE(upper_bound_target <= 1),
                          isTRUE(upper_bound_amount > lower_bound_amount))
  # create parameters
  p <- parameters(
    numeric_parameter("amount at lower bound", value = lower_bound_amount,
                      lower_limit = 0),
    numeric_parameter("amount at upper bound", value = upper_bound_amount,
                      lower_limit = 0),
    proportion_parameter("target at lower bound", value = lower_bound_target),
    proportion_parameter("target at upper bound", value = upper_bound_target))
  # add targets to problem
  x$add_targets(pproto(
    "LogLinearTargets",
    Target,
    name = "Log-linear targets",
    parameters = p,
    data = list(abundances = x$feature_abundances_in_planning_units()),
    output = function(self) {
      loglinear_interpolate(self$data$abundances,
                            self$parameters$get("amount at lower bound"),
                            self$parameters$get("target at lower bound"),
                            self$parameters$get("amount at upper bound"),
                            self$parameters$get("target at upper bound")) *
        self$get_data("abundances")
    }))
}
