#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add maximum coverage objective
#'
#' Set an objective to find the solution aims to represent one instance of
#' as many features as possible within a given budget. This type of objective
#' does not require the addition of targets. The mathematical formulation
#' underpinning this function is different from versions prior to 3.0.0.0;
#' see Details section for more information.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param budget \code{numeric} value specifying the maximum expenditure of
#' the prioritization.
#'
#' @details
#' A problem objective is used to specify the overall goal of the
#' conservation planning problem. Please note that \strong{all conservation
#' planning problems formulated in the prioritizr package require the addition
#' of objectives}. Failing to do so will return a default error message
#' when solving.
#'
#' The maximum coverage problem seeks to find the set of planning units that
#' maximizes the number of represented features, while keeping cost within a
#' fixed budget. Here, features are treated as being represented if
#' the reserve system contains any non-zero amount of the feature
#' (specifically, an amount greater than 1). One
#' situation where these problem formulation could be useful is when dealing
#' with binary biodiversity data which indicate the presence of suitable
#' habitat. Check out the \code{\link{add_max_features_objective}} for a more
#' generalized formulation which can accommodate user-specified representation
#' targets.
#'
#' In versions prior to 3.0.0.0, this objective function was implemented a
#' different mathematical formulation. This formulation is based on the
#' historical maximum coverage reserve selection formulation (Church & Velle
#' 1974; Church \emph{et al.} 1996). To access the formulation used in versions
#' prior to 3.0.0.0, please see the \code{\link{add_max_utility_objective}}
#' function.
#'
#' The maximum coverage objective for the reserve design problem can be
#' expressed mathematically for a set of planning units (\eqn{I}{I} indexed by
#' \eqn{i}{i}) and a set of features (\eqn{J}{J} indexed by \eqn{j}{j}) as:
#'
#' \deqn{\mathit{Maximize} \space \sum_{i = 1}^{I} s \space c_i +
#' \sum_{j = 1}^{J} y_j w_j \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i r_{ij} >= y_j \times 1 \forall j \in J \\
#' \sum_{i = 1}^{I} x_i c_i \leq B}{
#' Maximize sum_i^I (s * ci) + sum_j^J (yj * wj) subject to
#' sum_i^I (xi * rij) >= (yj * 1) for all j in J &
#' sum_i^I (xi * ci) <= B}
#'
#' Here, \eqn{x_i}{xi} is the \code{\link{decisions}} variable (e.g.
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ij}}{rij} is the amount of feature \eqn{j}{j} in planning unit
#' \eqn{i}{i}, \eqn{y_j}{yj} indicates if the solution has meet
#' the target \eqn{t_j}{tj} for feature \eqn{j}{j}, and \eqn{w_j}{wj} is the
#' weight for feature \eqn{j}{j} (defaults to 1 for all features; see
#' \code{\link{add_feature_weights}} to specify weights). Additionally,
#' \eqn{B}{B} is the budget allocated for the solution, \eqn{c_i}{ci} is the
#' cost of planning unit \eqn{i}{i}, and \eqn{s}{s} is a scaling factor used to
#' shrink the costs so that the problem will return a cheapest solution when
#' there are multiple solutions that represent the same amount of all features
#' within the budget.
#'
#' Note that this formulation is functionally equivalent to the
#' \code{\link{add_max_features_objective}} function with absolute targets
#' set to 1.
#'
#' @references
#' Church RL and Velle CR (1974) The maximum covering location problem.
#' \emph{Regional Science}, 32: 101--118.
#'
#' Church RL, Stoms DM, and Davis FW (1996) Reserve selection as a maximum
#' covering location problem. \emph{Biological Conservation}, 76: 105--112.
#'
#' @seealso \code{\link{add_feature_weights}}, \code{\link{objectives}}.
#'
#' @examples
#' # # not implemented
#' # # load data
#' # data(sim_pu_raster, sim_features)
#' #
#' # # set a 90th percentile threshold to the feature data
#' # sim_features_binary <- sim_features
#' # thresholds <- raster::quantile(sim_features, probs = 0.9, names = FALSE,
#' #                                na.rm = TRUE)
#' # for (i in seq_len(raster::nlayers(sim_features)))
#' #   sim_features_binary[[i]] <- as.numeric(raster::values(sim_features[[i]]) >
#' #                                          thresholds[[i]])
#' #
#' # # create problem
#' # p <- problem(sim_pu_raster, sim_features_binary) %>%
#' #      add_max_cover_objective(5000) %>%
#' #      add_binary_decisions()
#' # \donttest{
#' # # solve problem
#' # s <- solve(p)
#' #
#' # # plot solution
#' # plot(s, main = "solution", axes = FALSE, box = FALSE)
#' # }
#'
#' @name add_max_cover_objective
NULL

#' @rdname add_max_cover_objective
#' @export
add_max_cover_objective <- function(x, budget) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"),
                          isTRUE(all(is.finite(budget))),
                          assertthat::is.scalar(budget),
                          isTRUE(budget > 0.0))
  # make parameter
  p <- numeric_parameter("budget", budget, lower_limit = 0,
                         upper_limit = sum(x$planning_unit_costs()))
  # add objective to problem
  x$add_objective(pproto(
    "MaximumCoverageObjective",
    Objective,
    name = "Maximum coverage objective",
    parameters = parameters(p),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_max_cover_objective(x$ptr,
                y$planning_unit_costs(), self$parameters$get("budget")))
    }))
}
