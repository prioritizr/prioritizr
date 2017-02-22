#' @include internal.R 
NULL

#' Add feature weights
#' 
#' Conservation planning problems that aim to maximize the representation of 
#  features in a solution given a budget may not be able to conserve all 
#' features the budget is not high enough. In such budget-limited problems,
#' it may be desirable to prefer the representation of some features
#' over other features. Feature weights can be be applied to a conservation
#' planning problem to favour the representation of some features over other 
#' other features when making decisions about how the budget should be 
#' allocated.
#' 
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param weights \code{numeric} weights. Higher values for a feature 
#'   indicate that it is more desireable to represent the feature in a network. 
#'   Weights cannot have negative values.
#'
#' @details Weights can only be applied to a budget-limited 
#'   type of planning problem (ie. \code{\link{add_maximum_coverage_objective}},
#'   and \code{\link{add_maximum_representation_objective}}.
#'
#' @return \code{\link{ConservationProblem}} object with the weights
#'   added to it.
#'
#' @seealso \code{\link{targets}}.
#'
#' @examples
#' # create problem
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'   add_maximum_coverage_objective(budget=5000)
#' 
#' # create weights based on rarity (1/number occurrences)
#' w <- 1/raster::cellStats(sim_features, 'sum') * 100
#'
#' # create new problem with added weights according to rarity
#' p2 <- p1 %>% add_feature_weights(w)
#'
#' # solve solutions
#' s <- stack(solve(p1), solve(p2))
#'
#' # plot solutions
#' plot(s, main = c('equal weights', 'rarity weights'))
#'
#' @export
add_feature_weights <- function(x, weights) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, 'ConservationProblem'),
    inherits(weights, 'numeric'), isTRUE(all(is.finite(weights))), 
    isTRUE(all(weights >= 0.0)), isTRUE(length(weights) > 0),
    length(weights) == x$number_of_features())
  # create weight parameters
  weights <- numeric_parameter_array('weights', weights, x$feature_names(),
    lower_limit = rep(0, x$number_of_features()))
  # add targets to problem
  x$add_constraint(pproto(
    'FeatureWeights',
    Constraint,
    name='Feature weights',
    parameters=parameters(weights),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, 'OptimizationProblem'),
        inherits(y, 'ConservationProblem'))
      invisible(rcpp_apply_feature_weights(x$ptr,
        self$parameters$get('weights')[[1]]))
    }))
}
