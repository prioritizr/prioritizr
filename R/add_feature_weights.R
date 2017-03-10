#' @include internal.R
NULL

#' Add feature weights
#'
#' Conservation planning problems that aim to maximize the representation of
#  features may not be able to conserve all of them if the budget is not high
#' enough. In such budget-limited problems, it may be desirable to prefer the
#' representation of some features over others. Weights can be be applied to
#' a problem to favor the representation of some features over others when
#' making decisions about how the budget should be allocated.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param weights \code{numeric} weights. Features with higher weights indicate
#'   that it is more desirable to represent them in the solution. Weights
#'   cannot have negative values.
#'
#' @details Weights can only be applied to a budget-limited
#'   type of planning problem (ie. \code{\link{add_max_cover_objective}},
#'   and \code{\link{add_max_features_objective}}. Weights can also
#'   be applied to problems that aim to maximize phylogenetic representation
#'   (\code{\link{add_max_phylo_objective}}) to favor the
#'   representation of specific features over the representation of
#'   some phylogenetic branches.
#'
#' @return \code{\link{ConservationProblem-class}} object with the weights
#'   added to it.
#'
#' @seealso \code{\link{targets}}.
#'
#' @examples
#' # create problem
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'   add_max_cover_objective(budget=5000)
#'
#' # create weights based on rarity (1/number occurrences)
#' w <- 1 / cellStats(sim_features, "sum") * 1000
#'
#' # create new problem with added weights according to rarity
#' p2 <- p1 %>% add_feature_weights(w)
#'
#' \donttest{
#' # solve solutions
#' s <- stack(solve(p1), solve(p2))
#'
#' # plot solutions
#' plot(s, main = c("equal weights", "rarity weights"))
#' }
#'
#' @export
add_feature_weights <- function(x, weights) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    inherits(weights, "numeric"), isTRUE(all(is.finite(weights))),
    isTRUE(all(weights >= 0.0)), isTRUE(length(weights) > 0),
    length(weights) == x$number_of_features())
  # create weight parameters
  weights <- numeric_parameter_array("weights", weights, x$feature_names(),
    lower_limit = rep(0, x$number_of_features()))
  # add targets to problem
  x$add_constraint(pproto(
    "FeatureWeights",
    Constraint,
    name = "Feature weights",
    parameters = parameters(weights),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_feature_weights(x$ptr,
        self$parameters$get("weights")[[1]]))
    }))
}
