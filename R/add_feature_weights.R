#' @include internal.R
NULL

#' Add feature weights
#'
#' Conservation planning problems that aim to maximize the representation of
#' features may not be able to conserve all of them if the budget is not high
#' enough. In such budget-limited problems, it may be desirable to prefer the
#' representation of some features over other features. Weights can be
#' applied to a problem to favor the representation of some features over other
#' features when making decisions about how the budget should be allocated.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param weights \code{numeric} weights. Features with higher weights indicate
#'   that it is more desirable to represent them in the solution. Weights
#'   cannot have negative values.
#'
#' @details Weights can only be applied to a budget-limited
#'   type of planning problem (i.e. \code{\link{add_max_cover_objective}},
#'   and \code{\link{add_max_features_objective}}. Weights can also
#'   be applied to problems that aim to maximize phylogenetic representation
#'   (\code{\link{add_max_phylo_objective}}) to favor the
#'   representation of specific features over the representation of
#'   some phylogenetic branches.
#'
#' @return \code{\link{ConservationProblem-class}} object with the weights
#'   added to it.
#'
#' @seealso \code{\link{targets}}, \code{\link{objectives}}.
#'
#' @examples
#' # create problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'      add_max_cover_objective(budget = 5000)
#'
#' # problem problem with features weighted according to rarity
#' # (1 / number occurrences)
#' w1 <- 1 / cellStats(sim_features, "sum") * 1000
#' p1 <- p %>% add_feature_weights(w1)
#'
#' # problem problem with features weighted according to manually specified
#' # weights
#' w2  <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' p2 <- p %>% add_feature_weights(w2)
#' \donttest{
#' # solve solutions
#' s <- stack(solve(p), solve(p1), solve(p2))
#'
#' # plot solutions
#' plot(s, main = c("equal weights", "rarity weights", "manual weights"),
#'      axes = FALSE, box = FALSE)
#' }
#'
#' @export
add_feature_weights <- function(x, weights) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    is.numeric(weights), isTRUE(all(is.finite(weights))),
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
