#' @include internal.R pproto.R Objective-proto.R
NULL

#' Add Minimum Set Objective
#'
#' Set an objective to find the solution that fulfills all the targets and constraints for
#' the smallest cost. This objective is similar to that used in Marxan.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @details
#' A problem objective is used to specify the overall goal of the 
#' conservation planning problem. All conservation planning problems require 
#' adding objectives and targets, or solving will return an error.  
#'
#' @seealso \code{\link{objectives}}, \code{\link{constraints}}, \code{\link{problem}},
#'   \code{\link{targets}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features, sim_phylogeny)
#'
#' # create base problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'  add_relative_targets(0.1)
#'
#  # create problem with added minimum set objective
#' p1 <- p %>% add_min_set_objective()
#' # create problem with added maximum coverage objective
#' note that this objective does not use targets
#' p2 <- p %>% add_max_cover_objective(5000)
#'
#' # create problem with added maximum feature representation objective
#' p3 <- p %>% add_max_features_objective(5000)
#'
#' # create problem with added maximum phylogenetic representation objective
#' p4 <- p %>% add_max_phylo_objective(5000, sim_phylogeny)
#'
#' \donttest{
#' # solve problems
#' s <- stack(solve(p1), solve(p2), solve(p3), solve(p4))
#'
#' # plot solutions
#' plot(s, main=c("minimum set", "maximum coverage", "maximum representation",
#'                "phylogenetic representation"))
#' }
#'
#' @name add_min_set_objective
NULL

#' @rdname add_min_set_objective
#' @export
add_min_set_objective <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # add objective to problem
  x$add_objective(pproto(
    "MinimumSetObjective",
    Objective,
    name = "Minimum set objective",
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      invisible(rcpp_apply_min_set_objective(x$ptr, y$feature_targets(),
                                             y$planning_unit_costs()))
    }))
}
