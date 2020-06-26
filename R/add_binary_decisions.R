#' @include internal.R Parameters-proto.R Decision-proto.R
NULL

#' Add binary decisions
#'
#' Add a binary decision to a conservation planning [problem()].
#' This is the classic decision of either prioritizing or not prioritizing a
#' planning unit. Typically, this decision has the assumed action of buying
#' the planning unit to include in a protected area network. If no decision is
#' added to a problem then this decision class will be used by default.
#'
#' @param x [ConservationProblem-class()] object.
#'
#' @details Conservation planning problems involve making decisions on planning
#'   units. These decisions are then associated with actions (e.g. turning a
#'   planning unit into a protected area). If no decision is explicitly added to
#'   a problem, then the binary decision class will be used by default. Only a
#'   single decision should be added to a `ConservationProblem` object.
#'   **If multiple decisions are added to a problem object, then the last
#'   one to be added will be used.**
#'
#' @return [ConservationProblem-class()] object with the decisions
#'   added to it.
#'
#' @seealso [decisions].
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#'
#' # create minimal problem with binary decisions
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution")
#' }
#' # build multi-zone conservation problem with binary decisions
#' p2 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' plot(category_layer(s2), main = "solution", axes = FALSE, box = FALSE)
#' }
#' @name add_binary_decisions
NULL

#' @rdname add_binary_decisions
#' @export
add_binary_decisions <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # add decision
  x$add_decisions(
    pproto("BinaryDecision",
           Decision,
           name = "Binary decision",
           apply = function(self, x) {
             assertthat::assert_that(inherits(x,
                                     "OptimizationProblem"))
             invisible(rcpp_apply_decisions(x$ptr, "B", 0, 1))
           }))
}
