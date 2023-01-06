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
#' @param x [problem()] object.
#'
#' @details Conservation planning problems involve making decisions on planning
#'   units. These decisions are then associated with actions (e.g., turning a
#'   planning unit into a protected area). Only a
#'   single decision should be added to a [problem()] object.
#'   Note that if multiple decisions are added to an object, then the
#'   last one to be added will be used.
#'
#' @return An updated [problem()] object with the decisions added to it.
#'
#' @seealso
#' See [decisions] for an overview of all functions for adding decisions.
#'
#' @family decisions
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_pu_zones_raster <- get_sim_zones_pu_raster()
#' sim_features_zones <- get_sim_zones_features()
#'
#' # create minimal problem with binary decisions
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution")
#'
#' # create a matrix with targets for a multi-zone conservation problem
#' targs <- matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)
#'
#' # build multi-zone conservation problem with binary decisions
#' p2 <-
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(targs)  %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' plot(category_layer(s2), main = "solution", axes = FALSE)
#' }
#' @name add_binary_decisions
NULL

#' @rdname add_binary_decisions
#' @export
add_binary_decisions <- function(x) {
  # assert argument is valid
  assertthat::assert_that(is_conservation_problem(x))
  # add decision
  x$add_decisions(pproto(
    "BinaryDecision",
     Decision,
     name = "Binary decision",
     apply = function(self, x) {
       assertthat::assert_that(inherits(x,"OptimizationProblem"))
       invisible(rcpp_apply_decisions(x$ptr, "B", 0, 1))
     }
   ))
}
