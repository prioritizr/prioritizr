#' @include internal.R Decision-class.R
NULL

#' Add proportion decisions
#'
#' Add a proportion decision to a conservation planning [problem()].
#' This is a relaxed decision where a part of a planning unit can be
#' prioritized as opposed to the entire planning unit. Typically, this decision
#' has the assumed action of buying a fraction of a planning unit to include in
#  a protected area system. In most cases, problems that use proportion-type
#' decisions will solve much faster than problems that use binary-type
#' decisions
#'
#' @param x [problem()] object.
#'
#' @inherit add_binary_decisions details return
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
#' # create minimal problem with proportion decisions
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_proportion_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solutions
#' plot(s1, main = "solution")
#'
#' # build multi-zone conservation problem with proportion decisions
#' p2 <-
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_proportion_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve the problem
#' s2 <- solve(p2)
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' # panels show the proportion of each planning unit allocated to each zone
#' plot(s2, axes = FALSE)
#' }
#' @name add_proportion_decisions
NULL

#' @rdname add_proportion_decisions
#' @export
add_proportion_decisions <- function(x) {
  # assert argument is valid
  assert(is_conservation_problem(x))
  # add decision
  x$add_decisions(
    R6::R6Class(
      "ProportionDecision",
      inherit = Decision,
      public = list(
        name = "proportion decision",
        apply = function(x) {
          assert(inherits(x, "OptimizationProblem"), .internal = TRUE)
          invisible(rcpp_apply_decisions(x$ptr, "C", 0, 1))
        }
      )
    )$new()
  )
}
