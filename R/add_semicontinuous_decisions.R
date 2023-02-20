#' @include internal.R Decision-class.R
NULL

#' Add semi-continuous decisions
#'
#' Add a semi-continuous decision to a conservation planning
#' problem. This is a relaxed decision where a part of a planning
#' unit can be prioritized, as opposed to the entire planning unit.
#' This decision is similar to the [add_proportion_decisions()]
#' function, except that it has an upper bound parameter. By default, the
#' decision can range from prioritizing none (0%) to all (100%) of a
#' planning unit. However, an upper bound can be specified to ensure that, at
#' most, only a fraction (e.g., 80%) of a planning unit can be prioritized. This
#' type of decision may be useful when it is not practical to conserve entire
#' planning units.
#'
#' @param x [problem()] object.
#'
#' @param upper_limit `numeric` value specifying the maximum proportion
#'   of a planning unit that can be reserved (e.g., set to 0.8 for 80%).
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
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create minimal problem with semi-continuous decisions
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_semicontinuous_decisions(0.5) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solutions
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # build multi-zone conservation problem with semi-continuous decisions
#' p2 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_semicontinuous_decisions(0.5) %>%
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
#' @name add_semicontinuous_decisions
NULL

#' @rdname add_semicontinuous_decisions
#' @export
add_semicontinuous_decisions <- function(x, upper_limit) {
  # assert arguments are valid
  rlang::check_required(x)
  rlang::check_required(upper_limit)
  assert(
    is_conservation_problem(x),
    assertthat::is.number(upper_limit),
    all_finite(upper_limit),
    upper_limit <= 1,
    upper_limit >= 0
  )
  # add decision to problem
  x$add_decisions(
    R6::R6Class(
      "SemiContinuousDecision",
      inherit = Decision,
       public = list(
         name = "semicontinuous decision",
         data = list(upper_limit = upper_limit),
         apply = function(x) {
          assert(inherits(x, "OptimizationProblem"), .internal = TRUE)
          invisible(
            rcpp_apply_decisions(x$ptr, "C", 0, self$get_data("upper_limit"))
          )
        }
      )
    )$new()
  )
}
