#' @include internal.R
NULL

#' Number of problems
#'
#' Extract the number of conservation problems in an object.
#'
#' @param x A [problem()],or [multi_problem()] object.
#'
#' @param ... not used.
#'
#' @return An `integer` number of problems.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # define budget for multi-objective problem
#' b <- 0.3 * terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]]
#'
#' # create multi-objective problem
#' mp <-
#'   multi_problem(
#'     obj1 =
#'       problem(sim_pu_raster, sim_features[[1:2]]) %>%
#'       add_max_wtd_sum_objective(budget = b) %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions(),
#'     obj2 =
#'       problem(sim_pu_raster, sim_features[[3:5]]) %>%
#'       add_min_shortfall_objective(budget = b) %>%
#'       add_relative_targets(0.8) %>%
#'       add_binary_decisions()
#'   ) %>%
#'   add_hier_approach(rel_tol = 0)
#'
#' # print number of problems
#' print(number_of_problems(mp))
#' }
#' @export
number_of_problems <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  UseMethod("number_of_problems")
}

#' @rdname number_of_problems
#'
#' @export
number_of_problems.ConservationProblem <- function(x, ...) {
  1L
}

#' @rdname number_of_features
#'
#' @export
number_of_problems.MultiObjConservationProblem <- function(x, ...) {
  x$number_of_problems()
}
