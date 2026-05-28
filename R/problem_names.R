#' @include internal.R
NULL

#' Problem names
#'
#' Extract the names of the problems in an object.
#'
#' @param x [multi_problem()] object.
#'
#' @param ... not used.
#'
#' @return A `character` vector.
#'
#' @name problem_names
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create problem
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions()
#'
#' # print feature names
#' print(feature_names(p))
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
#'   )
#'
#' # print problem names
#' print(problem_names(mp))
#' }
#' @export
problem_names <- function(x, ...) {
  assert_required(x)
  UseMethod("problem_names")
}

#' @rdname feature_names
#'
#' @export
problem_names.MultiObjConservationProblem <- function(x, ...) {
  rlang::check_dots_empty()
  x$problem_names()
}
