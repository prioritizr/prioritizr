#' @include internal.R
NULL

#' Feature names
#'
#' Extract the names of the features in an object.
#'
#' @param x [problem()], [multi_problem()], or [Zones()] object.
#'
#' @param ... not used.
#'
#' @return A `character` vector of feature names.
#'
#' @name feature_names
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
#'    obj1 =
#'      problem(sim_pu_raster, sim_features[[1:2]]) %>%
#'      add_max_utility_objective(budget = b) %>%
#'      add_relative_targets(0.2) %>%
#'      add_binary_decisions(),
#'    obj2 =
#'      problem(sim_pu_raster, sim_features[[3:5]]) %>%
#'      add_min_shortfall_objective(budget = b) %>%
#'      add_relative_targets(0.8) %>%
#'      add_binary_decisions()
#'   )
#'
#' # print number of features
#' print(feature_names(mp))
#' }
#' @export
feature_names <- function(x, ...) {
  assert_required(x)
  UseMethod("feature_names")
}

#' @rdname feature_names
#'
#' @export
feature_names.ConservationProblem <- function(x, ...) {
  rlang::check_dots_empty()
  x$feature_names()
}

#' @rdname feature_names
#'
#' @export
feature_names.MultiObjConservationProblem <- function(x, ...) {
  rlang::check_dots_empty()
  x$feature_names()
}

#' @rdname feature_names
#'
#' @export
feature_names.ZonesRaster <- function(x, ...) {
  rlang::check_dots_empty()
  attr(x, "feature_names")
}

#' @rdname feature_names
#'
#' @export
feature_names.ZonesSpatRaster <- function(x, ...) {
  rlang::check_dots_empty()
  attr(x, "feature_names")
}

#' @rdname feature_names
#'
#' @export
feature_names.ZonesCharacter <- function(x, ...) {
  rlang::check_dots_empty()
  attr(x, "feature_names")
}
