#' @include internal.R
NULL

#' Feature names
#'
#' Extract the names of the features in an object.
#'
#' @param x [problem()] or [Zones()] object.
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
#' }
#' @export
feature_names <- function(x, ...) {
  UseMethod("feature_names")
}

#' @rdname feature_names
#'
#' @export
feature_names.ConservationProblem <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  x$feature_names()
}

#' @rdname feature_names
#'
#' @export
feature_names.ZonesRaster <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  attr(x, "feature_names")
}

#' @rdname feature_names
#'
#' @export
feature_names.ZonesSpatRaster <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  attr(x, "feature_names")
}

#' @rdname feature_names
#'
#' @export
feature_names.ZonesCharacter <- function(x, ...) {
  assert_required(x)
  rlang::check_dots_empty()
  attr(x, "feature_names")
}
