#' @include internal.R
NULL

#' Number of features
#'
#' Extract the number of features in an object.
#'
#' @param x A [problem()], [optimization_problem()], or [zones()] object.
#'
#' @param ... not used.
#'
#' @return An `integer` number of features.
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
#' # print number of features
#' print(number_of_features(p))
#' }
#' @export
number_of_features <- function(x, ...) UseMethod("number_of_features")

#' @rdname number_of_features
#'
#' @export
number_of_features.ConservationProblem <- function(x, ...) {
  rlang::check_required(x)
  rlang::check_dots_empty()
  x$number_of_features()
}

#' @rdname number_of_features
#'
#' @export
number_of_features.OptimizationProblem <- function(x, ...) {
  rlang::check_required(x)
  rlang::check_dots_empty()
  x$number_of_features()
}

#' @rdname number_of_features
#'
#' @export
number_of_features.ZonesSpatRaster <- function(x, ...) {
  rlang::check_required(x)
  rlang::check_dots_empty()
  terra::nlyr(x[[1]])
}

#' @rdname number_of_features
#'
#' @export
number_of_features.ZonesRaster <- function(x, ...) {
  rlang::check_required(x)
  rlang::check_dots_empty()
  raster::nlayers(x[[1]])
}

#' @rdname number_of_features
#'
#' @export
number_of_features.ZonesCharacter <- function(x, ...) {
  rlang::check_required(x)
  rlang::check_dots_empty()
  length(x[[1]])
}
