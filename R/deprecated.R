#' @include internal.R
NULL

#' Deprecation notice
#'
#' The functions listed here are deprecated.
#' This means that they once existed in earlier versions of the
#' of the \pkg{prioritizr} package, but they have since been removed
#' entirely, replaced by other functions, or renamed as other functions
#' in newer versions.
#' To help make it easier to transition to new versions of the \pkg{prioritizr}
#' package, we have listed alternatives for deprecated the functions
#' (where applicable).
#' If a function is described as being renamed, then this means
#' that only the name of the function has changed
#' (i.e., the inputs, outputs, and underlying code remain the same).
#'
#' @param ... not used.
#'
#' @details
#' The following functions have been deprecated:
#'
#' \describe{
#'
#' \item{`add_connected_constraints()`}{renamed
#'   as the [add_contiguity_constraints()] function.}
#'
#' \item{`add_corridor_constraints()`}{replaced by the
#'   [add_feature_contiguity_constraints()] function.}
#'
#' \item{`add_loglinear_targets()`}{replaced by the
#'   [add_interpolated_absolute_targets()] function.}
#'
#' \item{`set_number_of_threads()`}{
#'   no longer needed due to improved data extraction methods.}
#'
#' \item{`get_number_of_threads()`}{
#'   no longer needed due to improved data extraction methods.}
#'
#' \item{`is.parallel()`}{
#'   no longer needed due to improved data extraction methods.}
#'
#' \item{`add_pool_portfolio()`}{replaced by the
#'   [add_extra_portfolio()] and [add_top_portfolio()].}
#'
#' \item{`connected_matrix()`}{renamed as
#'   the [adjacency_matrix()] function.}
#'
#' \item{`feature_representation()`}{replaced by
#'  the [eval_feature_representation_summary()] function for consistency with
#'  other functions.}
#'
#' \item{`replacement_cost()`}{renamed as
#'  the [eval_replacement_importance()] function for consistency with
#'  other functions for evaluating solutions.}
#'
#' \item{`rarity_weighted_richness()`}{renamed as
#'  the [eval_rare_richness_importance()] function for consistency with
#'  other functions for evaluating solutions.}
#'
#' \item{`ferrier_score()`}{renamed as
#'  the [eval_ferrier_importance()] function for consistency with
#'  other functions for evaluating solutions.}
#'
#' \item{`distribute_load()`}{has been removed because it is no longer used.
#'   See `parallel::splitIndices()` for equivalent functionality.}
#'
#' \item{`new_optimization_problem()`}{
#'   replaced by [optimization_problem()].}
#'
#' \item{`predefined_optimization_problem()`}{
#'   replaced by [optimization_problem()].}
#'
#' }
#'
#' @keywords deprecated
#'
#' @name prioritizr-deprecated
NULL

#' @rdname prioritizr-deprecated
#' @export
add_connected_constraints <- function(...) {
  cli_defunct(
    old = "add_contiguity_constraints",
    new = "add_contiguity_constraints"
  )
}

#' @rdname prioritizr-deprecated
#' @export
add_corridor_constraints <- function(...) {
  cli_defunct(
    old = "add_corridor_constraints",
    new = "add_feature_contiguity_constraints"
  )
}

#' @rdname prioritizr-deprecated
#' @export
set_number_of_threads <- function(...) {
  cli_defunct(
    old = "set_number_of_threads"
  )
}

#' @rdname prioritizr-deprecated
#' @export
get_number_of_threads <- function(...) {
  cli_defunct(
    old = "get_number_of_threads"
  )
}

#' @rdname prioritizr-deprecated
#' @export
is.parallel <- function(...) {
  cli_defunct(
    old = "is.parallel"
  )
}

#' @rdname prioritizr-deprecated
#' @export
add_pool_portfolio <- function(...) {
  cli_defunct(
    old = "add_pool_portfolio",
    new = "add_top_portfolio()"
  )
}

#' @rdname prioritizr-deprecated
#' @export
connected_matrix <- function(...) {
  cli_defunct(
    old = "connected_matrix",
    new = "adjacency_matrix"
  )
}

#' @rdname prioritizr-deprecated
#' @export
feature_representation <- function(...) {
  cli_defunct(
    old = "feature_representation",
    new = "eval_feature_representation_summary"
  )
}

#' @rdname prioritizr-deprecated
#' @export
replacement_cost <- function(...) {
  cli_defunct(
    old = "replacement_cost",
    new = "eval_replacement_importance"
  )
}

#' @rdname prioritizr-deprecated
#' @export
rarity_weighted_richness <- function(...) {
  cli_defunct(
    old = "replacement_cost",
    new = "eval_rwr_importance"
  )
}

#' @rdname prioritizr-deprecated
#' @export
ferrier_score <- function(...) {
  cli_defunct(
    old = "ferrier_score",
    new = "eval_ferrier_importance"
  )
}

#' @rdname prioritizr-deprecated
#' @export
distribute_load <- function(...) {
  cli_defunct(
    old = "distribute_load",
    new = "parallel::splitIndices"
  )
}

#' @rdname prioritizr-deprecated
#' @export
new_optimization_problem <- function(...) {
  cli_defunct(
    old = "new_optimization_problem",
    new = "optimization_problem"
  )
}

#' @rdname prioritizr-deprecated
#' @export
predefined_optimization_problem <- function(...) {
  cli_defunct(
    old = "predefined_optimization_problem",
    new = "optimization_problem"
  )
}

#' @rdname prioritizr-deprecated
#' @export
add_loglinear_targets <- function(...) {
  cli_defunct(
    old = "add_loglinear_targets",
    new = "add_interpolated_targets"
  )
}

raster_pkg_deprecation_notice <- c(
  "Support for {.pkg raster} package will be deprecated.",
  "i" = "Use {.fn terra::rast} to convert data for future compatibility."
)

sp_pkg_deprecation_notice <- c(
  "Support for {.pkg sp} package will be deprecated.",
  "i" = "Use {.fn sf::st_as_sf} to convert data for future compatibility."
)
