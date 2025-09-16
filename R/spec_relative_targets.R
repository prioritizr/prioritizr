#' @include internal.R ConservationProblem-class.R
NULL

#' Specify relative targets
#'
#' Specify targets as a proportion (between 0 and 1) of the maximum level of
#' representation of each feature in the study area.
#' Please note that proportions
#' are scaled according to the features' total abundances in the study area
#' (including any locked out planning units, or planning units with `NA`
#' cost values) using the [feature_abundances()] function.
#' Note that this function is designed to be used within [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @param targets `numeric` vector that specifies targets for each
#' of the features. If a single `numeric` value is specified, then all features
#' are assigned the same proportion-based target. Note that values
#' range between 0 and 1 (corresponding to 0% and 100% respectively).
#'
#' @inheritParams spec_absolute_targets
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on a proportion.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total abundance of a feature (e.g., geographic
#' range size), and \eqn{a} the relative target for the feature
#' (per `targets`).
#' Given this terminology, the target threshold (\eqn{t}) for a feature
#' is calculated as follows.
#' \deqn{t = f * a}
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#' @inherit spec_jung_targets seealso return
#'
#' @references
#' Carwardine J, Klein CJ, Wilson KA, Pressey RL, Possingham HP (2009) Hitting
#' the target and missing the point: target‐based conservation planning in
#' context. *Conservation Letters*, 2: 4--11.
#'
#' @family methods
#'
#' @examples
#' # TODO
#'
#' @export
spec_relative_targets <- function(targets, ...) {
  # assert arguments are valid
  assert_valid_method_arg(targets, "add_relative_targets")
  assert_required(targets)
  rlang::check_dots_empty()
  # return new method
  new_method(
    name = "Relative targets",
    type = "relative",
    fun = calc_relative_targets,
    args = list(targets = targets)
  )
}

calc_relative_targets <- function(x, features, targets,
                                  call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call)
  assert_required(targets, call = call)
  assert(
    # x
    is_conservation_problem(x),
    has_single_zone(x),
    # features
    is_integer(features),
    all(features >= 1),
    all(features <= x$number_of_features()),
    call = call,
    .internal = TRUE
  )
  assert(
    # targets
    is.numeric(targets),
    is_match_of(length(targets), c(1, number_of_features(x))),
    all_finite(targets),
    all_proportion(targets),
    call = call
  )

  # if needed, duplicate target values for each feature
  if (identical(length(targets), 1L)) {
    targets <- rep(targets, x$number_of_features())
  }

  # return targets
  targets[features]
}
