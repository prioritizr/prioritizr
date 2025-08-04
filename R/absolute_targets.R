#' @include internal.R ConservationProblem-class.R
NULL

#' Specify absolute targets
#'
#' Specify targets expressed as the actual value
#' of features in the study area that need to be represented in the
#' prioritization. For example,
#' setting a target of 10 requires that the solution secure a set of
#' planning units for which their summed feature values are equal to or greater
#' than 10.
#' This function is designed to be used within `add_auto_targets()`.
#'
#' @param targets `numeric` vector that specifies targets for each
#' of the features. If a single value is specified, then all features
#' are assigned the same target threshold.
#'
#' @param ... not used.
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on a pre-specified
#' value.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{a} the absolute target for a feature
#' (per `targets`).
#' Given this terminology, the target threshold (\eqn{t}) for a feature
#' is calculated as follows.
#' \deqn{t = a}
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#' @inherit jung_targets seealso return
#' @inherit absolute_targets references
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
absolute_targets <- function(targets, ...) {
  UseMethod("absolute_targets")
}

#' @export
absolute_targets.default <- function(targets, ...) {
  # assert no dots
  rlang::check_dots_empty()
  # return method
  new_method(
    name = "Absolute targets",
    type = "absolute",
    fun = internal_absolute_targets,
    args = list(targets = targets)
  )
}

#' @export
absolute_targets.ConservationProblem <- function(targets, ...) {
  target_problem_error("add_absolute_targets")
}

internal_absolute_targets <- function(x, features, targets,
                                      call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call)
  assert_required(features, call = call)
  assert_required(targets, call = call)
  assert(
    # x
    is_conservation_problem(x),
    has_single_zone(x),
    # features
    is_integer(features),
    all(features >= 1),
    all(features <= x$number_of_features()),
    # targets
    is.numeric(targets),
    is_match_of(length(targets), c(1, number_of_features(x))),
    all_finite(targets),
    call = call
  )

  # if needed, duplicate target values for each feature
  if (identical(length(targets), 1L)) {
    targets <- rep(targets, x$number_of_features())
  }

  # return targets
  targets[features]
}
