#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets based on area units
#'
#' Specify targets expressed as area-based units.
#' For example, this function can be used to express targets as
#' hectares, acres, or \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' To ensure feasibility, area-based targets are clamped based on the
#' total abundance of features.
#'
#' @param targets `numeric` vector denoting the
#' area-based targets for the features.
#' These values must be expressed in the same units as `area_units`.
#' If a single `numeric` value is specified, then all
#' features are assigned targets based on the area-based target.
#'
#' @param area_units `character` vector denoting the unit of measurement
#' for each `targets` value (e.g., `"km^2", `"ha"`, `"acres"`).
#' If a single `character` value is specified, then all
#' features are assigned targets assuming that `targets`
#' are in the same units.
#'
#' @inheritSection add_auto_targets Data calculations
#'
#' @section Mathematical formulation:
#' This method provides an approach for setting target thresholds based
#' on an area-based threshold.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total spatial extent of a feature (e.g., geographic
#' range size expressed as \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}), and
#' \eqn{a} the specified area-based target
#' (expressed as \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}},
#' per `targets` and `area_units`)
#' Given this terminology, the target threshold (\eqn{t}) for a feature
#' is calculated as follows.
#' \deqn{
#' t = min(f, a)
#' }{
#' t = min(f, a)
#' }
#'
#' @inherit spec_jung_targets return seealso
#'
#' @family methods
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#'
#' @export
spec_area_targets <- function(targets, area_units) {
  # assert arguments are valid
  assert_valid_method_arg(targets)
  assert_required(area_units)
  # return new method
  new_method(
    name = "area targets",
    type = "relative",
    fun = calc_area_targets,
    args = list(
      targets = targets,
      area_units = area_units
    )
  )
}

calc_area_targets <- function(x, features, targets, area_units,
                              call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call, .internal = TRUE)
  assert_required(features, call = call, .internal = TRUE)
  assert_required(targets, call = call, .internal = TRUE)
  assert_required(area_units, call = call, .internal = TRUE)
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
    all_finite(targets),
    all_positive(targets),
    is_match_of(length(targets), c(1, number_of_features(x))),
    # area_units
    all_area_units(area_units),
    is_match_of(length(area_units), c(1, number_of_features(x))),
    call = call
  )
  assert_can_calculate_area_based_targets(x, features, call = call)

  # if needed, duplicate target values for each feature
  if (identical(length(targets), 1L)) {
    targets <- rep(targets, x$number_of_features())
  }
  if (identical(length(area_units), 1L)) {
    area_units <- rep(area_units, x$number_of_features())
  }

  # extract abundances
  fa <- x$feature_abundances_km2_in_total_units()[features, 1]

  # calculate targets as km^2
  targets <- as_km2(targets, area_units)

  # clamp targets to feature abundances
  targets <- pmin(targets, fa)

  # return target
  targets / fa
}
