#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets based on interpolating area-based thresholds
#'
#' Specify targets by interpolating them between area-based thresholds.
#' Briefly, this method involves
#' (i) setting target thresholds for rare features to a particular percentage
#' threshold, (ii) setting target thresholds for common features
#' to a particular percentage threshold, and (iii) interpolating
#' target thresholds for features with spatial distributions that
#' range between the those for the rare and common features.
#' Additionally, features can (optionally) have their targets capped at a
#' particular threshold.
#' This method is especially useful for setting targets based on
#' interpolation procedures when features have data expressed as an area-based
#' unit of measurement (e.g., \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#' Note that this function is designed to be used with [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @param rare_area_threshold `numeric` value indicating the threshold
#' area for identifying rare features.
#' This value must be expressed in the same units as the feature data.
#' In particular, features with a total spatial extent
#' smaller than this value will be considered rare during the target setting
#' calculations.
#'
#' @param rare_relative_target `numeric` value indicating the
#' relative target for rare features.
#' Note that this value must be a proportion between 0 and 1.
#' For example, a value of 0.1 corresponds to 10%.
#'
#' @param rare_area_target `numeric` value denoting the
#' area-based target for rare features.
#' This value must be expressed in the same units as `area_units`.
#' To avoid setting an area-based target for rare features,
#' a missing (`NA`) value can be specified.
#'
#' @param rare_method `character` value indicating how the target for rare
#' features should be calculated. Available options include `"min"` and `"max"`.
#' For example, a value of `"max"` means that the target for a rare features
#' is calculated as the maximum based on `rare_relative_target` and
#' `rare_area_threshold`. Note that `rare_method` will have no effect on
#' the target calculations if `rare_area_target` is a missing (`NA`) value.
#'
#' @param common_area_threshold `numeric` value indicating the
#' threshold area for identifying common features.
#' This value must be expressed in the same units as `area_units`.
#' In particular, features with a total spatial extent
#' greater than this value will be considered common during the target setting
#' calculations.
#'
#' @param common_relative_target `numeric` value denoting the
#' relative target for common features.
#' Note that this value must be a proportion between 0 and 1.
#' For example, a value of 0.1 corresponds to 10%.
#'
#' @param common_area_target `numeric` value denoting the
#' area-based target for common features.
#' This value must be expressed in the same units as `area_units`.
#' To avoid setting an area-based target for common features,
#' a missing (`NA`) value can be specified.
#'
#' @param common_method `character` value indicating how the target for common
#' features should be calculated. Available options include `"min"` and `"max"`.
#' For example, a value of `"max"` means that the target for a common feature
#' is calculated as the maximum based on `common_relative_target` and
#' `common_area_threshold`. Note that `common_method` will have
#' no effect on the target calculations if `common_area_target` is a
#' missing (`NA`) value.
#'
#' @param cap_area_target `numeric` value denoting the area-based target
#' cap.
#' This value must be expressed in the same units as `area_units`.
#' In particular, all targets are clamped to this value during target setting
#' calculations.
#' To avoid setting a target cap,
#' a missing (`NA`) value can be specified.
#'
#' @param interp_method `character` value denoting the interpolation method.
#' Available options include `"linear"` for linear interpolation and
#' `"log10"` for log-linear interpolation.
#'
#' @param area_units `character` value denoting the unit of measurement
#' for the area-based arguments (e.g., `"km^2", `"ha"`, `"acres"`).
#'
#' @inheritSection spec_jung_targets Data calculations
#'
#' @section Mathematical formulation:
#' This method provides a flexible approach for setting target thresholds based
#' on an interpolation procedure and the spatial extent of the features.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total spatial extent of a feature (e.g., geographic
#' range size),
#' \eqn{a} the threshold for identifying rare features
#' (per `rare_area_threshold` and `area_units`),
#' \eqn{b} the relative targets for rare features
#' (per `rare_relative_target`),
#' \eqn{c} the area-based targets for rare features
#' (per `rare_area_target` and `area_units`),
#' \eqn{d()} the function for calculating targets for rare features
#' as a maximum or minimum value (per `rare_method`),
#' \eqn{e} the threshold for identifying common features
#' (per `common_area_threshold` and `area_units`),
#' \eqn{g} the relative targets for common features
#' (per `common_relative_target`),
#' \eqn{h} the area-based targets for common features
#' (per `common_area_target` and `area_units`),
#' \eqn{i()} the method for calculating targets for common features
#' as a maximum or minimum value  (per `common_method`), and
#' \eqn{j} the target cap (per `cap_area_target` and `area_units`), and
#' \eqn{k()} the interpolation method for features with a spatial distribution
#' that is larger than a rare features and smaller than a common feature
#' (per `interp_method`).
#' In particular, \eqn{k()} is either a linear or log-linear interpolation
#' procedure based on the thresholds for identifying rare and common features
#' as well as the relative targets for rare and common features.
#' Given this terminology, the target threshold (\eqn{t}) for the feature
#' is calculated as follows.
#' \itemize{
#' \item If \eqn{f < a}, then \eqn{
#' t = min(d(c, b \times f), j)}{
#' t = min(d(c, b * f), j)
#' }.
#' \item If \eqn{f > e}, then \eqn{
#' t = min(i(h, g \times f), j)}{.
#' t = min(i(h, g * f), j)
#' }.
#' \item If \eqn{a \leq f \leq e}{a <= f <= e}, then
#' \eqn{t = min(k(f, a, b, e, g), j)}.
#' }
#'
#' @inherit spec_interp_absolute_targets details references
#'
#' @inherit spec_jung_targets return seealso
#'
#' @family methods
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_complex_pu_raster <- get_sim_complex_pu_raster()
#' sim_complex_features <- get_sim_complex_features()
#'
#' # create problem with interpolated targets.
#' # here, targets will be set as 100% for features smaller than 1000 km^2
#' # in size, 10% for features greater than 250,000 km^2 in size,
#' # log-linearly interpolated for features with an intermediate range size,
#' # and capped at 1,000,000 km^2
#' p1 <-
#'   problem(sim_complex_pu_raster, sim_complex_features) %>%
#'   add_min_set_objective() %>%
#'   add_auto_targets(
#'     method = spec_interp_area_targets(
#'      rare_area_threshold = 1000,
#'      rare_relative_target = 1,
#'      rare_area_target = NA,            # not used
#'      rare_method = "max",              # not used
#'      common_area_threshold = 250000,
#'      common_relative_target = 0.1,
#'      common_area_target = NA,          # not used
#'      common_method = "max",            # not used
#'      cap_area_target = 1000000,
#'      interp_method = "log10",
#'      area_units = "km^2"
#'     )
#'   ) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#' }
#' @export
spec_interp_area_targets <- function(rare_area_threshold,
                                     rare_relative_target,
                                     rare_area_target,
                                     rare_method,
                                     common_area_threshold,
                                     common_relative_target,
                                     common_area_target,
                                     common_method,
                                     cap_area_target,
                                     interp_method,
                                     area_units) {
  # assert arguments are valid
  assert_valid_method_arg(rare_area_threshold)
  assert_required(rare_area_threshold)
  assert_required(rare_relative_target)
  assert_required(rare_area_target)
  assert_required(rare_method)
  assert_required(common_area_threshold)
  assert_required(common_relative_target)
  assert_required(common_area_target)
  assert_required(common_method)
  assert_required(cap_area_target)
  assert_required(interp_method)
  assert_required(area_units)
  # return new method
  new_target_method(
    name = "interpolated area targets",
    type = "relative",
    fun = calc_interp_area_targets,
    args = list(
      rare_area_threshold = rare_area_threshold,
      rare_relative_target = rare_relative_target,
      rare_area_target = rare_area_target,
      rare_method = rare_method,
      common_area_threshold = common_area_threshold,
      common_relative_target = common_relative_target,
      common_area_target = common_area_target,
      common_method = common_method,
      cap_area_target = cap_area_target,
      interp_method = interp_method,
      area_units = area_units
    )
  )
}

calc_interp_area_targets <- function(x, features,
                                     rare_area_threshold,
                                     rare_relative_target,
                                     rare_area_target,
                                     rare_method,
                                     common_area_threshold,
                                     common_relative_target,
                                     common_area_target,
                                     common_method,
                                     cap_area_target,
                                     interp_method,
                                     area_units,
                                     call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call, .internal = TRUE)
  assert_required(features, call = call, .internal = TRUE)
  assert_required(rare_area_threshold, call = call, .internal = TRUE)
  assert_required(rare_relative_target, call = call, .internal = TRUE)
  assert_required(rare_area_target, call = call, .internal = TRUE)
  assert_required(rare_method, call = call, .internal = TRUE)
  assert_required(common_area_threshold, call = call, .internal = TRUE)
  assert_required(common_relative_target, call = call, .internal = TRUE)
  assert_required(common_area_target, call = call, .internal = TRUE)
  assert_required(common_method, call = call, .internal = TRUE)
  assert_required(cap_area_target, call = call, .internal = TRUE)
  assert_required(interp_method, call = call, .internal = TRUE)
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
    # rare_area_threshold
    assertthat::is.number(rare_area_threshold),
    all_finite(rare_area_threshold),
    rare_area_threshold >= 0,
    # common_area_threshold
    assertthat::is.number(common_area_threshold),
    all_finite(common_area_threshold),
    common_area_threshold >= 0,
    rare_area_threshold <= common_area_threshold,
    # interp_method
    assertthat::is.string(interp_method),
    assertthat::noNA(interp_method),
    is_match_of(interp_method, c("linear", "log10")),
    # rare_method
    assertthat::is.string(rare_method),
    assertthat::noNA(rare_method),
    is_match_of(rare_method, c("min", "max")),
    # common_method
    assertthat::is.string(common_method),
    assertthat::noNA(common_method),
    is_match_of(common_method, c("min", "max")),
    # number arguments
    assertthat::is.number(rare_relative_target),
    all_finite(rare_relative_target),
    all_proportion(rare_relative_target),
    assertthat::is.number(common_relative_target),
    all_finite(common_relative_target),
    all_proportion(common_relative_target),
    assertthat::is.scalar(rare_area_target),
    assertthat::is.scalar(common_area_target),
    assertthat::is.scalar(cap_area_target),
    # area_units
    is_area_units(area_units),
    call = call
  )
  assert_can_calculate_area_based_targets(x, features, call = call)
  # assert valid bounds for non-missing values
  if (assertthat::noNA(rare_area_target)) {
    assert(
      assertthat::is.number(rare_area_target),
      all_finite(rare_area_target),
      rare_area_target >= 0,
      call = call
    )
  } else {
    ## this is needed to account for different NA classes
    rare_area_target <- NA_real_
  }
  if (assertthat::noNA(common_area_target)) {
    assert(
      assertthat::is.number(common_area_target),
      all_finite(common_area_target),
      common_area_target >= 0,
      call = call
    )
  } else {
    ## this is needed to account for different NA classes
    common_area_target <- NA_real_
  }
  if (assertthat::noNA(cap_area_target)) {
    assert(
      assertthat::is.number(cap_area_target),
      all_finite(cap_area_target),
      cap_area_target >= 0,
      call = call
    )
  } else {
    ## this is needed to account for different NA classes
    cap_area_target <- NA_real_ # nocov
  }

  # calculate targets
  calc_interp_targets(
    x = c(x$feature_abundances_km2_in_total_units()[features, 1]),
    rare_absolute_threshold = as_km2(rare_area_threshold, area_units),
    rare_relative_target = rare_relative_target,
    rare_absolute_target = as_km2(rare_area_target, area_units),
    rare_method = rare_method,
    common_absolute_threshold = as_km2(common_area_threshold, area_units),
    common_relative_target = common_relative_target,
    common_absolute_target = as_km2(common_area_target, area_units),
    common_method = common_method,
    cap_absolute_target = as_km2(cap_area_target, area_units),
    interp_method = interp_method,
    call = call
  )
}
