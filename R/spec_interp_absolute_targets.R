#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets based on interpolating absolute thresholds
#'
#' Specify targets by interpolating them between thresholds expressed as the
#' same values as the underlying feature data (ignoring any specified
#' feature units).
#' Briefly, this method involves
#' (i) setting target thresholds for rare features to a particular percentage
#' threshold, (ii) setting target thresholds for common features
#' to a particular percentage threshold, and (iii) interpolating
#' target thresholds for features with spatial distributions that
#' range between the those for the rare and common features.
#' Additionally, features can (optionally) have their targets capped at a
#' particular threshold.
#' This method is especially useful for setting targets based on
#' interpolation procedures when features do not have data expressed as an
#' area-based unit of measurement.
#' Note that this function is designed to be used with [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @param rare_absolute_threshold `numeric` value indicating the absolute
#' threshold for identifying rare features.
#' This value must be expressed in the same units as the feature data.
#' In particular, features with a total abundance
#' smaller than this value will be considered rare during the target setting
#' calculations.
#'
#' @param rare_relative_target `numeric` value indicating the
#' relative target for rare features.
#' Note that this value must be a proportion between 0 and 1.
#' For example, a value of 0.1 corresponds to 10%.
#'
#' @param rare_absolute_target `numeric` value denoting the
#' absolute target for rare features.
#' This value must be expressed in the same units as the feature data.
#' To avoid setting an absolute target for rare features,
#' a missing (`NA`) value can be specified.
#'
#' @param rare_method `character` value indicating how the target for rare
#' features should be calculated. Available options include `"min"` and `"max"`.
#' For example, a value of `"max"` means that the target for a rare features
#' is calculated as the maximum based on `rare_relative_target` and
#' `rare_absolute_target`. Note that `rare_method` will have no effect on
#' the target calculations if `rare_absolute_target` is a missing (`NA`) value.
#'
#' @param common_absolute_threshold `numeric` value indicating the
#' absolute threshold for identifying common features.
#' This value must be expressed in the same units as the feature data.
#' In particular, features with a total abundance
#' greater than this value will be considered common during the target setting
#' calculations.
#'
#' @param common_relative_target `numeric` value denoting the
#' relative target for common features.
#' Note that this value must be a proportion between 0 and 1.
#' For example, a value of 0.1 corresponds to 10%.
#'
#' @param common_absolute_target `numeric` value denoting the
#' absolute target for common features.
#' This value must be expressed in the same units as the feature data.
#' To avoid setting an absolute target for common features,
#' a missing (`NA`) value can be specified.
#'
#' @param common_method `character` value indicating how the target for common
#' features should be calculated. Available options include `"min"` and `"max"`.
#' For example, a value of `"max"` means that the target for a common feature
#' is calculated as the maximum based on `common_relative_target` and
#' `common_absolute_target`. Note that `common_method` will have no effect on
#' the target calculations if `common_absolute_target` is a missing (`NA`)
#' value.
#'
#' @param cap_absolute_target `numeric` value denoting the absolute target
#' cap.
#' This value must be expressed in the same units as the feature data.
#' In particular, all targets are clamped to this value during target setting
#' calculations.
#' To avoid setting a target cap,
#' a missing (`NA`) value can be specified.
#'
#' @param interp_method `character` value denoting the interpolation method.
#' Available options include `"linear"` for linear interpolation and
#' `"log10"` for log-linear interpolation.
#'
#' @details
#' This method has been applied to set target thresholds at global and national
#' scales (e.g., Butchart *et al.* 2015;
#' Rodrigues *et al.* 2004; Polak *et al.* 2015).
#' It is based on the rationale that species with a smaller geographic
#' distribution are at a greater risk of extinction, and so require
#' a larger percentage of their geographic distribution to be represented
#' by a prioritization (Rodrigues *et al.* 2004).
#' When using this method in a planning exercise, it is important to ensure
#' that the threshold parameters reflect the stakeholder objectives.
#' Additionally, the threshold parameters may need to set according to
#' the spatial extent of the planning region.
#'
#' @section Mathematical formulation:
#' This method provides a flexible approach for setting target thresholds based
#' on an interpolation procedure and the feature data.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total abundance of a feature,
#' \eqn{a} the threshold for identifying rare features
#' (per `rare_absolute_threshold`),
#' \eqn{b} the relative targets for rare features
#' (per `rare_relative_target`),
#' \eqn{c} the absolute targets for rare features
#' (per `rare_absolute_target`),
#' \eqn{d()} the function for calculating targets for rare features
#' as a maximum or minimum value (per `rare_method`),
#' \eqn{e} the threshold for identifying common features
#' (per `common_absolute_threshold`),
#' \eqn{g} the relative targets for common features
#' (per `common_relative_target`),
#' \eqn{h} the absolute targets for common features
#' (per `common_absolute_target`),
#' \eqn{i()} the method for calculating targets for common features
#' as a maximum or minimum value  (per `common_method`),
#' \eqn{j} the target cap (per `cap_absolute_target`), and
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
#' @inherit spec_jung_targets return seealso
#'
#' @references
#' Butchart SHM, Clarke M, Smith RJ, Sykes RE, Scharlemann JPW, Harfoot M,
#' Buchanan GM, Angulo A, Balmford A, Bertzky B, Brooks TM, Carpenter KE,
#' Comeros‐Raynal MT, Cornell J, Ficetola GF, Fishpool LDC, Fuller RA,
#' Geldmann J, Harwell H, Hilton‐Taylor C, Hoffmann M, Joolia A, Joppa L,
#' Kingston N, May I, Milam A, Polidoro B, Ralph G, Richman N, Rondinini C,
#' Segan DB, Skolnik B, Spalding MD, Stuart SN, Symes A, Taylor J, Visconti P,
#' Watson JEM, Wood L, Burgess ND (2015) Shortfalls and solutions for meeting
#' national and global conservation area targets. *Conservation Letters*,
#' 8: 329--337.
#'
#' Polak T, Watson JEM, Fuller RA, Joseph LN, Martin TG, Possingham HP,
#' Venter O, Carwardine J (2015) Efficient expansion of global protected areas
#' requires simultaneous planning for species and ecosystems.
#' *Royal Society Open Science*, 2: 150107.
#'
#' Rodrigues ASL, Akçakaya HR, Andelman SJ, Bakarr MI, Boitani L, Brooks TM,
#' Chanson JS, Fishpool LDC, Da Fonseca GAB, Gaston KJ, Hoffmann M, Marquet PA,
#' Pilgrim JD, Pressey RL, Schipper J, Sechrest W, Stuart SN, Underhill LG,
#' Waller RW, Watts MEJ, Yan X (2004)
#' Global gap analysis: priority regions for expanding the global
#' protected-area network. *BioScience*, 54: 1092--1100.
#'
#' @family methods
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # this function sets targets based on the total abundance of the features
#' # (i.e., sum of planning unit values for the feature) and does not
#' # consider the spatial area covered by the planning units
#'
#' # display the total abundance of the features
#' print(terra::global(get_sim_features(), "sum", na.rm = TRUE))
#'
#' # create problem with interpolated targets.
#' # here, targets will be set as 70% for features with a total abundance
#' # (i.e., sum of planning unit values for the feature) smaller than 50,
#' # 20% for features with at total abundance greater than 70,
#' # linearly interpolated for features with an intermediate range size,
#' # and capped at a total abundance of 100
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_auto_targets(
#'     method = spec_interp_absolute_targets(
#'       rare_absolute_threshold = 50,
#'       rare_relative_target = 0.7,
#'       rare_absolute_target = NA,            # not used
#'       rare_method = "max",                  # not used
#'       common_absolute_threshold = 70,
#'       common_relative_target = 0.2,
#'       common_absolute_target = NA,          # not used
#'       common_method = "max",                # not used
#'       cap_absolute_target = 100,
#'       interp_method = "linear"
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
spec_interp_absolute_targets <- function(rare_absolute_threshold,
                                         rare_relative_target,
                                         rare_absolute_target,
                                         rare_method,
                                         common_absolute_threshold,
                                         common_relative_target,
                                         common_absolute_target,
                                         common_method,
                                         cap_absolute_target,
                                         interp_method) {
  # assert arguments are valid
  assert_valid_method_arg(rare_absolute_threshold)
  assert_required(rare_absolute_threshold)
  assert_required(rare_relative_target)
  assert_required(rare_absolute_target)
  assert_required(rare_method)
  assert_required(common_absolute_threshold)
  assert_required(common_absolute_target)
  assert_required(common_method)
  assert_required(cap_absolute_target)
  assert_required(interp_method)
  # return new method
  new_method(
    name = "interpolated absolute targets",
    type = "relative",
    fun = calc_interp_absolute_targets,
    args = list(
      rare_absolute_threshold = rare_absolute_threshold,
      rare_relative_target = rare_relative_target,
      rare_absolute_target = rare_absolute_target,
      rare_method = rare_method,
      common_absolute_threshold = common_absolute_threshold,
      common_relative_target = common_relative_target,
      common_absolute_target = common_absolute_target,
      common_method = common_method,
      cap_absolute_target = cap_absolute_target,
      interp_method = interp_method
    )
  )
}

calc_interp_absolute_targets <- function(x, features,
                                               rare_absolute_threshold,
                                               rare_relative_target,
                                               rare_absolute_target,
                                               rare_method,
                                               common_absolute_threshold,
                                               common_relative_target,
                                               common_absolute_target,
                                               common_method,
                                               cap_absolute_target,
                                               interp_method,
                                               call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call, .internal = TRUE)
  assert_required(features, call = call, .internal = TRUE)
  assert_required(rare_absolute_threshold, call = call, .internal = TRUE)
  assert_required(rare_relative_target, call = call, .internal = TRUE)
  assert_required(rare_absolute_target, call = call, .internal = TRUE)
  assert_required(rare_method, call = call, .internal = TRUE)
  assert_required(common_absolute_threshold, call = call, .internal = TRUE)
  assert_required(common_relative_target, call = call, .internal = TRUE)
  assert_required(common_absolute_target, call = call, .internal = TRUE)
  assert_required(common_method, call = call, .internal = TRUE)
  assert_required(cap_absolute_target, call = call, .internal = TRUE)
  assert_required(interp_method, call = call, .internal = TRUE)
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
    # rare_absolute_threshold
    assertthat::is.number(rare_absolute_threshold),
    all_finite(rare_absolute_threshold),
    rare_absolute_threshold >= 0,
    # common_absolute_threshold
    assertthat::is.number(common_absolute_threshold),
    all_finite(common_absolute_threshold),
    common_absolute_threshold >= 0,
    rare_absolute_threshold <= common_absolute_threshold,
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
    assertthat::is.scalar(rare_absolute_target),
    assertthat::is.scalar(common_absolute_target),
    assertthat::is.scalar(cap_absolute_target),
    call = call
  )
  # assert valid bounds for non-missing values
  if (assertthat::noNA(rare_absolute_target)) {
    assert(
      assertthat::is.number(rare_absolute_target),
      all_finite(rare_absolute_target),
      rare_absolute_target >= 0,
      call = call
    )
  } else {
    ## this is needed to account for different NA classes
    rare_absolute_target <- NA_real_ # nocov
  }
  if (assertthat::noNA(common_absolute_target)) {
    assert(
      assertthat::is.number(common_absolute_target),
      all_finite(common_absolute_target),
      common_absolute_target >= 0,
      call = call
    )
  } else {
    ## this is needed to account for different NA classes
    common_absolute_target <- NA_real_ # nocov
  }
  if (assertthat::noNA(cap_absolute_target)) {
    assert(
      assertthat::is.number(cap_absolute_target),
      all_finite(cap_absolute_target),
      cap_absolute_target >= 0,
      call = call
    )
  } else {
    ## this is needed to account for different NA classes
    common_absolute_target <- NA_real_ # nocov
  }

  # if features have user-defined area units, then throw warning indicating
  # that these targets do not consider the spatial units
  verify(
    inherits(x$data$cost, c("SpatRaster", "Raster")) ||
      all(is.na(x$feature_units())),
    msg = c(
     "!" = "{.arg x} has spatial units defined for the features.",
      "i" = paste(
        "{.fun spec_interp_absolute_targets} does not account",
        "for spatial units."
      ),
      "i" = paste(
        "See {.fun spec_interp_area_targets} to",
        "add targets that account for spatial units."
      )
    ),
    call = NULL
  )

  # calculate targets
  calc_interp_targets(
    x = c(x$feature_abundances_in_total_units()[features, 1]),
    rare_absolute_threshold = rare_absolute_threshold,
    rare_relative_target = rare_relative_target,
    rare_absolute_target = rare_absolute_target,
    rare_method = rare_method,
    common_absolute_threshold = common_absolute_threshold,
    common_relative_target = common_relative_target,
    common_absolute_target = common_absolute_target,
    common_method = common_method,
    cap_absolute_target = cap_absolute_target,
    interp_method = interp_method,
    call = call
  )
}

calc_interp_targets <- function(x,
                                rare_absolute_threshold,
                                rare_relative_target,
                                rare_absolute_target,
                                rare_method,
                                common_absolute_threshold,
                                common_relative_target,
                                common_absolute_target,
                                common_method,
                                cap_absolute_target,
                                interp_method,
                                call = fn_caller_env()) {
  # assert valid arguments
  assert_required(x, call = call, .internal = TRUE)
  assert_required(rare_absolute_threshold, call = call, .internal = TRUE)
  assert_required(rare_relative_target, call = call, .internal = TRUE)
  assert_required(rare_absolute_target, call = call, .internal = TRUE)
  assert_required(rare_method, call = call, .internal = TRUE)
  assert_required(common_absolute_threshold, call = call, .internal = TRUE)
  assert_required(common_relative_target, call = call, .internal = TRUE)
  assert_required(common_absolute_target, call = call, .internal = TRUE)
  assert_required(common_method, call = call, .internal = TRUE)
  assert_required(cap_absolute_target, call = call, .internal = TRUE)
  assert_required(interp_method, call = call, .internal = TRUE)
  assert(
    is.numeric(x),
    assertthat::noNA(x),
    assertthat::is.number(rare_absolute_threshold),
    assertthat::is.number(rare_relative_target),
    assertthat::is.number(rare_absolute_target),
    assertthat::is.number(rare_absolute_target),
    assertthat::is.string(rare_method),
    assertthat::is.number(common_absolute_threshold),
    assertthat::is.number(common_relative_target),
    assertthat::is.number(common_absolute_target),
    assertthat::is.string(common_method),
    assertthat::is.number(cap_absolute_target),
    assertthat::is.string(interp_method),
    call = call,
    .internal = TRUE
  )

  # calculate targets
  if (identical(interp_method, "linear")) {
    targets <- x * linear_interpolation(
      x = x,
      coordinate_one_x = rare_absolute_threshold,
      coordinate_one_y = rare_relative_target,
      coordinate_two_x = common_absolute_threshold,
      coordinate_two_y = common_relative_target
    )
  } else {
    targets <- x * loglinear_interpolation(
      x = x,
      coordinate_one_x = rare_absolute_threshold,
      coordinate_one_y = rare_relative_target,
      coordinate_two_x = common_absolute_threshold,
      coordinate_two_y = common_relative_target
    )
  }

  # apply absolute targets
  if (assertthat::noNA(rare_absolute_target)) {
    idx <- x < rare_absolute_threshold
    if (identical(rare_method, "min")) {
      targets[idx] <- pmin(targets[idx], rare_absolute_target)
    } else {
      targets[idx] <- pmax(targets[idx], rare_absolute_target)
    }
  }
  if (assertthat::noNA(common_absolute_target)) {
    idx <- x > common_absolute_threshold
    if (identical(common_method, "min")) {
      targets[idx] <- pmin(targets[idx], common_absolute_target)
    } else {
      targets[idx] <- pmax(targets[idx], common_absolute_target)
    }
  }

  # apply target cap
  targets <- pmin(targets, cap_absolute_target, na.rm = TRUE)

  # clamp targets to feature abundances
  targets <- pmin(targets, x)

  # return target
  targets / x
}
