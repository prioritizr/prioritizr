#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets based on interpolation
#'
#' Specify targets by interpolating them between area-based thresholds.
#' This function is designed to be used within `add_auto_targets()`.
#'
#' @param rare_threshold `numeric` value indicating the threshold area
#' for identifying rare features.
#' This value must be expressed as
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' For example, a value of 5 is corresponds to
#' 5 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#'
#' @param rare_relative_target `numeric` value indicating the
#' relative target for features with a spatial distribution
#' that is smaller than `rare_threshold`.
#' Note that this value must be a proportion between 0 and 1.
#' For example, a value of 0.1 corresponds to 10%.
#'
#' @param rare_absolute_target `numeric` value indicating the
#' absolute target for features with a spatial distribution
#' that is smaller than `rare_threshold`.
#' This value must be expressed as
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' For example, a value of 10 corresponds to
#' 10 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' To avoid setting an absolute target for rare features,
#' a missing (`NA`) value can be specified.
#'
#' @param rare_method `character` value indicating how the target for rare
#' features should be calculated. Available options include `"min"` and `"max"`.
#' For example, a value of `"max"` means that the target for a rare features
#' is calculated as the maximum based on `rare_relative_target` and
#' `rare_absolute_target`.
#'
#' @param common_threshold `numeric` value indicating the threshold area
#' for identifying common features.
#' This value must be expressed as
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' For example, a value of 10 is corresponds to
#' 10 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#'
#' @param common_relative_target `numeric` value indicating the
#' relative target for features with a spatial distribution
#' that is greater than `common_threshold`.
#' Note that this value must be a proportion between 0 and 1.
#' For example, a value of 0.1 corresponds to 10%.
#'
#' @param common_absolute_target `numeric` value indicating the
#' absolute target for features with a spatial distribution
#' that is greater than `common_threshold`.
#' This value must be expressed as
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' For example, a value of 10 is corresponds to
#' 10 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' To avoid setting an absolute target for common features,
#' a missing (`NA`) value can be specified.
#'
#' @param common_method `character` value indicating how the target for common
#' features should be calculated. Available options include `"min"` and `"max"`.
#' For example, a value of `"max"` means that the target for a common feature
#' is calculated as the maximum based on `common_relative_target` and
#' `common_absolute_target`.
#'
#' @param cap_threshold `numeric` value indicating the threshold
#' area for applying a target cap.
#' This value must be expressed as
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' For example, a value of 10 is corresponds to
#' 10 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' To avoid setting an absolute target for common features,
#' a missing (`NA`) value can be specified.
#'
#' @param interp_method `character` value denoting the interpolation method
#' that should be applied. Available options include `"linear"` and
#' `"log10"` for log-linear interpolation.
#'
#' @section Mathematical formulation:
#' This method provides a flexible approach for setting target thresholds based
#' on the spatial extent of the features.
#' To express this mathematically, we will define the following terminology.
#' Let \eqn{f} denote the total abundance of a feature (e.g., geographic
#' range size),
#' \eqn{a} the threshold area for identifying rare features
#' (per `rare_threshold`),
#' \eqn{b} the relative targets for rare features
#' (per `rare_relative_target`),
#' \eqn{c} the absolute targets for rare features
#' (per `rare_absolute_target`),
#' \eqn{d()} the function for calculating targets for rare features
#' as a maximum or minimum value (per `rare_method`).
#' \eqn{e} the threshold area for identifying common features
#' (per `common_threshold`),
#' \eqn{g} the relative targets for common features
#' (per `common_relative_target`),
#' \eqn{h} the absolute targets for common features
#' (per `common_absolute_target`),
#' \eqn{i()} the method for calculating targets for common features
#' as a maximum or minimum value  (per `common_method`).
#' \eqn{j} the cap threshold (per `cap_threshold`).
#' \eqn{k()} the interpolation method for features with a spatial distribution
#' that is larger than a rare features and smaller than a common feature
#' (per `interp_method`).
#' In particular, \eqn{j()} is either a linear or log-linear interpolation
#' procedure based on the thresholds for identifying rare and common features
#' as well as the relative targets for rare and common features.
#' Given this terminology, the target threshold (\eqn{t}) for a feature
#' is calculated as follows.
#' * If \eqn{f < a}, then \eqn{t = min(d(c, b * f), j)}.
#' * If \eqn{f > e}, then \eqn{t = min(i(h, g * f), j)}.
#' * If \eqn{a <= f <= e}, then \eqn{t = min(k(f, a, b, e, g), j)}.
#'
#' @details
#' This method has been applied to set target thresholds at global and national
#' scales (e.g., Butchart *et al.* 2015;
#' Rodrigues *et al.* 2004; Polak *et al.*2015).
#' It is based on the rationale that species with a smaller geographic
#' distribution are at a greater risk of extinction, and so require
#' a larger percentage of their geographic distribution to be represented
#' by a prioritization (Rodrigues *et al.* 2004).
#' When using this method in a planning exercise, it is important to ensure
#' that the threshold parameters reflect the stakeholder objectives.
#' Additionally, the threshold parameters may need to calibrated based on
#' the spatial extent of the planning region.
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit jung_targets return seealso
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
#' # TODO
#' }
#'
#' @name interpolated_targets
NULL

#' @rdname interpolated_targets
#' @export
interpolated_targets <- function(x, rare_threshold,
                                 rare_relative_target,
                                 rare_absolute_target,
                                 rare_method,
                                 common_threshold,
                                 common_relative_target,
                                 common_absolute_target,
                                 common_method,
                                 cap_threshold,
                                 interp_method) {
  # return method
  new_method(
    name = "interpolated targets",
    type = "absolute",
    fun = internal_interpolated_targets,
    args = list(
      rare_threshold = rare_threshold,
      rare_absolute_target = rare_absolute_target,
      rare_method = rare_method,
      common_threshold = common_threshold,
      common_relative_target = common_relative_target,
      common_absolute_target = common_absolute_target,
      common_method = common_method,
      cap_threshold = cap_threshold,
      interp_method = interp_method
    )
  )
}

internal_interpolated_targets <- function(x, rare_threshold,
                                          rare_relative_target,
                                          rare_absolute_target,
                                          rare_method,
                                          common_threshold,
                                          common_relative_target,
                                          common_absolute_target,
                                          common_method,
                                          cap_threshold,
                                          interp_method,
                                          call = fn_caller_env()) {
  # assert that arguments are valid
  assert_required(x, call = call)
  assert_required(rare_threshold, call = call)
  assert_required(rare_relative_target, call = call)
  assert_required(rare_method, call = call)
  assert_required(common_threshold, call = call)
  assert_required(common_relative_target, call = call)
  assert_required(common_method, call = call)
  assert_required(cap_threshold, call = call)
  assert_required(method, call = call)
  assert(
    # x
    is_conservation_problem(x),
    has_single_zone(x),
    # rare_threshold
    assertthat::is.number(rare_threshold),
    all_finite(rare_threshold),
    rare_threshold >= 0,
    # common_threshold
    assertthat::is.number(common_threshold),
    all_finite(common_threshold),
    common_threshold >= 0,
    rare_threshold <= common_threshold,
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
    assertthat::noNA(rare_relative_target),
    rare_relative_target >= 0,
    rare_relative_target <= 1,
    assertthat::is.number(common_relative_target),
    assertthat::noNA(common_relative_target),
    common_relative_target >= 0,
    common_relative_target <= 1,
    assertthat::is.scalar(rare_absolute_target),
    assertthat::is.scalar(common_absolute_target),
    assertthat::is.scalar(cap_threshold),
    call = call
  )

  # assert valid bounds for non-missing values
  if (assertthat::noNA(rare_absolute_target)) {
    assert(
      assertthat::is.number(rare_absolute_target),
      rare_absolute_target >= 0,
      call = call
    )
  }
  if (assertthat::noNA(common_absolute_target)) {
    assert(
      assertthat::is.number(common_absolute_target),
      common_absolute_target >= 0,
      call = call
    )
  }
  if (assertthat::noNA(cap_threshold)) {
    assert(
      assertthat::is.number(cap_threshold),
      cap_threshold >= 0,
      call = call
    )
  }

  # assert x has a single zone
  assert(
    isTRUE(number$number_of_zones() == 1),
    msg = c(
      "!" = "Can't calculate targets.",
      "x" = "{.arg x} must have a single management zone for this function."
    ),
    call = call
  )

  # extract abundances
  fa <- x$feature_abundances_in_total_units()

  # extract feature data resolution in square meters
  fr <- x$feature_resolution_m2()

  # if possible, convert to 1 km^2
  if (is.numeric(fr)) {
    fa <- c(fa) * (fr / (1000 * 1000))
  } else {
    cli::cli_bullets(
      c(
        ">" = paste(
          "Targets will be calculated assuming",
          "feature data are in units of 1 km^2."
        ),
        "i" = paste(
          "This is because the {.arg features} in {.arg x}",
          "are not rasters."
        )
      )
    )
  }

  # calculate targets
  if (identical(method, "linear")) {
    targets <- fa * linear_interpolation(
      fa,
      rare_threshold,
      rare_relative_target,
      common_threshold,
      common_relative_target
    )
  } else {
    targets <- fa * loglinear_interpolation(
      fa,
      rare_threshold,
      rare_relative_target,
      common_threshold,
      common_relative_target
    )
  }

  # apply absolute targets
  if (assertthat::noNA(rare_absolute_target)) {
    idx <- fa < rare_threshold
    if (identical(rare_method, "min")) {
      targets[idx] <- pmin(targets[idx], rare_absolute_target)
    } else {
      targets[idx] <- pmax(targets[idx], rare_absolute_target)
    }
  }
  if (assertthat::noNA(common_absolute_target)) {
    idx <- fa > common_threshold
    if (identical(common_method, "min")) {
      targets[idx] <- pmin(targets[idx], common_absolute_target)
    } else {
      targets[idx] <- pmax(targets[idx], common_absolute_target)
    }
  }

  # apply target cap
  if (assertthat::noNA(cap_threshold)) {
    targets <- pmin(targets, cap_threshold)
  }

  # return targets
  targets
}
