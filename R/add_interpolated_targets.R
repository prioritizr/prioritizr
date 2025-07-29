#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Add targets based on interpolation
#'
#' Add targets to a conservation planning problem by interpolating
#' them between area-based thresholds. Briefly, this method involves
#' (i) setting target thresholds for rare features to a particular percentage
#' threshold, (ii) setting target thresholds for common features
#' to a particular percentage threshold, and (iii) interpolating
#' target thresholds for features with spatial distributions that
#' range between the those for the rare and common features.
#' Additionally, features with a widespread spatial distribution can
#' (optionally) have their targets capped at a particular threshold.
#'
#' @inheritParams add_manual_targets
#' @inheritParams interpolated_targets
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit interpolated_targets details references
#' @inherit add_relative_targets return seealso
#'
#' @family targets
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#'
#' @name add_interpolated_targets
#'
#' @docType methods
NULL

#' @rdname add_interpolated_targets
#' @export
add_interpolated_targets <- function(x, rare_threshold,
                                     rare_relative_target,
                                     rare_absolute_target,
                                     rare_method,
                                     common_threshold,
                                     common_relative_target,
                                     common_absolute_target,
                                     common_method,
                                     cap_threshold,
                                     interp_method) {
  # calculate targets
  targets <- internal_add_interpolated_targets(
    x = x,
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

  # add targets to problem
  add_manual_targets(
    x,
    tibble::tibble(
      feature = x$feature_names(),
      target = targets,
      type = "absolute"
    )
  )

}

internal_add_interpolated_targets <- function(x, rare_threshold,
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
