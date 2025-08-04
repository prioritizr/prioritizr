#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Add targets based on interpolating absolute thresholds
#'
#' Add targets to a conservation planning problem by interpolating
#' them between thresholds based on the units of the features.
#' Briefly, this method involves
#' (i) setting target thresholds for rare features to a particular percentage
#' threshold, (ii) setting target thresholds for common features
#' to a particular percentage threshold, and (iii) interpolating
#' target thresholds for features with spatial distributions that
#' range between the those for the rare and common features.
#' Additionally, features can (optionally) have their targets capped at a
#' particular threshold.
#' This method is especially useful for setting targets based on
#' interpolation procedures when features do not have data in an area-based
#' unit of measurement.
#'
#' @inheritParams add_manual_targets
#' @inheritParams interpolated_absolute_targets
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit interpolated_absolute_targets details references
#' @inherit add_relative_targets return seealso
#'
#' @family targets
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#'
#' @name add_interpolated_absolute_targets
#'
#' @docType methods
NULL

#' @rdname add_interpolated_absolute_targets
#' @export
add_interpolated_absolute_targets <- function(x, rare_absolute_threshold,
                                              rare_relative_target,
                                              rare_absolute_target,
                                              rare_method,
                                              common_absolute_threshold,
                                              common_relative_target,
                                              common_absolute_target,
                                              common_method,
                                              cap_absolute_target,
                                              interp_method) {
  # add targets
  internal_add_auto_targets.Method(
    x,
    method = interpolated_absolute_targets(
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
