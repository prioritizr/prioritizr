#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Add targets based on interpolating area-based thresholds
#'
#' Add targets to a conservation planning problem by interpolating
#' them between thresholds based on area-based units.
#' Briefly, this method involves
#' (i) setting target thresholds for rare features to a particular percentage
#' threshold, (ii) setting target thresholds for common features
#' to a particular percentage threshold, and (iii) interpolating
#' target thresholds for features with spatial distributions that
#' range between the those for the rare and common features.
#' Additionally, features can (optionally) have their targets capped at a
#' particular threshold.
#' This method is especially useful for setting targets based on
#' interpolation procedures when features have data in an area-based
#' unit of measurement (e.g., \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @inheritParams add_manual_targets
#' @inheritParams interpolated_area_targets
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit interpolated_area_targets details references
#' @inherit add_relative_targets return seealso
#'
#' @family targets
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#'
#' @name add_interpolated_area_targets
#'
#' @docType methods
NULL

#' @rdname add_interpolated_area_targets
#' @export
add_interpolated_area_targets <- function(x, rare_area_threshold,
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
  # add targets
  internal_add_auto_targets.Method(
    x,
    method = internal_interpolated_area_targets(
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
