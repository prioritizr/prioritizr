#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Add targets following Watson *et al.* (2010)
#'
#' Add targets to a conservation planning problem following
#' Watson *et al.* (2010).
#' Briefly, it involves setting targets thresholds as a percentage based on
#' whether or not a feature would be considered rare.
#' To help prevent widespread features from obscuring priorities,
#' targets are capped following Butchart *et al.* (2015).
#' This method was designed for species protection at national-scales.
#'
#' @inheritParams add_manual_targets
#' @inheritParams watson_targets
#'
#' @inheritSection watson_targets Mathematical formulation
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit watson_targets details references
#' @inherit add_relative_targets return seealso
#'
#' @family targets
#'
#' @examples
#' #TODO
#'
#' @name add_watson_targets
NULL

#' @rdname add_watson_targets
#' @export
add_watson_targets <- function(x, rare_area_threshold = 10000,
                               rare_relative_target = 1,
                               rare_area_target = 1000,
                               common_relative_target = 0.1,
                               cap_area_target = 1000000,
                               area_units = "km^2") {
  # add targets
  internal_add_auto_targets.Method(
    x,
    method = internal_watson_targets(
      rare_area_threshold = rare_area_threshold,
      rare_relative_target = rare_relative_target,
      rare_area_target = rare_area_target,
      common_relative_target = common_relative_target,
      cap_area_target = cap_area_target,
      area_units = area_units
    )
  )
}
