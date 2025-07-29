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
add_watson_targets <- function(x, rare_threshold = 10000,
                               rare_relative_target = 1,
                               rare_absolute_target = 1000,
                               common_relative_target = 0.1,
                               cap_threshold = 1000000) {
  # add targets
  internal_add_auto_targets.Method(
    x,
    method = watson_targets(
      rare_threshold = rare_threshold,
      rare_relative_target = rare_relative_target,
      rare_absolute_target = rare_absolute_target,
      common_relative_target = common_relative_target,
      cap_threshold = cap_threshold
    )
  )
}
