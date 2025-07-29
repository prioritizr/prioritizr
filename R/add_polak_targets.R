#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Add targets following Polak *et al.* (2015)
#'
#' Add targets to a conservation planning problem following Polak *et al.*
#' (2015).
#' Briefly, this method involves setting targets based on linear
#' interpolation methods.
#' To help prevent widespread features from obscuring priorities,
#' targets are capped following Butchart *et al.* (2015).
#' This method was designed for species protection at national-scales.
#'
#' @inheritParams add_manual_targets
#' @inheritParams polak_targets
#'
#' @inheritSection polak_targets Mathematical formulation
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit polak_targets details references
#' @inherit add_relative_targets return seealso
#'
#' @family targets
#'
#' @examples
#' #TODO
#'
#' @name add_polak_targets
NULL

#' @rdname add_polak_targets
#' @export
add_polak_targets <- function(x, rare_threshold = 1000,
                              rare_relative_target = 1,
                              common_threshold = 10000,
                              common_relative_target = 0.1,
                              cap_threshold = 1000000) {
  # add targets
  internal_add_auto_targets.Method(
    x,
    method = polak_targets(
      rare_threshold = rare_threshold,
      rare_relative_target = rare_relative_target,
      common_threshold = common_threshold,
      common_relative_target = common_relative_target,
      cap_threshold = cap_threshold
    )
  )
}
