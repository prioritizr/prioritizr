#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Add targets following Rodrigues *et al.* (2004)
#'
#' Add targets to a conservation planning problem following
#' Rodrigues *et al.* (2004).
#' Briefly, this method involves setting targets based on log-linear
#' interpolation methods.
#' To help prevent widespread features from obscuring priorities,
#' targets are capped following Butchart *et al.* (2015).
#' This method was designed for global-scale prioritizations.
#'
#' @inheritParams add_manual_targets
#' @inheritParams rodrigues_targets
#'
#' @inheritSection rodrigues_targets Mathematical formulation
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit rodrigues_targets details references
#' @inherit add_relative_targets return seealso
#'
#' @family targets
#"
#' @examples
#' #TODO
#'
#' @name add_rodrigues_targets
NULL

#' @rdname add_rodrigues_targets
#' @export
add_rodrigues_targets <- function(x, rare_threshold = 1000,
                                  rare_relative_target = 1,
                                  common_threshold = 250000,
                                  common_relative_target = 0.1,
                                  cap_threshold = 1000000) {
  # add targets
  internal_add_auto_targets.Method(
    x,
    method = rodrigues_targets(
      rare_threshold = rare_threshold,
      rare_relative_target = rare_relative_target,
      common_threshold = common_threshold,
      common_relative_target = common_relative_target,
      cap_threshold = cap_threshold
    )
  )
}
