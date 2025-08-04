#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Add targets following a set of rules
#'
#' Add targets to a conservation planning problem following a rule-based
#' procedure based on a set of ecological and ecosystem criteria. This is a
#' customizable version of the approach in Harris and Holness (2023).
#' To help prevent widespread features from obscuring priorities,
#' targets are capped following Butchart *et al.* (2015).
#' This method was designed to help set targets for a broad range of features
#' (e.g., species, ecosystems, ecosystem services, ecological processes)
#' at local and national scales.
#'
#' @inheritParams rule_targets
#' @inheritParams add_manual_targets
#'
#' @inheritSection rule_targets Mathematical formulation
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit rule_targets details references
#' @inherit add_relative_targets return seealso
#'
#' @family targets
#'
#' @examples
#' #TODO
#'
#' @name add_rule_targets
NULL

#' @rdname add_rule_targets
#' @export
add_rule_targets <- function(x, baseline_relative_target,
                            rules_relative_target, data,
                            cap_area_target = 1000000,
                            area_units = "km^2") {
  # add targets
  internal_add_auto_targets.Method(
    x,
    method = rule_targets(
      baseline_relative_target = baseline_relative_target,
      rules_relative_target = rules_relative_target,
      data = data,
      cap_area_target = cap_area_target,
      area_units = area_units
    )
  )
}
