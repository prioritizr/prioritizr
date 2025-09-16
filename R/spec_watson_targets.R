#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets following Watson *et al.* (2010)
#'
#' Specify targets for a conservation planning problem following
#' Watson *et al.* (2010).
#' Briefly, it involves setting targets thresholds as a percentage based on
#' whether or not a feature would be considered rare.
#' To help prevent widespread features from obscuring priorities,
#' targets are capped following Butchart *et al.* (2015).
#' This method was designed for species protection at national-scales.
#' Note that this function is designed to be used within [add_auto_targets()]
#' and [add_group_targets()].
#'
#' @param rare_area_threshold `numeric` value indicating the threshold
#' area for rare identifying rare features.
#' Defaults to 10000 (i.e., 10000 km^2).
#'
#' @param rare_area_target `numeric` value indicating the
#' area-based target for features with a spatial distribution
#' that is smaller than `rare_threshold`.
#' Defaults to 1000 (i.e., 1000 km^2).
#'
#' @inheritParams spec_rodrigues_targets
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on the spatial
#' extent of the features.
#' By default, this method identifies rare features as those with a
#' spatial distribution smaller than 10,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} (per `rare_threshold`)
#' and common features as those with a larger spatial distribution.
#' Given this, rare features are assigned target threshold of 100%
#' (per `rare_relative_target`)
#' or 1,000 km\ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}
#' (per `rare_absolute_target`)
#' (whichever of these two values is smaller), and
#' common features are assigned a target threshold of 10%
#' (per `common_relative_target`).
#' Additionally, following Butchart *et al.* (2015), a cap of 1,000,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} is applied to target
#' thresholds.
#'
#' @details
#' This target setting method was designed to protect species in
#' a national-scale prioritization (Watson *et al.* 2010).
#' Although similar methods have also been successfully to national-scale
#' prioritizations (e.g., Kark *et al.* 2009),
#' it may fail to identify meaningful priorities for
#' prioritizations conducted at smaller geographic scales
#' (e.g., local or county-level scales).
#' For example, if this target setting method is applied to
#' such geographic scales, then the resulting prioritizations
#' may select an overly large percentage of the study area,
#' or be biased towards over-representing common and widespread species.
#' This is because the thresholds
#' (i.e., `rare_threshold` and `cap_threshold`)
#' were originally developed based on criteria for national-scales.
#' As such, if you working at the sub-national scale, it is recommended to set
#' thresholds based on that criteria are appropriate to the spatial extent
#' of the planning region.
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#' @inherit spec_jung_targets return seealso
#'
#' @family methods
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
#' Carwardine J, Klein CJ, Wilson KA, Pressey RL, Possingham HP (2009) Hitting
#' the target and missing the point: target‐based conservation planning in
#' context. *Conservation Letters*, 2: 4--11.
#'
#' Kark S, Levin N, Grantham HS, Possingham HP (2009) Between-country
#' collaboration and consideration of costs increase conservation planning
#' efficiency in the Mediterranean Basin.
#' *Proceedings of the National Academy of Sciences*, 106: 15368--15373.
#'
#' UNEP-WCMC and IUCN (2025) Protected Planet Report 2024.
#' Cambridge, UK: UNEP-WCMC and IUCN. Available at <www.protectedplanet.net>.
#'
#' Watson JEM, Evans MC, Carwardine J, Fuller RA, Joseph LN, Segan DB,
#' Taylor MFJ, Fensham RJ, Possingham HP (2010) The capacity of Australia's
#' protected-area system to represent threatened species.
#' *Conservation Biology*,25: 324--332.
#'
#' @examples
#' #TODO
#'
#' @export
spec_watson_targets <- function(rare_area_threshold = 10000,
                                rare_relative_target = 1,
                                rare_area_target = 1000,
                                common_relative_target = 0.1,
                                cap_area_target = 1000000,
                                area_units = "km^2") {
   # assert arguments are valid
  assert_valid_method_arg(rare_area_threshold)
  assert_required(rare_area_threshold)
  assert_required(rare_relative_target)
  assert_required(rare_area_target)
  assert_required(common_relative_target)
  assert_required(cap_area_target)
  assert_required(area_units)
  # return new method
  new_method(
    name = "Watson et al. (2010) targets",
    type = "relative",
    fun = calc_interp_area_targets,
    args = list(
      rare_area_threshold = rare_area_threshold,
      rare_relative_target = rare_relative_target,
      rare_area_target = rare_area_target,
      rare_method = "min",
      common_area_threshold = rare_area_threshold,
      common_relative_target = common_relative_target,
      common_area_target = NA_real_,
      common_method = "max", # has no effect
      cap_area_target = cap_area_target,
      interp_method = "linear", # has no effect
      area_units = area_units
    )
  )
}
