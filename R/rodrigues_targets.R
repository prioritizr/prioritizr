#' @include internal.R ConservationProblem-class.R loglinear_interpolation.R
NULL

#' Specify targets following Rodrigues *et al.* (2004)
#'
#' Specify targets following Rodrigues *et al.* (2004).
#' This function is designed to be used within `add_auto_targets()` and
#' `add_group_targets()`.
#'
#' @param rare_threshold `numeric` value indicating the threshold area for rare
#' identifying rare features.
#' Defaults to 1000 (i.e., 1000 km^2).
#'
#' @param rare_relative_target `numeric` value indicating the
#' relative target for features with a spatial distribution
#' that is smaller than `rare_threshold`.
#' Note that this value must be a proportion between 0 and 1.
#' Defaults to 1 (i.e., 100%).
#'
#' @param common_threshold `numeric` value indicating the threshold area
#' for identifying common features.
#' Defaults to 250000
#' (i.e., 250,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @param common_relative_target `numeric` value indicating the
#' relative target for features with a spatial distribution
#' that is greater than `common_threshold`.
#' Defaults to 0.1 (i.e., 10%).
#' Since this default value is based on historical levels of global protected
#' area coverage, it may be appropriate to set this value based on current
#' levels of protected area coverage (e.g., 17.6% for terrestrial and 8.4% for
#' marine systems globally; UNEP-WCMC and IUCN 2025).
#'
#' @param cap_threshold `numeric` value indicating the threshold
#' area for applying a target cap.
#' To avoid setting a target cap, a missing (`NA`) value can be specified.
#' Defaults to 1000000
#' (i.e., 1,000,000 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @section Mathematical formulation:
#' This method involves setting target thresholds based on the spatial
#' extent of the features.
#' By default, this method identifies rare features as those with a
#' spatial distribution smaller than 1,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} (per `rare_threshold`)
#' and common features as those with a spatial distribution
#' larger than 250,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} (per `common_threshold`).
#' Given this, rare features are assigned a target threshold
#' of 100% (per `rare_relative_target`), common features
#' are assigned a target threshold of 10% (per `common_relative_target`),
#' and features with a spatial distribution that is between
#' the area-based thresholds used to identify rare and common features are
#' assigned a target threshold through log-linear interpolation.
#' Additionally, following Butchart *et al.* (2015), a cap of 1,000,000
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} is applied to target
#' thresholds.
#'
#' @details
#' This target setting method was designed to protect species in global-scale
#' prioritizations (Rodrigues *et al.* 2004).
#' Although it has also been successfully applied to global-scales
#' (e.g., Butchart *et al.* 2015; Hanson *et al.* 2020; Venter *et al.* 2014),
#' it may fail to identify meaningful priorities for
#' prioritizations conducted at smaller geographic scales
#' (e.g., national, state-level or county scales).
#' For example, if this method is applied to
#' such geographic scales, then the resulting prioritizations
#' may select an overly large percentage of the study area,
#' or be biased towards over-representing common and widespread species.
#' This is because the thresholds
#' (i.e., `rare_threshold`, `common_threshold`, and `cap_threshold`)
#' were originally developed based on criteria for promoting the long-term
#' persistence of entire species.
#' As such, if you are working at a sub-global scale, it is recommended to set
#' thresholds based on that criteria are appropriate to the spatial extent
#' of the planning region.
#' Please note that this function is provided as convenient method to
#' set targets for problems with a single management zone, and cannot
#' be used for those with multiple management zones.
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
#' @inherit jung_targets return seealso
#'
#' @family method
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
#' Hanson JO, Rhodes JR, Butchart SHM, Buchanan GM, Rondinini C, Ficetola GF,
#' Fuller RA (2020) Global conservation of species’ niches. Global conservation
#' of species' niches. *Nature*, 580: 232--234
#'
#' Rodrigues ASL, Akçakaya HR, Andelman SJ, Bakarr MI, Boitani L, Brooks TM,
#' Chanson JS, Fishpool LDC, Da Fonseca GAB, Gaston KJ, Hoffmann M, Marquet PA,
#' Pilgrim JD, Pressey RL, Schipper J, Sechrest W, Stuart SN, Underhill LG,
#' Waller RW, Watts MEJ, Yan X (2004)
#' Global gap analysis: priority regions for expanding the global
#' protected-area network. *BioScience*, 54: 1092--1100.
#'
#' UNEP-WCMC and IUCN (2025) Protected Planet Report 2024.
#' Cambridge, UK: UNEP-WCMC and IUCN. Available at <www.protectedplanet.net>.
#'
#' Venter O, Fuller RA, Segan DB, Carwardine J, Brooks T, Butchart SHM,
#' Di Marco M, Iwamura T, Joseph L, O'Grady D, Possingham HP, Rondinini C,
#' Smith RJ, Venter M, Watson JEM (2014) Targeting global protected area
#' expansion for imperiled biodiversity. *PLoS Biology*, 12: e1001891.
#'
#' @examples
#' #TODO
#'
#' @name rodrigues_targets
NULL

#' @rdname rodrigues_targets
#' @export
rodrigues_targets <- function(x, rare_threshold = 1000,
                             rare_relative_target = 1,
                             common_threshold = 250000,
                             common_relative_target = 0.1,
                             cap_threshold = 1000000) {
  # return method
  new_method(
    name = "Rodrigues et al. (2004) targets",
    type = "absolute",
    fun = internal_interpolated_targets,
    args = list(
      rare_threshold = rare_threshold,
      rare_relative_target = rare_relative_target,
      rare_absolute_target = NA_real_,
      rare_method = "max", # has no effect
      common_threshold = common_threshold,
      common_relative_target = common_relative_target,
      common_absolute_target = NA_real_,
      common_method = "max", # has no effect
      cap_threshold = cap_threshold,
      method = "log10"
    )
  )
}
